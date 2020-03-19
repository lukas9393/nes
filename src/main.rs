mod cpu;
mod cpu_mode;
mod cpu_opcode;
mod ppu;

use crate::cpu::{cpu_step, init_cpu, read};
use crate::ppu::{init_ppu, ppu_step};
use sdl2::event::Event;
use sdl2::keyboard::{Keycode, Scancode};
use sdl2::pixels::{Color, PixelFormatEnum};
use sdl2::render::{Canvas, TextureCreator};
use sdl2::video::{Window, WindowContext};
use sdl2::EventPump;
use std::collections::HashSet;
use std::fs::File;
use std::io::prelude::*;
use std::time::{Duration, Instant};
use std::{env, process};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("usage: nes PATH");
        process::exit(1);
    }
    let path = &args[1];

    match execute(path) {
        Ok(_) => (),
        Err(error) => panic!("Problem with the emulator: {:?}", error),
    }
}

fn execute(path: &str) -> Result<(), std::io::Error> {
    let (mut canvas, texture_creator, mut event_pump) = match init_window() {
        Ok(t) => t,
        Err(error) => panic!(error),
    };
    let mut n_rom = read_cartridge(path)?;
    let mut mem: Vec<u8> = vec![0; 0x0800]; // 2 KB
    let mut ppu = init_ppu();
    let mut cpu = init_cpu(&mut ppu, &n_rom, &mem);

    let mut texture = texture_creator
        .create_texture_streaming(PixelFormatEnum::RGB24, 256, 240)
        .unwrap();

    let mut timer = Instant::now();
    let mut clock = 0 as usize;
    'running: loop {
        let old_clock = clock;

        if ppu.dma_transfer {
            if ppu.dma_dummy {
                if clock % 2 == 1 {
                    ppu.dma_dummy = false;
                }
            } else if clock % 2 == 0 {
                let address = ((ppu.dma_page as u16) << 8) | ppu.dma_addr as u16;
                ppu.dma_data = read(&mut cpu, &mut ppu, &n_rom, &mem, address);
            } else {
                ppu.primary_oam[ppu.dma_addr as usize] = ppu.dma_data;
                ppu.dma_addr = ppu.dma_addr.wrapping_add(1);
                if ppu.dma_addr == 0x00 {
                    ppu.dma_transfer = false;
                    ppu.dma_dummy = true;
                }
            }
            clock += 1;
        } else {
            clock += cpu_step(&mut n_rom, &mut ppu, &mut cpu, &mut mem);
        }

        for _ in 0..(clock - old_clock) * 3 {
            ppu_step(&mut cpu, &mut ppu, &n_rom);

            if ppu.frame_complete {
                match texture.update(None, &ppu.buffer, 256 * 3) {
                    Ok(_) => (),
                    Err(e) => panic!(e),
                };
                match canvas.copy(&texture, None, None) {
                    Ok(_) => (),
                    Err(e) => panic!(e),
                };
                ppu.frame_complete = false;
                ppu.buffer = vec![0; 240 * 256 * 3];
                canvas.present();

                let pressed_keys: HashSet<Scancode> =
                    event_pump.keyboard_state().pressed_scancodes().collect();
                cpu.controller[0] = 0x00;
                for key in pressed_keys.iter() {
                    cpu.controller[0] |= match key {
                        Scancode::Right => 1,       // A
                        Scancode::Left => 1 << 1,   // B
                        Scancode::Down => 1 << 2,   // Select
                        Scancode::Up => 1 << 3,     // Start
                        Scancode::Return => 1 << 4, // Up
                        Scancode::RShift => 1 << 5, // Down
                        Scancode::F => 1 << 6,      // Left
                        Scancode::D => 1 << 7,      // Right
                        _ => 0,
                    }
                }

                for event in event_pump.poll_iter() {
                    match event {
                        Event::Quit { .. }
                        | Event::KeyDown {
                            keycode: Some(Keycode::Escape),
                            ..
                        } => {
                            break 'running;
                        }
                        _ => {}
                    }
                }

                let now = Instant::now();
                if now < timer + Duration::from_millis(1000 / 60) {
                    std::thread::sleep(timer + Duration::from_millis(1000 / 60) - now);
                }
                timer = Instant::now();
            }
        }
    }

    Ok(())
}

fn init_window() -> Result<(Canvas<Window>, TextureCreator<WindowContext>, EventPump), String> {
    let sdl_context = sdl2::init()?;
    let video_subsystem = sdl_context.video()?;
    let window = video_subsystem
        .window("NES Emulator", 256, 240)
        .position_centered()
        .opengl()
        .build()
        .map_err(|e| e.to_string())?;
    let mut canvas = window.into_canvas().build().map_err(|e| e.to_string())?;
    let texture_creator = canvas.texture_creator();
    canvas.set_draw_color(Color::RGB(0, 0, 0));
    canvas.clear();
    canvas.present();
    let event_pump = sdl_context.event_pump()?;

    Ok((canvas, texture_creator, event_pump))
}

pub struct NROM {
    prg_rom: Vec<Vec<u8>>,
    prg_rom_size: usize,
    chr_rom: Vec<Vec<u8>>,
    chr_ram: Vec<u8>,
    chr_rom_size: usize,
    flags_6: u8,
}

fn read_cartridge(path: &str) -> Result<NROM, std::io::Error> {
    let mut file = File::open(path)?;
    let mut data = Vec::new();
    file.read_to_end(&mut data)?;

    let prg_rom_size = data[4] as usize;
    let prg_chunk_size = 0x4000;
    let mut prg_offset = 0x10;
    if data[6] & 0b0000_0100 != 0 {
        prg_offset += 0x200
    };
    let prg_rom = fill(prg_rom_size, prg_offset, prg_chunk_size, &data);

    let chr_rom_size = data[5] as usize;
    let chr_offset = prg_rom_size * prg_chunk_size + prg_offset;
    let chr_chunk_size = 0x2000;
    let chr_rom = fill(chr_rom_size, chr_offset, chr_chunk_size, &data);

    let flags_6 = data[6];
    let n_rom = NROM {
        prg_rom,
        prg_rom_size,
        chr_rom,
        chr_ram: vec![0; 0x2000],
        chr_rom_size,
        flags_6,
    };

    Ok(n_rom)
}

fn fill(size: usize, offset: usize, chunk_size: usize, data: &[u8]) -> Vec<Vec<u8>> {
    let mut rom = Vec::new();
    for i in 0..size {
        let start = offset + (i * chunk_size);
        let chunk = data[start..(start + chunk_size)].to_vec();
        rom.push(chunk);
    }

    rom
}
