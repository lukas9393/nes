use crate::cpu_mode::*;
use crate::cpu_opcode::*;
use crate::ppu::{read_ppu_reg, write_ppu_reg, PPU};
use crate::NROM;

const RESET_VECTOR: u16 = 0xFFFC;
const NMI_VECTOR: u16 = 0xFFFA;

pub struct CPU {
    // program counter	(16 bit)
    pub(crate) pc: u16,
    // accumulator	(8 bit)
    pub(crate) ac: u8,
    // X register	(8 bit)
    pub(crate) x: u8,
    // Y register	(8 bit)
    pub(crate) y: u8,
    // status register [NV-BDIZC]	(8 bit)
    pub(crate) sr: u8,
    // stack pointer	(8 bit)
    pub(crate) sp: u8,

    pub(crate) nmi: bool,

    pub controller_state: [u8; 2],
    pub controller: [u8; 2],
}

pub fn cpu_step(n_rom: &mut NROM, ppu: &mut PPU, cpu: &mut CPU, mem: &mut Vec<u8>) -> usize {
    if cpu.nmi {
        return nmi(cpu, n_rom, ppu, mem);
    }

    let op = read(cpu, ppu, n_rom, mem, cpu.pc);
    cpu.pc += 1;

    match op {
        0x20 => {
            let (address, cycles) = abs(cpu, ppu, n_rom, mem);
            cycles + jsr(cpu, address, ppu, n_rom, mem)
        }
        0x6C => {
            let (address, cycles) = ind(cpu, ppu, n_rom, mem);
            cycles + jmp(cpu, address)
        }
        0x96 | 0x97 | 0xB6 | 0xB7 => {
            let (address, cycles) = zpg_y(cpu, ppu, n_rom, mem);
            cycles
                + match op {
                    0x96 => stx(cpu, address, ppu, n_rom, mem),
                    0x97 => sax(cpu, address, ppu, n_rom, mem),
                    0xB6 => ldx(cpu, address, ppu, n_rom, mem),
                    0xB7 => lax(cpu, address, ppu, n_rom, mem),
                    _ => 0,
                }
        }
        0xA0 | 0xA2 | 0xC0 | 0xE0 | 0xEB => {
            let (address, cycles) = imm_rel(cpu);
            cycles
                + match op {
                    0xA0 => ldy(cpu, address, ppu, n_rom, mem),
                    0xC0 => cpy(cpu, address, ppu, n_rom, mem),
                    0xE0 => cpx(cpu, address, ppu, n_rom, mem),
                    0xA2 => ldx(cpu, address, ppu, n_rom, mem),
                    0xEB => sbc(cpu, address, ppu, n_rom, mem),
                    _ => 0,
                }
        }
        0xBE | 0xBF => {
            let (address, cycles) = abs_y(cpu, ppu, n_rom, mem);
            cycles
                + match op {
                    0xBE => ldx(cpu, address, ppu, n_rom, mem),
                    0xBF => lax(cpu, address, ppu, n_rom, mem),
                    _ => 0,
                }
        }
        _ => {
            match op & 0b0001_1111 {
                0x00 | 0x08 | 0x0A | 0x18 | 0x1A => {
                    match op {
                        0x00 => brk(cpu, ppu, n_rom, mem),
                        0x08 => php(cpu, ppu, n_rom, mem),
                        0x0A => asl_a(cpu),
                        0x18 => clc(cpu),
                        0x1A => nop(),
                        0x28 => plp(cpu, ppu, n_rom, mem),
                        0x2A => rol_a(cpu),
                        0x38 => sec(cpu),
                        0x3A => nop(),
                        0x40 => rti(cpu, ppu, n_rom, mem),
                        0x48 => pha(cpu, ppu, n_rom, mem),
                        0x4A => lsr_a(cpu),
                        0x58 => cli(cpu),
                        0x5A => nop(),
                        0x60 => rts(cpu, ppu, n_rom, mem),
                        0x68 => pla(cpu, ppu, n_rom, mem),
                        0x6A => ror_a(cpu),
                        0x78 => sei(cpu),
                        0x7A => nop(),
                        0x80 => nop(), // SKB
                        0x82 => nop(), // SKB
                        0x88 => dey(cpu),
                        0x89 => nop(), // SKB
                        0x8A => txa(cpu),
                        0x98 => tya(cpu),
                        0x9A => txs(cpu),
                        0xA8 => tay(cpu),
                        0xAA => tax(cpu),
                        0xB8 => clv(cpu),
                        0xBA => tsx(cpu),
                        0xC2 => nop(), // SKB
                        0xC8 => iny(cpu),
                        0xCA => dex(cpu),
                        0xD8 => cld(cpu),
                        0xDA => nop(),
                        0xE2 => nop(), // SKB
                        0xE8 => inx(cpu),
                        0xEA => nop(),
                        0xF8 => sed(cpu),
                        0xFA => nop(),
                        _ => 0,
                    }
                }
                0x01 | 0x03 => {
                    let (address, cycles) = x_ind(cpu, ppu, n_rom, mem);
                    cycles
                        + match op {
                            0x01 => ora(cpu, address, ppu, n_rom, mem),
                            0x03 => slo(cpu, address, ppu, n_rom, mem),
                            0x21 => and(cpu, address, ppu, n_rom, mem),
                            0x23 => rla(cpu, address, ppu, n_rom, mem),
                            0x41 => eor(cpu, address, ppu, n_rom, mem),
                            0x43 => sre(cpu, address, ppu, n_rom, mem),
                            0x61 => adc(cpu, address, ppu, n_rom, mem),
                            0x63 => rra(cpu, address, ppu, n_rom, mem),
                            0x81 => sta(cpu, address, ppu, n_rom, mem),
                            0x83 => sax(cpu, address, ppu, n_rom, mem),
                            0xA1 => lda(cpu, address, ppu, n_rom, mem),
                            0xA3 => lax(cpu, address, ppu, n_rom, mem),
                            0xC1 => cmp(cpu, address, ppu, n_rom, mem),
                            0xC3 => dcp(cpu, address, ppu, n_rom, mem),
                            0xE1 => sbc(cpu, address, ppu, n_rom, mem),
                            0xE3 => isc(cpu, address, ppu, n_rom, mem),
                            _ => 0,
                        }
                }
                0x11 | 0x13 => {
                    let (address, cycles) = ind_y(cpu, ppu, n_rom, mem);
                    cycles
                        + match op {
                            0x11 => ora(cpu, address, ppu, n_rom, mem),
                            0x13 => slo(cpu, address, ppu, n_rom, mem),
                            0x31 => and(cpu, address, ppu, n_rom, mem),
                            0x33 => rla(cpu, address, ppu, n_rom, mem),
                            0x51 => eor(cpu, address, ppu, n_rom, mem),
                            0x53 => sre(cpu, address, ppu, n_rom, mem),
                            0x71 => adc(cpu, address, ppu, n_rom, mem),
                            0x73 => rra(cpu, address, ppu, n_rom, mem),
                            0x91 => sta(cpu, address, ppu, n_rom, mem),
                            0xB1 => lda(cpu, address, ppu, n_rom, mem),
                            0xB3 => lax(cpu, address, ppu, n_rom, mem),
                            0xD1 => cmp(cpu, address, ppu, n_rom, mem),
                            0xD3 => dcp(cpu, address, ppu, n_rom, mem),
                            0xF1 => sbc(cpu, address, ppu, n_rom, mem),
                            0xF3 => isc(cpu, address, ppu, n_rom, mem),
                            _ => 0,
                        }
                }
                0x04..=0x07 => {
                    let (address, cycles) = zpg(cpu, ppu, n_rom, mem);
                    cycles
                        + match op {
                            0x04 => ign(cpu, address, ppu, n_rom, mem),
                            0x05 => ora(cpu, address, ppu, n_rom, mem),
                            0x06 => asl(cpu, address, ppu, n_rom, mem),
                            0x07 => slo(cpu, address, ppu, n_rom, mem),
                            0x24 => bit(cpu, address, ppu, n_rom, mem),
                            0x25 => and(cpu, address, ppu, n_rom, mem),
                            0x26 => rol(cpu, address, ppu, n_rom, mem),
                            0x27 => rla(cpu, address, ppu, n_rom, mem),
                            0x44 => ign(cpu, address, ppu, n_rom, mem),
                            0x45 => eor(cpu, address, ppu, n_rom, mem),
                            0x46 => lsr(cpu, address, ppu, n_rom, mem),
                            0x47 => sre(cpu, address, ppu, n_rom, mem),
                            0x64 => ign(cpu, address, ppu, n_rom, mem),
                            0x65 => adc(cpu, address, ppu, n_rom, mem),
                            0x66 => ror(cpu, address, ppu, n_rom, mem),
                            0x67 => rra(cpu, address, ppu, n_rom, mem),
                            0x84 => sty(cpu, address, ppu, n_rom, mem),
                            0x85 => sta(cpu, address, ppu, n_rom, mem),
                            0x86 => stx(cpu, address, ppu, n_rom, mem),
                            0x87 => sax(cpu, address, ppu, n_rom, mem),
                            0xA4 => ldy(cpu, address, ppu, n_rom, mem),
                            0xA5 => lda(cpu, address, ppu, n_rom, mem),
                            0xA6 => ldx(cpu, address, ppu, n_rom, mem),
                            0xA7 => lax(cpu, address, ppu, n_rom, mem),
                            0xC4 => cpy(cpu, address, ppu, n_rom, mem),
                            0xC5 => cmp(cpu, address, ppu, n_rom, mem),
                            0xC6 => dec(cpu, address, ppu, n_rom, mem),
                            0xC7 => dcp(cpu, address, ppu, n_rom, mem),
                            0xE4 => cpx(cpu, address, ppu, n_rom, mem),
                            0xE5 => sbc(cpu, address, ppu, n_rom, mem),
                            0xE6 => inc(cpu, address, ppu, n_rom, mem),
                            0xE7 => isc(cpu, address, ppu, n_rom, mem),
                            _ => 0,
                        }
                }
                0x14..=0x17 => {
                    let (address, cycles) = zpg_x(cpu, ppu, n_rom, mem);
                    cycles
                        + match op {
                            0x14 => ign(cpu, address, ppu, n_rom, mem),
                            0x15 => ora(cpu, address, ppu, n_rom, mem),
                            0x16 => asl(cpu, address, ppu, n_rom, mem),
                            0x17 => slo(cpu, address, ppu, n_rom, mem),
                            0x34 => ign(cpu, address, ppu, n_rom, mem),
                            0x35 => and(cpu, address, ppu, n_rom, mem),
                            0x36 => rol(cpu, address, ppu, n_rom, mem),
                            0x37 => rla(cpu, address, ppu, n_rom, mem),
                            0x54 => ign(cpu, address, ppu, n_rom, mem),
                            0x55 => eor(cpu, address, ppu, n_rom, mem),
                            0x56 => lsr(cpu, address, ppu, n_rom, mem),
                            0x57 => sre(cpu, address, ppu, n_rom, mem),
                            0x74 => ign(cpu, address, ppu, n_rom, mem),
                            0x75 => adc(cpu, address, ppu, n_rom, mem),
                            0x76 => ror(cpu, address, ppu, n_rom, mem),
                            0x77 => rra(cpu, address, ppu, n_rom, mem),
                            0x94 => sty(cpu, address, ppu, n_rom, mem),
                            0x95 => sta(cpu, address, ppu, n_rom, mem),
                            0xB4 => ldy(cpu, address, ppu, n_rom, mem),
                            0xB5 => lda(cpu, address, ppu, n_rom, mem),
                            0xD4 => ign(cpu, address, ppu, n_rom, mem),
                            0xD5 => cmp(cpu, address, ppu, n_rom, mem),
                            0xD6 => dec(cpu, address, ppu, n_rom, mem),
                            0xD7 => dcp(cpu, address, ppu, n_rom, mem),
                            0xF4 => ign(cpu, address, ppu, n_rom, mem),
                            0xF5 => sbc(cpu, address, ppu, n_rom, mem),
                            0xF6 => inc(cpu, address, ppu, n_rom, mem),
                            0xF7 => isc(cpu, address, ppu, n_rom, mem),
                            _ => 0,
                        }
                }
                0x10 | 0x09 => {
                    let (address, cycles) = imm_rel(cpu);
                    cycles
                        + match op {
                            0x09 => ora(cpu, address, ppu, n_rom, mem),
                            0x10 => bpl(cpu, address, ppu, n_rom, mem),
                            0x29 => and(cpu, address, ppu, n_rom, mem),
                            0x30 => bmi(cpu, address, ppu, n_rom, mem),
                            0x49 => eor(cpu, address, ppu, n_rom, mem),
                            0x50 => bvc(cpu, address, ppu, n_rom, mem),
                            0x69 => adc(cpu, address, ppu, n_rom, mem),
                            0x70 => bvs(cpu, address, ppu, n_rom, mem),
                            0x90 => bcc(cpu, address, ppu, n_rom, mem),
                            0xA9 => lda(cpu, address, ppu, n_rom, mem),
                            0xB0 => bcs(cpu, address, ppu, n_rom, mem),
                            0xC9 => cmp(cpu, address, ppu, n_rom, mem),
                            0xD0 => bne(cpu, address, ppu, n_rom, mem),
                            0xE9 => sbc(cpu, address, ppu, n_rom, mem),
                            0xF0 => beq(cpu, address, ppu, n_rom, mem),
                            _ => 0,
                        }
                }
                0x19 | 0x1B => {
                    let (address, cycles) = abs_y(cpu, ppu, n_rom, mem);
                    cycles
                        + match op {
                            0x19 => ora(cpu, address, ppu, n_rom, mem),
                            0x1B => slo(cpu, address, ppu, n_rom, mem),
                            0x39 => and(cpu, address, ppu, n_rom, mem),
                            0x3B => rla(cpu, address, ppu, n_rom, mem),
                            0x59 => eor(cpu, address, ppu, n_rom, mem),
                            0x5B => sre(cpu, address, ppu, n_rom, mem),
                            0x79 => adc(cpu, address, ppu, n_rom, mem),
                            0x7B => rra(cpu, address, ppu, n_rom, mem),
                            0x99 => sta(cpu, address, ppu, n_rom, mem),
                            0xB9 => lda(cpu, address, ppu, n_rom, mem),
                            0xD9 => cmp(cpu, address, ppu, n_rom, mem),
                            0xDB => dcp(cpu, address, ppu, n_rom, mem),
                            0xF9 => sbc(cpu, address, ppu, n_rom, mem),
                            0xFB => isc(cpu, address, ppu, n_rom, mem),
                            _ => 0,
                        }
                }
                0x0C..=0x0F => {
                    let (address, cycles) = abs(cpu, ppu, n_rom, mem);
                    cycles
                        + match op {
                            0x0C => ign(cpu, address, ppu, n_rom, mem),
                            0x0D => ora(cpu, address, ppu, n_rom, mem),
                            0x0E => asl(cpu, address, ppu, n_rom, mem),
                            0x0F => slo(cpu, address, ppu, n_rom, mem),
                            0x2C => bit(cpu, address, ppu, n_rom, mem),
                            0x2D => and(cpu, address, ppu, n_rom, mem),
                            0x2E => rol(cpu, address, ppu, n_rom, mem),
                            0x2F => rla(cpu, address, ppu, n_rom, mem),
                            0x4C => jmp(cpu, address),
                            0x4D => eor(cpu, address, ppu, n_rom, mem),
                            0x4E => lsr(cpu, address, ppu, n_rom, mem),
                            0x4F => sre(cpu, address, ppu, n_rom, mem),
                            0x6D => adc(cpu, address, ppu, n_rom, mem),
                            0x6E => ror(cpu, address, ppu, n_rom, mem),
                            0x6F => rra(cpu, address, ppu, n_rom, mem),
                            0x8C => sty(cpu, address, ppu, n_rom, mem),
                            0x8D => sta(cpu, address, ppu, n_rom, mem),
                            0x8E => stx(cpu, address, ppu, n_rom, mem),
                            0x8F => sax(cpu, address, ppu, n_rom, mem),
                            0xAC => ldy(cpu, address, ppu, n_rom, mem),
                            0xAD => lda(cpu, address, ppu, n_rom, mem),
                            0xAE => ldx(cpu, address, ppu, n_rom, mem),
                            0xAF => lax(cpu, address, ppu, n_rom, mem),
                            0xCC => cpy(cpu, address, ppu, n_rom, mem),
                            0xCD => cmp(cpu, address, ppu, n_rom, mem),
                            0xCE => dec(cpu, address, ppu, n_rom, mem),
                            0xCF => dcp(cpu, address, ppu, n_rom, mem),
                            0xEC => cpx(cpu, address, ppu, n_rom, mem),
                            0xED => sbc(cpu, address, ppu, n_rom, mem),
                            0xEE => inc(cpu, address, ppu, n_rom, mem),
                            0xEF => isc(cpu, address, ppu, n_rom, mem),
                            _ => 0,
                        }
                }
                0x1C..=0x1F => {
                    let (address, cycles) = abs_x(cpu, ppu, n_rom, mem);
                    cycles
                        + match op {
                            0x1C => ign(cpu, address, ppu, n_rom, mem),
                            0x1D => ora(cpu, address, ppu, n_rom, mem),
                            0x1E => asl(cpu, address, ppu, n_rom, mem),
                            0x1F => slo(cpu, address, ppu, n_rom, mem),
                            0x3C => ign(cpu, address, ppu, n_rom, mem),
                            0x3D => and(cpu, address, ppu, n_rom, mem),
                            0x3E => rol(cpu, address, ppu, n_rom, mem),
                            0x3F => rla(cpu, address, ppu, n_rom, mem),
                            0x5C => ign(cpu, address, ppu, n_rom, mem),
                            0x5D => eor(cpu, address, ppu, n_rom, mem),
                            0x5E => lsr(cpu, address, ppu, n_rom, mem),
                            0x5F => sre(cpu, address, ppu, n_rom, mem),
                            0x7C => ign(cpu, address, ppu, n_rom, mem),
                            0x7D => adc(cpu, address, ppu, n_rom, mem),
                            0x7E => ror(cpu, address, ppu, n_rom, mem),
                            0x7F => rra(cpu, address, ppu, n_rom, mem),
                            0x9D => sta(cpu, address, ppu, n_rom, mem),
                            0xBC => ldy(cpu, address, ppu, n_rom, mem),
                            0xBD => lda(cpu, address, ppu, n_rom, mem),
                            0xDC => ign(cpu, address, ppu, n_rom, mem),
                            0xDD => cmp(cpu, address, ppu, n_rom, mem),
                            0xDE => dec(cpu, address, ppu, n_rom, mem),
                            0xDF => dcp(cpu, address, ppu, n_rom, mem),
                            0xFC => ign(cpu, address, ppu, n_rom, mem),
                            0xFD => sbc(cpu, address, ppu, n_rom, mem),
                            0xFE => inc(cpu, address, ppu, n_rom, mem),
                            0xFF => isc(cpu, address, ppu, n_rom, mem),
                            _ => 0,
                        }
                }
                _ => 0,
            }
        }
    }
}

pub fn init_cpu(ppu: &mut PPU, n_rom: &NROM, mem: &[u8]) -> CPU {
    let mut cpu = CPU {
        pc: 0,
        ac: 0,
        x: 0,
        y: 0,
        sr: 0,
        sp: 0,
        nmi: false,
        controller_state: [0; 2],
        controller: [0; 2],
    };
    cpu.pc = (read(&mut cpu, ppu, n_rom, mem, RESET_VECTOR + 1) as u16) << 8;
    cpu.pc += read(&mut cpu, ppu, n_rom, mem, RESET_VECTOR) as u16;

    cpu
}

pub fn nmi(cpu: &mut CPU, n_rom: &mut NROM, ppu: &mut PPU, mem: &mut Vec<u8>) -> usize {
    push(cpu, ppu, mem, n_rom, (cpu.pc >> 8) as u8);
    push(cpu, ppu, mem, n_rom, (cpu.pc & 0xFF) as u8);
    push(cpu, ppu, mem, n_rom, cpu.sr | 0b0011_0000);
    cpu.sr |= 0b0000_0100;
    cpu.pc = (read(cpu, ppu, n_rom, mem, NMI_VECTOR + 1) as u16) << 8;
    cpu.pc += read(cpu, ppu, n_rom, mem, NMI_VECTOR) as u16;
    cpu.nmi = false;

    7
}

pub fn push(cpu: &mut CPU, ppu: &mut PPU, mem: &mut Vec<u8>, n_rom: &mut NROM, byte: u8) {
    write(cpu, ppu, mem, n_rom, 0x100 + cpu.sp as u16, byte);
    cpu.sp = cpu.sp.wrapping_sub(1);
}

pub fn write(
    cpu: &mut CPU,
    ppu: &mut PPU,
    mem: &mut Vec<u8>,
    n_rom: &mut NROM,
    address: u16,
    value: u8,
) {
    match address {
        0x0000..=0x1FFF => mem[(address % 0x0800) as usize] = value,
        0x2000..=0x3FFF => write_ppu_reg(ppu, n_rom, address % 8, value),
        0x4014 => write_dma(ppu, value),
        0x4016..=0x4017 => write_controller(cpu, address),
        0x4020..=0xFFFF => write_nrom(n_rom, address as usize, value),
        _ => (),
    }
}

fn write_dma(ppu: &mut PPU, value: u8) {
    ppu.dma_page = value;
    ppu.dma_addr = 0x00;
    ppu.dma_transfer = true;
}

fn write_controller(cpu: &mut CPU, address: u16) {
    let controller_number = (address & 0x0001) as usize;
    cpu.controller_state[controller_number] = cpu.controller[controller_number];
}

pub fn write_nrom(n_rom: &mut NROM, address: usize, value: u8) {
    match address {
        0x0000..=0x1FFF => {
            if n_rom.chr_rom_size == 0 {
                n_rom.chr_ram[address] = value;
            }
        }
        _ => panic!("invalid write to NROM mapper: 0x{:X}", address),
    }
}

pub fn pull(cpu: &mut CPU, ppu: &mut PPU, mem: &mut Vec<u8>, n_rom: &NROM) -> u8 {
    cpu.sp = cpu.sp.wrapping_add(1);

    read(cpu, ppu, n_rom, mem, 0x100 + cpu.sp as u16)
}

pub fn read(cpu: &mut CPU, ppu: &mut PPU, n_rom: &NROM, mem: &[u8], address: u16) -> u8 {
    match address {
        0x0000..=0x1FFF => mem[(address % 0x0800) as usize],
        0x2000..=0x3FFF => read_ppu_reg(ppu, n_rom, address % 8),
        0x4000..=0x4015 => 0,
        0x4016..=0x4017 => read_controller(cpu, address),
        0x4020..=0xFFFF => read_nrom(n_rom, address as usize),
        _ => panic!("invalid read from 0x{:02x}", address),
    }
}

fn read_controller(cpu: &mut CPU, address: u16) -> u8 {
    let controller_number = (address & 0x0001) as usize;
    let data = ((cpu.controller_state[controller_number] & 0x80) > 0) as u8;
    cpu.controller_state[controller_number] <<= 1;

    data
}

pub fn read_nrom(n_rom: &NROM, address: usize) -> u8 {
    let addr = address % 0x4000;
    match address {
        0x0000..=0x1FFF => {
            if n_rom.chr_rom_size > 0 {
                n_rom.chr_rom[0][address]
            } else {
                n_rom.chr_ram[address]
            }
        }
        0x8000..=0xBFFF => n_rom.prg_rom[0][addr],
        0xC000..=0xFFFF => n_rom.prg_rom[n_rom.prg_rom_size - 1][addr],
        _ => {
            panic!("bad address read from NROM mapper: 0x{:X}", address);
        }
    }
}
