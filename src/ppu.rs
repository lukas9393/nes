use crate::cpu::{read_nrom, write_nrom, CPU};
use crate::NROM;
use sdl2::pixels::Color;

pub struct PPU {
    // $2000	VPHB SINN	NMI enable (V), PPU master/slave (P), sprite height (H),
    //          background tile select (B), sprite tile select (S), increment mode (I),
    //          nametable select (NN)
    pub ppuctrl: u8,

    // $2001	BGRs bMmG	color emphasis (BGR), sprite enable (s), background enable (b),
    //          sprite left column enable (M), background left column enable (m), greyscale (G)
    pub ppumask: u8,

    // $2002	VSO- ----	vblank (V), sprite 0 hit (S), sprite overflow (O);
    //          read resets write pair for $2005/$2006
    pub ppustatus: u8,

    // $2003	aaaa aaaa	OAM read/write address
    oamaddr: u8,

    // $2004	dddd dddd	OAM data read/write
    // oamdata: u8,

    // $2007	dddd dddd	PPU data read/write
    pub ppudata: u8,

    // yyy NNYY YYYX XXXX   fine Y scroll (yyy), nametable select (NN), coarse Y scroll (YYYYY),
    // coarse X scroll (XXXXX) current VRAM address (15 bits)
    pub vram_address: u16,

    // yyy NNYY YYYX XXXX    fine Y scroll (yyy), nametable select (NN), coarse Y scroll (YYYYY),
    // coarse X scroll (XXXXX) temporary VRAM address (15 bits)
    pub t_vram_address: u16,

    // fine X scroll (3 bits)
    pub x: u8,

    // first or second write toggle (1 bit)
    pub w: u8,

    cycle: u16,
    scanline: u16,

    pub tbl_name: Vec<Vec<u8>>,
    pub tbl_palette: Vec<u8>,
    pub frame_complete: bool,
    pub buffer: Vec<u8>,
    pub primary_oam: Vec<u8>,
    pub ppu_data_buffer: u8,

    nametable_byte: u8,
    background_palette_latch: u8,
    high_pattern_table_byte: u16,
    low_pattern_table_byte: u16,

    // These contain the pattern table data for two tiles. Every 8 cycles, the data for the next
    // tile is loaded into the upper 8 bits of this shift register. Meanwhile, the pixel to render
    // is fetched from one of the lower 8 bits.
    background_pattern_table_lo: u16,
    background_pattern_table_hi: u16,

    // These contain the palette attributes for the lower 8 pixels of the 16-bit shift register.
    // These registers are fed by a latch which contains the palette attribute for the next tile.
    // Every 8 cycles, the latch is loaded with the palette attribute for the next tile.
    background_palette_attributes_lo: u16,
    background_palette_attributes_hi: u16,

    pal_screen: Vec<Color>,

    pub dma_page: u8,
    pub dma_addr: u8,
    pub dma_transfer: bool,
    pub dma_dummy: bool,
    pub dma_data: u8,

    pub sprite_shifter_pattern_lo: Vec<u8>,
    pub sprite_shifter_pattern_hi: Vec<u8>,
    pub b_sprite_zero_hit_possible: bool,
    pub secondary_oam: Vec<u8>,
    pub sprite_count: usize,
    pub odd_frame: bool,
}

pub fn set_ppustatus_v(ppu: &mut PPU, value: u8) {
    ppu.ppustatus &= 0b0111_1111;
    ppu.ppustatus |= value << 7;
}

pub fn ppu_step(cpu: &mut CPU, ppu: &mut PPU, n_rom: &NROM) {
    match ppu.scanline {
        0..=239 | 261 => match ppu.cycle {
            1..=256 | 321..=336 => {
                if ppu.ppumask & 0b0001_1000 != 0 {
                    update_shifters(ppu);
                    match ppu.cycle % 8 {
                        1 => {
                            if ppu.scanline == 261 {
                                ppu.ppustatus &= 0b0001_1111;

                                for i in 0..8 {
                                    ppu.sprite_shifter_pattern_lo[i] = 0;
                                    ppu.sprite_shifter_pattern_hi[i] = 0;
                                }
                            }
                            fetch_nt_byte(ppu, n_rom)
                        }
                        3 => fetch_at_byte(ppu, n_rom),
                        5 => fetch_low_bg_tile_byte(ppu, n_rom),
                        7 => fetch_high_bg_tile_byte(ppu, n_rom),
                        0 => {
                            load_bg_shifters(ppu);
                            increment_horizontal(ppu);
                            if ppu.cycle == 256 {
                                increment_vertical(ppu);
                            }
                        }
                        _ => (),
                    }
                };
            }
            257 => {
                copy_horizontal(ppu);
                fetch_sprites(ppu)
            }
            280..=304 => {
                if ppu.scanline == 261 {
                    reload_vertical_scroll_bits(ppu)
                }
            }
            339 => load_fg_shifters(ppu, n_rom),
            _ => (),
        },
        241 => {
            if ppu.cycle == 1 {
                ppu.ppustatus |= 0b1000_0000;
                if ppu.ppuctrl & 0b1000_0000 != 0 {
                    cpu.nmi = true;
                }
            }
        }
        _ => (),
    }

    render_pixel(ppu);

    ppu.cycle += 1;
    if ppu.cycle >= 341 || ppu.cycle > 339 && ppu.odd_frame {
        ppu.cycle = 0;
        ppu.scanline += 1;
        if ppu.scanline > 261 {
            ppu.scanline = 0;
            ppu.frame_complete = true;
            ppu.odd_frame = !ppu.odd_frame;
        }
    }
}

fn render_pixel(ppu: &mut PPU) {
    let background_pixel = if ppu.ppumask & 0b0000_1000 != 0 {
        let bit_mux = 1 << (15 - ppu.x as u16);
        let low_bit = (ppu.background_pattern_table_lo & bit_mux != 0) as u8;
        let high_bit = (ppu.background_pattern_table_hi & bit_mux != 0) as u8;
        (high_bit << 1) | low_bit
    } else {
        0
    };

    let background_palette = if ppu.ppumask & 0b0000_1000 != 0 {
        let bit_mux = 1 << (15 - ppu.x as u16);
        let low_palette_bit = (ppu.background_palette_attributes_lo & bit_mux != 0) as u8;
        let high_palette_bit = (ppu.background_palette_attributes_hi & bit_mux != 0) as u8;
        (high_palette_bit << 1) | low_palette_bit
    } else {
        0
    };

    let mut foreground_pixel = 0;
    let mut foreground_palette = 0;
    let mut foreground_priority = false;
    let mut b_sprite_zero_being_rendered = false;
    if ppu.ppumask & 0b0001_0000 != 0 {
        for i in 0..ppu.sprite_count {
            if ppu.secondary_oam[i * 4 + 3] == 0 {
                foreground_palette = (ppu.secondary_oam[i * 4 + 2] & 0x03) + 0x04;
                foreground_priority = (ppu.secondary_oam[i * 4 + 2] & 0x20) == 0;

                let fg_pixel_lo = ((ppu.sprite_shifter_pattern_lo[i] & 0x80) > 0) as u8;
                let fg_pixel_hi = ((ppu.sprite_shifter_pattern_hi[i] & 0x80) > 0) as u8;
                foreground_pixel = (fg_pixel_hi << 1) | fg_pixel_lo;
                if foreground_pixel != 0 {
                    if i == 0 {
                        b_sprite_zero_being_rendered = true;
                    }
                    break;
                }
            }
        }
    }

    let palette_address = if background_pixel == 0 && foreground_pixel > 0 {
        foreground_pixel + (foreground_palette << 2)
    } else if background_pixel > 0 && foreground_pixel == 0 {
        background_pixel + (background_palette << 2)
    } else if background_pixel > 0 && foreground_pixel > 0 && foreground_priority {
        foreground_pixel + (foreground_palette << 2)
    } else if background_pixel > 0 && foreground_pixel > 0 {
        background_pixel + (background_palette << 2)
    } else {
        0
    };

    if background_pixel != 0
        && foreground_pixel != 0
        && ppu.b_sprite_zero_hit_possible
        && b_sprite_zero_being_rendered
        && ppu.ppumask & 0b0001_1000 == 0b0001_1000
    {
        if (!ppu.ppumask & 0b0000_0010 | ppu.ppumask & 0b0000_0100) != 0 {
            if ppu.cycle >= 9 && ppu.cycle < 258 {
                ppu.ppustatus |= 0b0100_0000;
            }
        } else if ppu.cycle >= 1 && ppu.cycle < 258 {
            ppu.ppustatus |= 0b0100_0000;
        }
    }

    let index = ppu.tbl_palette[palette_address as usize] as usize;
    let pixel = ppu.pal_screen[index as usize];
    if ppu.cycle > 0 && ppu.scanline < 240 && ppu.cycle < 256 {
        let offset = (ppu.scanline as usize) * 256 * 3 + ((ppu.cycle as usize) - 1) * 3;
        ppu.buffer[offset] = pixel.r;
        ppu.buffer[offset + 1] = pixel.g;
        ppu.buffer[offset + 2] = pixel.b;
    }
}

fn load_fg_shifters(ppu: &mut PPU, n_rom: &NROM) {
    for i in 0..ppu.sprite_count {
        // 8x8 Sprite Mode - The control register determines the pattern table
        let sprite_pattern_addr_lo = if ppu.secondary_oam[i * 4 + 2] & 0x80 == 0 {
            ((ppu.ppuctrl as u16) & 0b0000_1000 << 9)
                | ((ppu.secondary_oam[i * 4 + 1] as u16) << 4)
                | (ppu.scanline - (ppu.secondary_oam[i * 4] as u16))
        } else {
            ((ppu.ppuctrl as u16) & 0b0000_1000 << 9)
                | ((ppu.secondary_oam[i * 4 + 1] as u16) << 4)
                | (7 - (ppu.scanline - (ppu.secondary_oam[i * 4] as u16)))
        };

        let mut sprite_pattern_bits_lo = read_ppu(ppu, n_rom, sprite_pattern_addr_lo as usize);
        let mut sprite_pattern_bits_hi =
            read_ppu(ppu, n_rom, (sprite_pattern_addr_lo as usize) + 8);

        if ppu.secondary_oam[i * 4 + 2] & 0x40 != 0 {
            sprite_pattern_bits_lo =
                (sprite_pattern_bits_lo & 0xF0) >> 4 | (sprite_pattern_bits_lo & 0x0F) << 4;
            sprite_pattern_bits_lo =
                (sprite_pattern_bits_lo & 0xCC) >> 2 | (sprite_pattern_bits_lo & 0x33) << 2;
            sprite_pattern_bits_lo =
                (sprite_pattern_bits_lo & 0xAA) >> 1 | (sprite_pattern_bits_lo & 0x55) << 1;

            sprite_pattern_bits_hi =
                (sprite_pattern_bits_hi & 0xF0) >> 4 | (sprite_pattern_bits_hi & 0x0F) << 4;
            sprite_pattern_bits_hi =
                (sprite_pattern_bits_hi & 0xCC) >> 2 | (sprite_pattern_bits_hi & 0x33) << 2;
            sprite_pattern_bits_hi =
                (sprite_pattern_bits_hi & 0xAA) >> 1 | (sprite_pattern_bits_hi & 0x55) << 1;
        }
        ppu.sprite_shifter_pattern_lo[i] = sprite_pattern_bits_lo;
        ppu.sprite_shifter_pattern_hi[i] = sprite_pattern_bits_hi;
    }
}

fn fetch_sprites(ppu: &mut PPU) {
    for i in 0..8 {
        ppu.sprite_shifter_pattern_lo[i] = 0;
        ppu.sprite_shifter_pattern_hi[i] = 0;
    }

    ppu.secondary_oam = vec![0xFF; 0x20];

    ppu.sprite_count = 0;
    ppu.b_sprite_zero_hit_possible = false;
    for n in 0..64 {
        let diff = ppu.scanline as isize - ppu.primary_oam[n * 4] as isize;
        let sprite_size = if ppu.ppuctrl & 0b0010_0000 != 0 {
            16
        } else {
            8
        };
        if diff >= 0 && diff < sprite_size && ppu.sprite_count < 8 {
            ppu.secondary_oam[ppu.sprite_count * 4] = ppu.primary_oam[n * 4];
            ppu.secondary_oam[ppu.sprite_count * 4 + 1] = ppu.primary_oam[n * 4 + 1];
            ppu.secondary_oam[ppu.sprite_count * 4 + 2] = ppu.primary_oam[n * 4 + 2];
            ppu.secondary_oam[ppu.sprite_count * 4 + 3] = ppu.primary_oam[n * 4 + 3];
            ppu.sprite_count += 1;
            if n == 0 {
                ppu.b_sprite_zero_hit_possible = true;
            }
        }
    }
    ppu.ppustatus &= 0b1101_1111;
    if ppu.sprite_count > 8 {
        ppu.ppustatus |= 0b0010_0000;
    }
}

fn update_shifters(ppu: &mut PPU) {
    ppu.background_pattern_table_lo <<= 1;
    ppu.background_pattern_table_hi <<= 1;
    ppu.background_palette_attributes_lo <<= 1;
    ppu.background_palette_attributes_hi <<= 1;

    if ppu.cycle > 0 && ppu.cycle < 256 {
        for i in 0..ppu.sprite_count {
            if ppu.secondary_oam[i * 4 + 3] > 0 {
                ppu.secondary_oam[i * 4 + 3] -= 1;
            } else {
                ppu.sprite_shifter_pattern_lo[i] <<= 1;
                ppu.sprite_shifter_pattern_hi[i] <<= 1;
            }
        }
    }
}

fn load_bg_shifters(ppu: &mut PPU) {
    ppu.background_pattern_table_lo |= ppu.low_pattern_table_byte as u16;
    ppu.background_pattern_table_hi |= ppu.high_pattern_table_byte as u16;

    if ppu.background_palette_latch & 0b0000_0001 != 0 {
        ppu.background_palette_attributes_lo |= 0xFF
    }

    if ppu.background_palette_latch & 0b0000_0010 != 0 {
        ppu.background_palette_attributes_hi |= 0xFF
    }
}

fn reload_vertical_scroll_bits(ppu: &mut PPU) {
    // During dots 280 to 304 of the pre-render scanline (end of vblank)
    // If rendering is enabled, at the end of vblank, shortly after the horizontal bits are
    // copied from t to v at dot 257, the PPU will repeatedly copy the vertical bits from t
    // to v from dots 280 to 304, completing the full initialization of v from t:
    // v: IHGF.ED CBA..... = t: IHGF.ED CBA.....
    // During pixels 280 through 304 of this scanline, the vertical scroll bits are reloaded if
    // rendering is enabled.
    if ppu.ppumask & 0b0001_1000 != 0 {
        ppu.vram_address &= 0b1000_0100_0001_1111;
        ppu.vram_address |= ppu.t_vram_address & 0b0111_1011_1110_0000;
    }
}

fn copy_horizontal(ppu: &mut PPU) {
    // At dot 257 of each scanline
    // If rendering is enabled, the PPU copies all bits related to horizontal
    // position from t to v:
    // v: ....F.. ...EDCBA = t: ....F.. ...EDCBA
    if ppu.ppumask & 0b0001_1000 != 0 {
        ppu.vram_address &= 0b1111_1011_1110_0000;
        ppu.vram_address |= ppu.t_vram_address & 0b0000_0100_0001_1111;
    }
}

fn increment_vertical(ppu: &mut PPU) {
    // At dot 256 of each scanline
    // If rendering is enabled, the PPU increments the vertical position in v.
    // The effective Y scroll coordinate is incremented, which is a complex
    // operation that will correctly skip the attribute table memory regions,
    // and wrap to the next nametable appropriately. See Wrapping around below.
    // Y increment
    // If rendering is enabled, fine Y is incremented at dot 256 of each
    // scanline, overflowing to coarse Y, and finally adjusted to wrap among
    // the nametables vertically.
    // Bits 12-14 are fine Y. Bits 5-9 are coarse Y. Bit 11 selects the
    // vertical nametable.
    if (ppu.vram_address & 0x7000) != 0x7000 {
        ppu.vram_address += 0x1000;
    } else {
        ppu.vram_address &= !0x7000;
        let mut y = (ppu.vram_address & 0x03E0) >> 5;
        if y == 29 {
            y = 0;
            ppu.vram_address ^= 0x0800;
        } else if y == 31 {
            y = 0;
        } else {
            y += 1;
        }
        ppu.vram_address = (ppu.vram_address & !0x03E0) | (y << 5);
    }
}

fn increment_horizontal(ppu: &mut PPU) {
    // Between dot 328 of a scanline, and 256 of the next scanline
    // If rendering is enabled, the PPU increments the horizontal
    // position in v many times across the scanline, it begins at dots
    // 328 and 336, and will continue through the next scanline at 8,
    // 16, 24... 240, 248, 256 (every 8 dots across the scanline until
    // 256). Across the scanline the effective coarse X scroll
    // coordinate is incremented repeatedly, which will also wrap to
    // the next nametable appropriately.
    // Coarse X increment
    if ppu.vram_address & 0x001F == 0x001F {
        ppu.vram_address &= !0x001F;
        ppu.vram_address ^= 0x0400;
    } else {
        ppu.vram_address += 1;
    }
}

fn fetch_nt_byte(ppu: &mut PPU, n_rom: &NROM) {
    ppu.nametable_byte = read_ppu(ppu, n_rom, (0x2000 | ppu.vram_address & 0x0FFF) as usize);
}

fn fetch_at_byte(ppu: &mut PPU, n_rom: &NROM) {
    // Fetch the corresponding attribute table entry from $23C0-$2FFF and increment the current VRAM address within the same row.
    let address = (0x23C0
        | (ppu.vram_address & 0x0C00)
        | ((ppu.vram_address >> 4) & 0x38)
        | ((ppu.vram_address >> 2) & 0x07)) as usize;
    let byte = read_ppu(ppu, n_rom, address);
    let coarse_x = ppu.vram_address & 0b0000_0000_0001_1111;
    let coarse_y = (ppu.vram_address & 0b0000_0011_1110_0000) >> 5;
    ppu.background_palette_latch = match ((coarse_y / 2) % 2, (coarse_x / 2) % 2) {
        (0, 0) => byte & 0b0000_0011,
        (0, 1) => (byte >> 2) & 0b0000_0011,
        (1, 0) => (byte >> 4) & 0b0000_0011,
        (1, 1) => (byte >> 6) & 0b0000_0011,
        _ => panic!("should not get here"),
    };
}

fn fetch_low_bg_tile_byte(ppu: &mut PPU, n_rom: &NROM) {
    let address = (((ppu.ppuctrl & 0b0001_0000) as u16) << 8)
        + ((ppu.nametable_byte as u16) << 4)
        + ((ppu.vram_address >> 12) & 0x07);
    ppu.low_pattern_table_byte = read_ppu(ppu, n_rom, address as usize) as u16;
}

fn fetch_high_bg_tile_byte(ppu: &mut PPU, n_rom: &NROM) {
    let address = (((ppu.ppuctrl & 0b0001_0000) as u16) << 8)
        + ((ppu.nametable_byte as u16) << 4)
        + ((ppu.vram_address >> 12) & 0x07)
        + 8;
    ppu.high_pattern_table_byte = read_ppu(ppu, n_rom, address as usize) as u16;
}

pub fn init_ppu() -> PPU {
    let mut pal_screen = vec![Color::RGB(0, 0, 0); 0x40];
    pal_screen[0x00] = Color::RGB(84, 84, 84);
    pal_screen[0x01] = Color::RGB(0, 30, 116);
    pal_screen[0x02] = Color::RGB(8, 16, 144);
    pal_screen[0x03] = Color::RGB(48, 0, 136);
    pal_screen[0x04] = Color::RGB(68, 0, 100);
    pal_screen[0x05] = Color::RGB(92, 0, 48);
    pal_screen[0x06] = Color::RGB(84, 4, 0);
    pal_screen[0x07] = Color::RGB(60, 24, 0);
    pal_screen[0x08] = Color::RGB(32, 42, 0);
    pal_screen[0x09] = Color::RGB(8, 58, 0);
    pal_screen[0x0A] = Color::RGB(0, 64, 0);
    pal_screen[0x0B] = Color::RGB(0, 60, 0);
    pal_screen[0x0C] = Color::RGB(0, 50, 60);
    pal_screen[0x0D] = Color::RGB(0, 0, 0);
    pal_screen[0x0E] = Color::RGB(0, 0, 0);
    pal_screen[0x0F] = Color::RGB(0, 0, 0);

    pal_screen[0x10] = Color::RGB(152, 150, 152);
    pal_screen[0x11] = Color::RGB(8, 76, 196);
    pal_screen[0x12] = Color::RGB(48, 50, 236);
    pal_screen[0x13] = Color::RGB(92, 30, 228);
    pal_screen[0x14] = Color::RGB(136, 20, 176);
    pal_screen[0x15] = Color::RGB(160, 20, 100);
    pal_screen[0x16] = Color::RGB(152, 34, 32);
    pal_screen[0x17] = Color::RGB(120, 60, 0);
    pal_screen[0x18] = Color::RGB(84, 90, 0);
    pal_screen[0x19] = Color::RGB(40, 114, 0);
    pal_screen[0x1A] = Color::RGB(8, 124, 0);
    pal_screen[0x1B] = Color::RGB(0, 118, 40);
    pal_screen[0x1C] = Color::RGB(0, 102, 120);
    pal_screen[0x1D] = Color::RGB(0, 0, 0);
    pal_screen[0x1E] = Color::RGB(0, 0, 0);
    pal_screen[0x1F] = Color::RGB(0, 0, 0);

    pal_screen[0x20] = Color::RGB(236, 238, 236);
    pal_screen[0x21] = Color::RGB(76, 154, 236);
    pal_screen[0x22] = Color::RGB(120, 124, 236);
    pal_screen[0x23] = Color::RGB(176, 98, 236);
    pal_screen[0x24] = Color::RGB(228, 84, 236);
    pal_screen[0x25] = Color::RGB(236, 88, 180);
    pal_screen[0x26] = Color::RGB(236, 106, 100);
    pal_screen[0x27] = Color::RGB(212, 136, 32);
    pal_screen[0x28] = Color::RGB(160, 170, 0);
    pal_screen[0x29] = Color::RGB(116, 196, 0);
    pal_screen[0x2A] = Color::RGB(76, 208, 32);
    pal_screen[0x2B] = Color::RGB(56, 204, 108);
    pal_screen[0x2C] = Color::RGB(56, 180, 204);
    pal_screen[0x2D] = Color::RGB(60, 60, 60);
    pal_screen[0x2E] = Color::RGB(0, 0, 0);
    pal_screen[0x2F] = Color::RGB(0, 0, 0);

    pal_screen[0x30] = Color::RGB(236, 238, 236);
    pal_screen[0x31] = Color::RGB(168, 204, 236);
    pal_screen[0x32] = Color::RGB(188, 188, 236);
    pal_screen[0x33] = Color::RGB(212, 178, 236);
    pal_screen[0x34] = Color::RGB(236, 174, 236);
    pal_screen[0x35] = Color::RGB(236, 174, 212);
    pal_screen[0x36] = Color::RGB(236, 180, 176);
    pal_screen[0x37] = Color::RGB(228, 196, 144);
    pal_screen[0x38] = Color::RGB(204, 210, 120);
    pal_screen[0x39] = Color::RGB(180, 222, 120);
    pal_screen[0x3A] = Color::RGB(168, 226, 144);
    pal_screen[0x3B] = Color::RGB(152, 226, 180);
    pal_screen[0x3C] = Color::RGB(160, 214, 228);
    pal_screen[0x3D] = Color::RGB(160, 162, 160);
    pal_screen[0x3E] = Color::RGB(0, 0, 0);
    pal_screen[0x3F] = Color::RGB(0, 0, 0);

    PPU {
        ppuctrl: 0,
        ppumask: 0,
        ppustatus: 0,
        oamaddr: 0,
        x: 0,
        t_vram_address: 0,
        vram_address: 0,
        ppudata: 0,

        cycle: 0,
        scanline: 0,
        tbl_name: vec![vec![0; 0x1024]; 2],
        tbl_palette: vec![0; 0x1024],
        frame_complete: false,
        buffer: vec![0; 240 * 256 * 3],
        primary_oam: vec![0; 0x1024],
        ppu_data_buffer: 0,
        nametable_byte: 0,
        background_palette_latch: 0,
        high_pattern_table_byte: 0,
        low_pattern_table_byte: 0,
        background_pattern_table_lo: 0,
        background_pattern_table_hi: 0,
        background_palette_attributes_lo: 0,
        background_palette_attributes_hi: 0,
        w: 0,
        pal_screen,
        dma_page: 0,
        dma_addr: 0,
        dma_transfer: false,
        dma_dummy: false,
        dma_data: 0,
        sprite_shifter_pattern_lo: vec![0; 8],
        sprite_shifter_pattern_hi: vec![0; 8],
        b_sprite_zero_hit_possible: false,
        secondary_oam: vec![0xFF; 32],
        sprite_count: 0,
        odd_frame: true,
    }
}

fn write_ppu(ppu: &mut PPU, n_rom: &mut NROM, mut address: usize, data: u8) {
    match address {
        0x0000..=0x1FFF => write_nrom(n_rom, address, data),
        0x2000..=0x3EFF => {
            address &= 0x0FFF;
            if n_rom.flags_6 & 0b0000_0001 != 0 {
                // Vertical
                if address <= 0x03FF {
                    ppu.tbl_name[0][address & 0x03FF] = data;
                } else if address >= 0x0400 && address <= 0x07FF {
                    ppu.tbl_name[1][address & 0x03FF] = data;
                } else if address >= 0x0800 && address <= 0x0BFF {
                    ppu.tbl_name[0][address & 0x03FF] = data;
                } else if address >= 0x0C00 && address <= 0x0FFF {
                    ppu.tbl_name[1][address & 0x03FF] = data;
                } else {
                    panic!("invalid write to 0x{:02X}", address)
                }
            } else {
                // Horizontal
                if address <= 0x07FF {
                    ppu.tbl_name[0][address & 0x03FF] = data;
                } else if address >= 0x0800 && address <= 0x0FFF {
                    ppu.tbl_name[1][address & 0x03FF] = data;
                } else {
                    panic!("invalid write to 0x{:02X}", address)
                }
            }
        }
        0x3F00..=0x3FFF => match address % 0x10 {
            0x00 => {
                ppu.tbl_palette[0] = data;
                ppu.tbl_palette[0x10] = data;
            }
            0x04 => {
                ppu.tbl_palette[0x04] = data;
                ppu.tbl_palette[0x14] = data;
            }
            0x08 => {
                ppu.tbl_palette[0x08] = data;
                ppu.tbl_palette[0x18] = data;
            }
            0x0C => {
                ppu.tbl_palette[0x0C] = data;
                ppu.tbl_palette[0x1C] = data;
            }
            _ => ppu.tbl_palette[address % 0x0020] = data,
        },
        _ => panic!("invalid write to 0x{:02X}", address),
    }
}

pub fn read_ppu(ppu: &mut PPU, n_rom: &NROM, mut address: usize) -> u8 {
    match address {
        0x0000..=0x1FFF => read_nrom(n_rom, address),
        0x2000..=0x3EFF => {
            address &= 0x0FFF;
            if n_rom.flags_6 & 0b0000_0001 != 0 {
                // Vertical
                if address <= 0x03FF {
                    ppu.tbl_name[0][address & 0x03FF]
                } else if address >= 0x0400 && address <= 0x07FF {
                    ppu.tbl_name[1][address & 0x03FF]
                } else if address >= 0x0800 && address <= 0x0BFF {
                    ppu.tbl_name[0][address & 0x03FF]
                } else if address >= 0x0C00 && address <= 0x0FFF {
                    ppu.tbl_name[1][address & 0x03FF]
                } else {
                    panic!("invalid read to 0x{:04X}", address);
                }
            } else {
                // Horizontal
                if address <= 0x07FF {
                    ppu.tbl_name[0][address & 0x03FF]
                } else if address >= 0x0800 && address <= 0x0FFF {
                    ppu.tbl_name[1][address & 0x03FF]
                } else {
                    panic!("invalid read to 0x{:04X}", address);
                }
            }
        }
        0x3F00..=0x3FFF => ppu.tbl_palette[address & 0x000F], // & (mask.grayscale ? 0x30 : 0x3F);
        _ => panic!("invalid read to 0x{:04X}", address),
    }
}

pub fn write_ppu_reg(ppu: &mut PPU, n_rom: &mut NROM, address: u16, data: u8) {
    match address {
        0x00 => {
            // t: ...BA.. ........ = d: ......BA
            ppu.ppuctrl = data;
            ppu.t_vram_address &= 0b1111_0011_1111_1111;
            ppu.t_vram_address |= ((data & 0b0000_0011) as u16) << 10;
        }
        0x01 => ppu.ppumask = data,
        0x03 => ppu.oamaddr = data,
        0x04 => ppu.primary_oam[ppu.oamaddr as usize] = data,
        0x05 => {
            if ppu.w == 0 {
                // $2005 first write (w is 0)
                // t: ....... ...HGFED = d: HGFED...
                // x:              CBA = d: .....CBA
                // w:                  = 1
                ppu.t_vram_address &= 0b1111_1111_1110_0000;
                ppu.t_vram_address |= (data >> 3) as u16;
                ppu.x = data & 0b0000_0111;
                ppu.w = 1;
            } else {
                // t: CBA..HG FED..... = d: HGFEDCBA
                // w:                  = 0
                ppu.t_vram_address &= 0b1000_1100_0001_1111;
                ppu.t_vram_address |=
                    ((data & 0b0000_0111) as u16) << 12 | ((data & 0b1111_1000) as u16) << 2;
                ppu.w = 0;
            }
        }
        0x06 => {
            if ppu.w == 0 {
                // t: .FEDCBA ........ = d: ..FEDCBA
                // t: X...... ........ = 0
                // w:                  = 1
                ppu.t_vram_address &= data as u16 & 0b1000_0000_1111_1111;
                ppu.t_vram_address |= (data as u16 & 0b0011_1111) << 8;
                ppu.w = 1;
            } else {
                // t: ....... HGFEDCBA = d: HGFEDCBA
                // v                   = t
                // w:                  = 0
                ppu.t_vram_address &= 0xFF00;
                ppu.t_vram_address |= data as u16;
                ppu.vram_address = ppu.t_vram_address;
                ppu.w = 0;
            }
        }
        0x07 => {
            write_ppu(ppu, n_rom, ppu.vram_address as usize, data);
            if (ppu.ppuctrl & 0b0000_0100) >> 2 != 0 {
                ppu.vram_address += 32;
            } else {
                ppu.vram_address += 1;
            }
        }
        _ => panic!("invalid write to 0x{:02X}", address),
    }
}

pub fn read_ppu_reg(ppu: &mut PPU, n_rom: &NROM, address: u16) -> u8 {
    match address {
        0x02 => {
            let data = (ppu.ppustatus & 0xE0) | (ppu.ppu_data_buffer & 0x1F);
            set_ppustatus_v(ppu, 0);
            ppu.w = 0;

            data
        }
        0x04 => ppu.primary_oam[ppu.oamaddr as usize],
        0x07 => {
            let mut data = ppu.ppu_data_buffer;
            ppu.ppu_data_buffer = read_ppu(ppu, n_rom, ppu.vram_address as usize);

            if ppu.vram_address >= 0x3F00 {
                data = ppu.ppu_data_buffer;
            }

            if (ppu.ppuctrl & 0b0000_0100) >> 2 != 0 {
                ppu.vram_address += 32
            } else {
                ppu.vram_address += 1
            }

            data
        }
        _ => panic!("invalid read to 0x{:02X}", address),
    }
}
