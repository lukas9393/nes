use crate::cpu::{read, CPU};
use crate::ppu::PPU;
use crate::NROM;

pub fn abs(cpu: &mut CPU, ppu: &mut PPU, n_rom: &NROM, mem: &[u8]) -> (u16, usize) {
    let ll = read(cpu, ppu, &n_rom, mem, cpu.pc) as u16;
    let hh = read(cpu, ppu, &n_rom, mem, cpu.pc + 1) as u16;
    cpu.pc += 2;
    ((hh << 8) | ll, 4)
}

pub fn abs_x(cpu: &mut CPU, ppu: &mut PPU, n_rom: &NROM, mem: &[u8]) -> (u16, usize) {
    let (address, mut cycle) = abs(cpu, ppu, n_rom, mem);
    let address_with_x = address.wrapping_add(cpu.x as u16);
    if address_with_x & 0xFF00 != address & 0xFF00 {
        cycle += 1;
    }
    (address_with_x, cycle)
}

pub fn abs_y(cpu: &mut CPU, ppu: &mut PPU, n_rom: &NROM, mem: &[u8]) -> (u16, usize) {
    let (address, mut cycle) = abs(cpu, ppu, n_rom, mem);
    let address_with_y = address.wrapping_add(cpu.y as u16);
    if address_with_y & 0xFF00 != address & 0xFF00 {
        cycle += 1;
    }
    (address_with_y, cycle)
}

pub fn imm_rel(cpu: &mut CPU) -> (u16, usize) {
    cpu.pc += 1;
    (cpu.pc - 1, 2)
}

pub fn ind(cpu: &mut CPU, ppu: &mut PPU, n_rom: &NROM, mem: &[u8]) -> (u16, usize) {
    let ll = read(cpu, ppu, &n_rom, mem, cpu.pc) as u16;
    let hh = read(cpu, ppu, &n_rom, mem, cpu.pc + 1) as u16;
    cpu.pc += 2;
    let low_byte = read(cpu, ppu, &n_rom, mem, (hh << 8) | ll) as u16;
    let high_byte = read(cpu, ppu, &n_rom, mem, (hh << 8) | ((ll + 1) & 0xFF)) as u16;
    ((high_byte << 8) | low_byte, 5)
}

pub fn ind_y(cpu: &mut CPU, ppu: &mut PPU, n_rom: &NROM, mem: &[u8]) -> (u16, usize) {
    let zp_ll = read(cpu, ppu, n_rom, mem, cpu.pc);
    let zp_hh = zp_ll.wrapping_add(1);
    cpu.pc += 1;
    let ll = read(cpu, ppu, n_rom, mem, zp_ll as u16) as u16;
    let hh = read(cpu, ppu, n_rom, mem, zp_hh as u16) as u16;
    let address = ((hh) << 8) | ll;
    let address_with_y = address.wrapping_add(cpu.y as u16);
    if address & 0xFF00 != address_with_y & 0xFF00 {
        return (address_with_y, 6);
    }
    (address_with_y, 5)
}

pub fn x_ind(cpu: &mut CPU, ppu: &mut PPU, n_rom: &NROM, mem: &[u8]) -> (u16, usize) {
    let ll_with_x = read(cpu, ppu, &n_rom, mem, cpu.pc).wrapping_add(cpu.x) as u16;
    cpu.pc += 1;
    let low_byte = read(cpu, ppu, &n_rom, mem, ll_with_x) as u16;
    let high_byte = read(cpu, ppu, &n_rom, mem, (ll_with_x + 1) & 0xFF) as u16;
    ((high_byte << 8) | low_byte, 6)
}

pub fn zpg(cpu: &mut CPU, ppu: &mut PPU, n_rom: &NROM, mem: &[u8]) -> (u16, usize) {
    let address = read(cpu, ppu, n_rom, mem, cpu.pc) as u16;
    cpu.pc += 1;
    (address, 3)
}

pub fn zpg_x(cpu: &mut CPU, ppu: &mut PPU, n_rom: &NROM, mem: &[u8]) -> (u16, usize) {
    let (address, cycles) = zpg(cpu, ppu, n_rom, mem);
    ((address + (cpu.x as u16)) & 0xFF, cycles + 1)
}

pub fn zpg_y(cpu: &mut CPU, ppu: &mut PPU, n_rom: &NROM, mem: &[u8]) -> (u16, usize) {
    let (address, cycles) = zpg(cpu, ppu, n_rom, mem);
    ((address + (cpu.y as u16)) & 0xFF, cycles + 1)
}
