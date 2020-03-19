use crate::cpu::{pull, push, read, write, CPU};
use crate::ppu::PPU;
use crate::NROM;

pub fn adc(
    cpu: &mut CPU,
    address: u16,
    ppu: &mut PPU,
    n_rom: &mut NROM,
    mem: &mut Vec<u8>,
) -> usize {
    let byte = read(cpu, ppu, n_rom, mem, address);
    let result = cpu.ac.wrapping_add(byte).wrapping_add(cpu.sr & 0b0000_0001);

    if result <= cpu.ac && (byte != 0 || cpu.sr & 0b0000_0001 != 0) {
        cpu.sr |= 0b0000_0001;
    } else {
        cpu.sr &= 0b1111_1110;
    }

    if (byte ^ result) & (cpu.ac ^ result) & 0b1000_0000 != 0 {
        cpu.sr |= 0b0100_0000;
    } else {
        cpu.sr &= 0b1011_1111;
    }

    if result == 0 {
        cpu.sr |= 0b0000_0010;
    } else {
        cpu.sr &= 0b1111_1101;
    }

    cpu.sr = (cpu.sr & 0b0111_1111) | (result & 0b1000_0000);

    cpu.ac = result;
    0
}

pub fn and(
    cpu: &mut CPU,
    address: u16,
    ppu: &mut PPU,
    n_rom: &mut NROM,
    mem: &mut Vec<u8>,
) -> usize {
    cpu.ac &= read(cpu, ppu, n_rom, mem, address);

    if cpu.ac == 0 {
        cpu.sr |= 0b0000_0010;
    } else {
        cpu.sr &= 0b1111_1101;
    }

    cpu.sr = (cpu.sr & 0b0111_1111) | (cpu.ac & 0b1000_0000);

    0
}

pub fn asl(
    cpu: &mut CPU,
    address: u16,
    ppu: &mut PPU,
    n_rom: &mut NROM,
    mem: &mut Vec<u8>,
) -> usize {
    let mut byte = read(cpu, ppu, n_rom, mem, address);
    if byte & 0b1000_0000 != 0 {
        cpu.sr |= 0b0000_0001;
    } else {
        cpu.sr &= 0b1111_1110;
    }
    byte <<= 1;

    if byte == 0 {
        cpu.sr |= 0b0000_0010;
    } else {
        cpu.sr &= 0b1111_1101;
    }

    cpu.sr = (cpu.sr & 0b0111_1111) | (byte & 0b1000_0000);

    write(cpu, ppu, mem, n_rom, address, byte);
    2
}

pub fn asl_a(cpu: &mut CPU) -> usize {
    if cpu.ac & 0b1000_0000 != 0 {
        cpu.sr |= 0b0000_0001;
    } else {
        cpu.sr &= 0b1111_1110;
    }
    cpu.ac <<= 1;

    if cpu.ac == 0 {
        cpu.sr |= 0b0000_0010;
    } else {
        cpu.sr &= 0b1111_1101;
    }

    cpu.sr = (cpu.sr & 0b0111_1111) | (cpu.ac & 0b1000_0000);
    2
}

pub fn bcc(
    cpu: &mut CPU,
    address: u16,
    ppu: &mut PPU,
    n_rom: &mut NROM,
    mem: &mut Vec<u8>,
) -> usize {
    let byte = read(cpu, ppu, n_rom, mem, address);
    branch(cpu, cpu.sr & 0b0000_0001 == 0, byte)
}

pub fn bcs(
    cpu: &mut CPU,
    address: u16,
    ppu: &mut PPU,
    n_rom: &mut NROM,
    mem: &mut Vec<u8>,
) -> usize {
    let byte = read(cpu, ppu, n_rom, mem, address);
    branch(cpu, cpu.sr & 0b0000_0001 != 0, byte)
}

pub fn beq(
    cpu: &mut CPU,
    address: u16,
    ppu: &mut PPU,
    n_rom: &mut NROM,
    mem: &mut Vec<u8>,
) -> usize {
    let byte = read(cpu, ppu, n_rom, mem, address);
    branch(cpu, cpu.sr & 0b0000_0010 != 0, byte)
}

pub fn bit(
    cpu: &mut CPU,
    address: u16,
    ppu: &mut PPU,
    n_rom: &mut NROM,
    mem: &mut Vec<u8>,
) -> usize {
    let byte = read(cpu, ppu, n_rom, mem, address);
    cpu.sr &= 0b0011_1111;
    cpu.sr += byte & 0b1100_0000;

    if byte & cpu.ac == 0 {
        cpu.sr |= 0b0000_0010;
    } else {
        cpu.sr &= 0b1111_1101;
    }
    0
}

pub fn bmi(
    cpu: &mut CPU,
    address: u16,
    ppu: &mut PPU,
    n_rom: &mut NROM,
    mem: &mut Vec<u8>,
) -> usize {
    let byte = read(cpu, ppu, n_rom, mem, address);
    branch(cpu, cpu.sr & 0b1000_0000 != 0, byte)
}

pub fn bne(
    cpu: &mut CPU,
    address: u16,
    ppu: &mut PPU,
    n_rom: &mut NROM,
    mem: &mut Vec<u8>,
) -> usize {
    let byte = read(cpu, ppu, n_rom, mem, address);
    branch(cpu, cpu.sr & 0b0000_0010 == 0, byte)
}

pub fn bpl(
    cpu: &mut CPU,
    address: u16,
    ppu: &mut PPU,
    n_rom: &mut NROM,
    mem: &mut Vec<u8>,
) -> usize {
    let byte = read(cpu, ppu, n_rom, mem, address);
    branch(cpu, cpu.sr & 0b1000_0000 == 0, byte)
}

pub fn brk(cpu: &mut CPU, ppu: &mut PPU, n_rom: &mut NROM, mem: &mut Vec<u8>) -> usize {
    cpu.sr |= 0b0000_0100;
    let pc = cpu.pc + 2;
    push(cpu, ppu, mem, n_rom, (pc >> 8) as u8);
    push(cpu, ppu, mem, n_rom, pc as u8);
    push(cpu, ppu, mem, n_rom, cpu.sr);
    7
}

pub fn bvc(
    cpu: &mut CPU,
    address: u16,
    ppu: &mut PPU,
    n_rom: &mut NROM,
    mem: &mut Vec<u8>,
) -> usize {
    let byte = read(cpu, ppu, n_rom, mem, address);
    branch(cpu, cpu.sr & 0b0100_0000 == 0, byte)
}

pub fn bvs(
    cpu: &mut CPU,
    address: u16,
    ppu: &mut PPU,
    n_rom: &mut NROM,
    mem: &mut Vec<u8>,
) -> usize {
    let byte = read(cpu, ppu, n_rom, mem, address);
    branch(cpu, cpu.sr & 0b0100_0000 != 0, byte)
}

pub fn clc(cpu: &mut CPU) -> usize {
    cpu.sr &= 0b1111_1110;
    2
}

pub fn cld(cpu: &mut CPU) -> usize {
    cpu.sr &= 0b1111_0111;
    2
}

pub fn cli(cpu: &mut CPU) -> usize {
    cpu.sr &= 0b1111_1011;
    2
}

pub fn clv(cpu: &mut CPU) -> usize {
    cpu.sr &= 0b1011_1111;
    2
}

pub fn cmp(
    cpu: &mut CPU,
    address: u16,
    ppu: &mut PPU,
    n_rom: &mut NROM,
    mem: &mut Vec<u8>,
) -> usize {
    let byte = read(cpu, ppu, n_rom, mem, address);
    compare(cpu, byte, cpu.ac);
    0
}

pub fn cpx(
    cpu: &mut CPU,
    address: u16,
    ppu: &mut PPU,
    n_rom: &mut NROM,
    mem: &mut Vec<u8>,
) -> usize {
    let byte = read(cpu, ppu, n_rom, mem, address);
    compare(cpu, byte, cpu.x);
    0
}

pub fn cpy(
    cpu: &mut CPU,
    address: u16,
    ppu: &mut PPU,
    n_rom: &mut NROM,
    mem: &mut Vec<u8>,
) -> usize {
    let byte = read(cpu, ppu, n_rom, mem, address);
    compare(cpu, byte, cpu.y);
    0
}

pub fn dcp(
    cpu: &mut CPU,
    address: u16,
    ppu: &mut PPU,
    n_rom: &mut NROM,
    mem: &mut Vec<u8>,
) -> usize {
    let byte = read(cpu, ppu, n_rom, mem, address).wrapping_sub(1);
    write(cpu, ppu, mem, n_rom, address, byte);
    compare(cpu, byte, cpu.ac);
    6
}

pub fn dec(
    cpu: &mut CPU,
    address: u16,
    ppu: &mut PPU,
    n_rom: &mut NROM,
    mem: &mut Vec<u8>,
) -> usize {
    let mut byte = read(cpu, ppu, n_rom, mem, address);
    byte = byte.wrapping_sub(1);
    write(cpu, ppu, mem, n_rom, address, byte);
    if byte == 0 {
        cpu.sr |= 0b0000_0010;
    } else {
        cpu.sr &= 0b1111_1101;
    };
    cpu.sr = (cpu.sr & 0b0111_1111) | (byte & 0b1000_0000);
    2
}

pub fn dex(cpu: &mut CPU) -> usize {
    cpu.x = cpu.x.wrapping_sub(1);
    if cpu.x == 0 {
        cpu.sr |= 0b0000_0010;
    } else {
        cpu.sr &= 0b1111_1101;
    };
    cpu.sr = (cpu.sr & 0b0111_1111) | (cpu.x & 0b1000_0000);
    2
}

pub fn dey(cpu: &mut CPU) -> usize {
    cpu.y = cpu.y.wrapping_sub(1);
    if cpu.y == 0 {
        cpu.sr |= 0b0000_0010;
    } else {
        cpu.sr &= 0b1111_1101;
    };
    cpu.sr = (cpu.sr & 0b0111_1111) | (cpu.y & 0b1000_0000);
    2
}

pub fn eor(
    cpu: &mut CPU,
    address: u16,
    ppu: &mut PPU,
    n_rom: &mut NROM,
    mem: &mut Vec<u8>,
) -> usize {
    cpu.ac ^= read(cpu, ppu, n_rom, mem, address);
    if cpu.ac == 0 {
        cpu.sr |= 0b0000_0010;
    } else {
        cpu.sr &= 0b1111_1101;
    };
    cpu.sr = (cpu.sr & 0b0111_1111) | (cpu.ac & 0b1000_0000);
    0
}

pub fn ign(
    cpu: &mut CPU,
    address: u16,
    ppu: &mut PPU,
    n_rom: &mut NROM,
    mem: &mut Vec<u8>,
) -> usize {
    read(cpu, ppu, n_rom, mem, address);
    0
}

pub fn inc(
    cpu: &mut CPU,
    address: u16,
    ppu: &mut PPU,
    n_rom: &mut NROM,
    mem: &mut Vec<u8>,
) -> usize {
    let mut byte = read(cpu, ppu, n_rom, mem, address);
    byte = byte.wrapping_add(1);
    write(cpu, ppu, mem, n_rom, address, byte);
    if byte == 0 {
        cpu.sr |= 0b0000_0010;
    } else {
        cpu.sr &= 0b1111_1101;
    }
    cpu.sr = (cpu.sr & 0b0111_1111) | (byte & 0b1000_0000);
    2
}

pub fn inx(cpu: &mut CPU) -> usize {
    cpu.x = cpu.x.wrapping_add(1);
    if cpu.x == 0 {
        cpu.sr |= 0b0000_0010;
    } else {
        cpu.sr &= 0b1111_1101;
    }
    cpu.sr = (cpu.sr & 0b0111_1111) | (cpu.x & 0b1000_0000);
    2
}

pub fn iny(cpu: &mut CPU) -> usize {
    cpu.y = cpu.y.wrapping_add(1);
    if cpu.y == 0 {
        cpu.sr |= 0b0000_0010;
    } else {
        cpu.sr &= 0b1111_1101;
    };
    cpu.sr = (cpu.sr & 0b0111_1111) | (cpu.y & 0b1000_0000);
    2
}

pub fn isc(
    cpu: &mut CPU,
    address: u16,
    ppu: &mut PPU,
    n_rom: &mut NROM,
    mem: &mut Vec<u8>,
) -> usize {
    let cycles = inc(cpu, address, ppu, n_rom, mem);
    cycles + sbc(cpu, address, ppu, n_rom, mem)
}

pub fn jmp(cpu: &mut CPU, address: u16) -> usize {
    cpu.pc = address;
    0
}

pub fn jsr(
    cpu: &mut CPU,
    address: u16,
    ppu: &mut PPU,
    n_rom: &mut NROM,
    mem: &mut Vec<u8>,
) -> usize {
    let pc = cpu.pc - 1;
    push(cpu, ppu, mem, n_rom, (pc >> 8) as u8);
    push(cpu, ppu, mem, n_rom, pc as u8);
    cpu.pc = address;
    2
}

pub fn lax(
    cpu: &mut CPU,
    address: u16,
    ppu: &mut PPU,
    n_rom: &mut NROM,
    mem: &mut Vec<u8>,
) -> usize {
    let byte = read(cpu, ppu, n_rom, mem, address);
    cpu.ac = byte;
    cpu.x = byte;
    if byte == 0 {
        cpu.sr |= 0b0000_0010;
    } else {
        cpu.sr &= 0b1111_1101;
    }
    cpu.sr = (cpu.sr & 0b0111_1111) | (byte & 0b1000_0000);
    2
}

pub fn lda(
    cpu: &mut CPU,
    address: u16,
    ppu: &mut PPU,
    n_rom: &mut NROM,
    mem: &mut Vec<u8>,
) -> usize {
    cpu.ac = read(cpu, ppu, n_rom, mem, address);
    if cpu.ac == 0 {
        cpu.sr |= 0b0000_0010;
    } else {
        cpu.sr &= 0b1111_1101;
    }
    cpu.sr = (cpu.sr & 0b0111_1111) | (cpu.ac & 0b1000_0000);
    0
}

pub fn ldx(
    cpu: &mut CPU,
    address: u16,
    ppu: &mut PPU,
    n_rom: &mut NROM,
    mem: &mut Vec<u8>,
) -> usize {
    cpu.x = read(cpu, ppu, n_rom, mem, address);
    if cpu.x == 0 {
        cpu.sr |= 0b0000_0010;
    } else {
        cpu.sr &= 0b1111_1101;
    }
    cpu.sr = (cpu.sr & 0b0111_1111) | (cpu.x & 0b1000_0000);
    0
}

pub fn ldy(
    cpu: &mut CPU,
    address: u16,
    ppu: &mut PPU,
    n_rom: &mut NROM,
    mem: &mut Vec<u8>,
) -> usize {
    cpu.y = read(cpu, ppu, n_rom, mem, address);
    if cpu.y == 0 {
        cpu.sr |= 0b0000_0010;
    } else {
        cpu.sr &= 0b1111_1101;
    }
    cpu.sr = (cpu.sr & 0b0111_1111) | (cpu.y & 0b1000_0000);
    0
}

pub fn lsr(
    cpu: &mut CPU,
    address: u16,
    ppu: &mut PPU,
    n_rom: &mut NROM,
    mem: &mut Vec<u8>,
) -> usize {
    let mut byte = read(cpu, ppu, n_rom, mem, address);

    if byte & 0b0000_0001 != 0 {
        cpu.sr |= 0b0000_0001;
    } else {
        cpu.sr &= 0b1111_1110;
    }
    byte >>= 1;

    write(cpu, ppu, mem, n_rom, address, byte);

    if byte == 0 {
        cpu.sr |= 0b0000_0010;
    } else {
        cpu.sr &= 0b1111_1101;
    }
    cpu.sr = (cpu.sr & 0b0111_1111) | (byte & 0b1000_0000);
    2
}

pub fn lsr_a(cpu: &mut CPU) -> usize {
    if cpu.ac & 0b0000_0001 != 0 {
        cpu.sr |= 0b0000_0001;
    } else {
        cpu.sr &= 0b1111_1110;
    }
    cpu.ac >>= 1;

    if cpu.ac == 0 {
        cpu.sr |= 0b0000_0010;
    } else {
        cpu.sr &= 0b1111_1101;
    }
    cpu.sr = (cpu.sr & 0b0111_1111) | (cpu.ac & 0b1000_0000);
    2
}

pub fn nop() -> usize {
    2
}

pub fn ora(
    cpu: &mut CPU,
    address: u16,
    ppu: &mut PPU,
    n_rom: &mut NROM,
    mem: &mut Vec<u8>,
) -> usize {
    cpu.ac |= read(cpu, ppu, n_rom, mem, address);
    if cpu.ac == 0 {
        cpu.sr |= 0b0000_0010;
    } else {
        cpu.sr &= 0b1111_1101;
    }
    cpu.sr = (cpu.sr & 0b0111_1111) | (cpu.ac & 0b1000_0000);
    0
}

pub fn pha(cpu: &mut CPU, ppu: &mut PPU, n_rom: &mut NROM, mem: &mut Vec<u8>) -> usize {
    push(cpu, ppu, mem, n_rom, cpu.ac);
    3
}

pub fn php(cpu: &mut CPU, ppu: &mut PPU, n_rom: &mut NROM, mem: &mut Vec<u8>) -> usize {
    push(cpu, ppu, mem, n_rom, cpu.sr);
    3
}

pub fn pla(cpu: &mut CPU, ppu: &mut PPU, n_rom: &mut NROM, mem: &mut Vec<u8>) -> usize {
    cpu.ac = pull(cpu, ppu, mem, n_rom);
    if cpu.ac == 0 {
        cpu.sr |= 0b0000_0010;
    } else {
        cpu.sr &= 0b1111_1101;
    }
    cpu.sr = (cpu.sr & 0b0111_1111) | (cpu.ac & 0b1000_0000);
    4
}

pub fn plp(cpu: &mut CPU, ppu: &mut PPU, n_rom: &mut NROM, mem: &mut Vec<u8>) -> usize {
    cpu.sr = pull(cpu, ppu, mem, n_rom);
    4
}

pub fn rla(
    cpu: &mut CPU,
    address: u16,
    ppu: &mut PPU,
    n_rom: &mut NROM,
    mem: &mut Vec<u8>,
) -> usize {
    let cycles = rol(cpu, address, ppu, n_rom, mem);
    cycles + and(cpu, address, ppu, n_rom, mem)
}

pub fn rol(
    cpu: &mut CPU,
    address: u16,
    ppu: &mut PPU,
    n_rom: &mut NROM,
    mem: &mut Vec<u8>,
) -> usize {
    let mut byte = read(cpu, ppu, n_rom, mem, address);
    let new_cfb = byte & 0b1000_0000 != 0;
    byte <<= 1;
    byte += cpu.sr & 0b0000_0001;

    write(cpu, ppu, mem, n_rom, address, byte);

    if new_cfb {
        cpu.sr |= 0b0000_0001;
    } else {
        cpu.sr &= 0b1111_1110;
    }

    if byte == 0 {
        cpu.sr |= 0b0000_0010;
    } else {
        cpu.sr &= 0b1111_1101;
    }
    cpu.sr = (cpu.sr & 0b0111_1111) | (byte & 0b1000_0000);
    2
}

pub fn rol_a(cpu: &mut CPU) -> usize {
    let new_cfb = cpu.ac & 0b1000_0000 != 0;
    cpu.ac <<= 1;
    cpu.ac += cpu.sr & 0b0000_0001;

    if new_cfb {
        cpu.sr |= 0b0000_0001;
    } else {
        cpu.sr &= 0b1111_1110;
    }

    if cpu.ac == 0 {
        cpu.sr |= 0b0000_0010;
    } else {
        cpu.sr &= 0b1111_1101;
    }
    cpu.sr = (cpu.sr & 0b0111_1111) | (cpu.ac & 0b1000_0000);
    2
}

pub fn ror(
    cpu: &mut CPU,
    address: u16,
    ppu: &mut PPU,
    n_rom: &mut NROM,
    mem: &mut Vec<u8>,
) -> usize {
    let mut byte = read(cpu, ppu, n_rom, mem, address);
    let new_cfb = byte & 0b0000_0001 != 0;
    byte >>= 1;
    byte += (cpu.sr & 0b0000_0001) * 0x80;

    write(cpu, ppu, mem, n_rom, address, byte);

    if new_cfb {
        cpu.sr |= 0b0000_0001;
    } else {
        cpu.sr &= 0b1111_1110;
    }

    if byte == 0 {
        cpu.sr |= 0b0000_0010;
    } else {
        cpu.sr &= 0b1111_1101;
    }
    cpu.sr = (cpu.sr & 0b0111_1111) | (byte & 0b1000_0000);
    2
}

pub fn ror_a(cpu: &mut CPU) -> usize {
    let new_cfb = cpu.ac & 0b0000_0001 != 0;
    cpu.ac >>= 1;
    cpu.ac += (cpu.sr & 0b0000_0001) * 0x80;

    if new_cfb {
        cpu.sr |= 0b0000_0001;
    } else {
        cpu.sr &= 0b1111_1110;
    }

    if cpu.ac == 0 {
        cpu.sr |= 0b0000_0010;
    } else {
        cpu.sr &= 0b1111_1101;
    }
    cpu.sr = (cpu.sr & 0b0111_1111) | (cpu.ac & 0b1000_0000);
    2
}

pub fn rra(
    cpu: &mut CPU,
    address: u16,
    ppu: &mut PPU,
    n_rom: &mut NROM,
    mem: &mut Vec<u8>,
) -> usize {
    let cycles = ror(cpu, address, ppu, n_rom, mem);
    cycles + adc(cpu, address, ppu, n_rom, mem)
}

pub fn rti(cpu: &mut CPU, ppu: &mut PPU, n_rom: &mut NROM, mem: &mut Vec<u8>) -> usize {
    cpu.sr = pull(cpu, ppu, mem, n_rom);
    cpu.pc = pull(cpu, ppu, mem, n_rom) as u16;
    cpu.pc |= (pull(cpu, ppu, mem, n_rom) as u16) << 8;
    6
}

pub fn rts(cpu: &mut CPU, ppu: &mut PPU, n_rom: &mut NROM, mem: &mut Vec<u8>) -> usize {
    cpu.pc = pull(cpu, ppu, mem, n_rom) as u16;
    cpu.pc |= (pull(cpu, ppu, mem, n_rom) as u16) << 8;
    cpu.pc += 1;
    6
}

pub fn sax(
    cpu: &mut CPU,
    address: u16,
    ppu: &mut PPU,
    n_rom: &mut NROM,
    mem: &mut Vec<u8>,
) -> usize {
    write(cpu, ppu, mem, n_rom, address, cpu.ac & cpu.x);
    3
}

pub fn sbc(
    cpu: &mut CPU,
    address: u16,
    ppu: &mut PPU,
    n_rom: &mut NROM,
    mem: &mut Vec<u8>,
) -> usize {
    let byte = read(cpu, ppu, n_rom, mem, address);
    let new_val = cpu
        .ac
        .wrapping_sub(byte)
        .wrapping_sub(!cpu.sr & 0b0000_0001);

    if new_val >= cpu.ac && (byte != 0 || !cpu.sr & 0b0000_0001 != 0) {
        cpu.sr &= 0b1111_1110;
    } else {
        cpu.sr |= 0b0000_0001;
    }

    let acc = cpu.ac & 0x80 == 0;
    let mem = byte & 0x80 == 0;
    let res = new_val & 0x80 == 0;
    if (acc && !mem && !res) || (!acc && mem && res) {
        cpu.sr |= 0b0100_0000;
    } else {
        cpu.sr &= 0b1011_1111;
    }

    cpu.ac = new_val;
    if cpu.ac == 0 {
        cpu.sr |= 0b0000_0010;
    } else {
        cpu.sr &= 0b1111_1101;
    }
    cpu.sr = (cpu.sr & 0b0111_1111) | (cpu.ac & 0b1000_0000);
    0
}

pub fn sec(cpu: &mut CPU) -> usize {
    cpu.sr |= 0b0000_0001;
    2
}

pub fn sed(cpu: &mut CPU) -> usize {
    cpu.sr |= 0b0000_1000;
    2
}

pub fn sei(cpu: &mut CPU) -> usize {
    cpu.sr |= 0b0000_0100;
    2
}

pub fn slo(
    cpu: &mut CPU,
    address: u16,
    ppu: &mut PPU,
    n_rom: &mut NROM,
    mem: &mut Vec<u8>,
) -> usize {
    let cycles = asl(cpu, address, ppu, n_rom, mem);
    cycles + ora(cpu, address, ppu, n_rom, mem)
}

pub fn sre(
    cpu: &mut CPU,
    address: u16,
    ppu: &mut PPU,
    n_rom: &mut NROM,
    mem: &mut Vec<u8>,
) -> usize {
    let cycles = lsr(cpu, address, ppu, n_rom, mem);
    cycles + eor(cpu, address, ppu, n_rom, mem)
}

pub fn sta(
    cpu: &mut CPU,
    address: u16,
    ppu: &mut PPU,
    n_rom: &mut NROM,
    mem: &mut Vec<u8>,
) -> usize {
    write(cpu, ppu, mem, n_rom, address, cpu.ac);

    0
}

pub fn stx(
    cpu: &mut CPU,
    address: u16,
    ppu: &mut PPU,
    n_rom: &mut NROM,
    mem: &mut Vec<u8>,
) -> usize {
    write(cpu, ppu, mem, n_rom, address, cpu.x);

    0
}

pub fn sty(
    cpu: &mut CPU,
    address: u16,
    ppu: &mut PPU,
    n_rom: &mut NROM,
    mem: &mut Vec<u8>,
) -> usize {
    write(cpu, ppu, mem, n_rom, address, cpu.y);

    0
}

pub fn tax(cpu: &mut CPU) -> usize {
    cpu.x = cpu.ac;

    2
}

pub fn tay(cpu: &mut CPU) -> usize {
    cpu.y = cpu.ac;

    2
}

pub fn tsx(cpu: &mut CPU) -> usize {
    cpu.x = cpu.sp;
    if cpu.x == 0 {
        cpu.sr |= 0b0000_0010;
    } else {
        cpu.sr &= 0b1111_1101;
    }
    cpu.sr = (cpu.sr & 0b0111_1111) | (cpu.x & 0b1000_0000);

    2
}

pub fn txa(cpu: &mut CPU) -> usize {
    cpu.ac = cpu.x;
    if cpu.ac == 0 {
        cpu.sr |= 0b0000_0010;
    } else {
        cpu.sr &= 0b1111_1101;
    }
    cpu.sr = (cpu.sr & 0b0111_1111) | (cpu.ac & 0b1000_0000);

    2
}

pub fn txs(cpu: &mut CPU) -> usize {
    cpu.sp = cpu.x;

    2
}

pub fn tya(cpu: &mut CPU) -> usize {
    cpu.ac = cpu.y;

    2
}

fn branch(cpu: &mut CPU, flag: bool, byte: u8) -> usize {
    let old_pc = cpu.pc;
    let mut cycles = 0;
    if flag {
        if byte as i8 >= 0 {
            cpu.pc += byte as u16;
        } else {
            cpu.pc -= (-(byte as i8)) as u16;
        }
        cycles += 1;
    }
    if old_pc & 0xFF00 != cpu.pc & 0xFF00 {
        cycles += 1;
    }
    cycles
}

fn compare(cpu: &mut CPU, byte: u8, reg: u8) {
    if reg >= byte {
        cpu.sr |= 0b0000_0001;
    } else {
        cpu.sr &= 0b1111_1110;
    }

    let diff = reg.wrapping_sub(byte);
    if diff == 0 {
        cpu.sr |= 0b0000_0010;
    } else {
        cpu.sr &= 0b1111_1101;
    }
    cpu.sr = (cpu.sr & 0b0111_1111) | (diff & 0b1000_0000);
}
