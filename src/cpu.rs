use crate::memory::MemoryBus;
use bitflags::bitflags;

bitflags!(
  pub struct Flags: u8 {
    const ZERO         = 0b_1000_0000;
    const ADD_SUBTRACT = 0b_0100_0000;
    const HALF_CARRY   = 0b_0010_0000;
    const CARRY        = 0b_0001_0000;
  }
);

pub struct Cpu {
  a: u8,
  f: Flags,
  b: u8,
  c: u8,
  d: u8,
  e: u8,
  h: u8,
  l: u8,
  sp: u16,
  pc: u16,
  mem: MemoryBus,
}

pub enum Reg8 {
  A,
  B,
  C,
  D,
  E,
  H,
  L,
}

pub enum Reg16 {
  AF,
  BC,
  DE,
  HL,
}

enum Cycle {
  ONE,
  TWO,
  THREE,
  FOUR,
  FIVE,
}

impl Cpu {
  pub fn new(mem: MemoryBus) -> Cpu {
    return Cpu {
      a: 0,
      b: 0,
      f: Flags::empty(),
      c: 0,
      d: 0,
      e: 0,
      h: 0,
      l: 0,
      sp: 0,
      pc: 0,
      mem: mem,
    };
  }

  fn read8reg(&mut self, reg: &Reg8) -> u8 {
    use Reg8::*;
    match reg {
      A => self.a,
      B => self.b,
      C => self.c,
      D => self.d,
      E => self.e,
      H => self.h,
      L => self.l,
    }
  }

  fn write8reg(&mut self, reg: &Reg8, value: u8) {
    use Reg8::*;
    match reg {
      A => self.a = value,
      B => self.b = value,
      C => self.c = value,
      D => self.d = value,
      E => self.e = value,
      H => self.h = value,
      L => self.l = value,
    }
  }

  fn read16reg(&mut self, reg: &Reg16) -> u16 {
    use Reg16::*;
    match reg {
      AF => ((self.a as u16) << 8) | (self.f.bits() as u16),
      BC => ((self.b as u16) << 8) | (self.c as u16),
      DE => ((self.d as u16) << 8) | (self.e as u16),
      HL => ((self.h as u16) << 8) | (self.l as u16),
    }
  }

  fn write16reg(&mut self, reg: &Reg16, value: u16) {
    use Reg16::*;
    match reg {
      AF => {
        self.a = (value >> 8) as u8;
        self.f = Flags::from_bits_truncate(value as u8);
      }
      BC => {
        self.b = (value >> 8) as u8;
        self.c = value as u8;
      }
      DE => {
        self.d = (value >> 8) as u8;
        self.e = value as u8;
      }
      HL => {
        self.h = (value >> 8) as u8;
        self.l = value as u8;
      }
    }
  }

  fn get_flag(&self, flag: Flags) -> bool {
    self.f.contains(flag)
  }

  fn set_zf(&mut self, set: bool) {
    self.f.set(Flags::ZERO, set);
  }
  fn set_nf(&mut self, set: bool) {
    self.f.set(Flags::ADD_SUBTRACT, set);
  }
  fn set_hf(&mut self, set: bool) {
    self.f.set(Flags::HALF_CARRY, set);
  }
  fn set_cf(&mut self, set: bool) {
    self.f.set(Flags::CARRY, set);
  }

  pub fn execute(&mut self, opcode: u8) {
    use Reg16::*;
    use Reg8::*;
    let lsb = self.mem.get_addr((self.pc + 1) as usize);
    let msb = self.mem.get_addr((self.pc + 2) as usize);
    let nn: u16 = (lsb as u16) & ((msb as u16) << 8);
    match opcode {
      0x00 => self.nop(),
      0x01 => self.ld_16(BC, nn),
      0x02 => self.ld_16_addr(BC, self.a),
      0x03 => self.inc_16(BC),
      0x04 => self.inc_8(B),
      0x05 => self.dec_8(B),
      0x06 => self.ld_8(B, lsb),
      0x07 => self.rlc(A),
      0x08 => self.ld_nn_sp(nn, self.sp),
      0x09 => self.add_16_rr(HL, BC),
      0x0A => self.ld_a_rr(BC),
      0x0B => self.dec_16(BC),
      0x0C => self.inc_8(C),
      0x0D => self.dec_8(C),
      0x0E => self.ld_8(C, lsb),
      0x0F => self.rrc(A),
      _ => panic!("you need to handle opcode {}", opcode),
    };
  }

  fn nop(&mut self) -> Cycle {
    self.pc += 1;
    Cycle::ONE
  }

  fn rlc(&mut self, reg: Reg8) -> Cycle {
    let value = self.read8reg(&reg);
    let carry = value & 0x80;
    let new_value = value.rotate_left(1);
    self.write8reg(&reg, new_value);
    let z = match reg {
      Reg8::A => false,
      _ => new_value == 0,
    };

    self.set_zf(z);
    self.set_nf(false);
    self.set_hf(false);
    self.set_cf(carry != 0);

    self.pc += match reg {
      Reg8::A => 1,
      _ => 2,
    };
    match reg {
      Reg8::A => Cycle::ONE,
      _ => Cycle::TWO,
    }
  }

  fn rrc(&mut self, reg: Reg8) -> Cycle {
    let value = self.read8reg(&reg);
    let carry = value & 0x01;
    let new_value = value.rotate_right(1);
    self.write8reg(&reg, new_value);
    let z = match reg {
      Reg8::A => false,
      _ => new_value == 0,
    };

    self.set_zf(z);
    self.set_nf(false);
    self.set_hf(false);
    self.set_cf(carry != 0);

    self.pc += match reg {
      Reg8::A => 1,
      _ => 2,
    };
    match reg {
      Reg8::A => Cycle::ONE,
      _ => Cycle::TWO,
    }
  }

  fn ld_8(&mut self, reg: Reg8, value: u8) -> Cycle {
    self.write8reg(&reg, value);
    self.pc += 2;
    Cycle::TWO
  }

  fn ld_16(&mut self, reg: Reg16, value: u16) -> Cycle {
    self.write16reg(&reg, value);
    self.pc += 3;
    Cycle::THREE
  }

  fn ld_8_reg(&mut self, reg: Reg8, value: u8) -> Cycle {
    self.write8reg(&reg, value);
    self.pc += 1;
    Cycle::ONE
  }

  fn ld_16_addr(&mut self, reg: Reg16, value: u8) -> Cycle {
    let addr = self.read16reg(&reg);
    self.mem.write_addr(addr as usize, value);
    self.pc += 2;
    Cycle::TWO
  }

  fn ld_nn_sp(&mut self, addr: u16, value: u16) -> Cycle {
    let low = value as u8;
    let high = (value >> 8) as u8;
    self.mem.write_addr(addr as usize, low);
    self.mem.write_addr((addr + 1) as usize, high);
    self.pc += 3;
    Cycle::FIVE
  }

  fn ld_a_rr(&mut self, reg: Reg16) -> Cycle {
    let addr = self.read16reg(&reg);
    let value = self.mem.get_addr(addr as usize);
    self.write8reg(&Reg8::A, value);
    self.pc += 1;
    Cycle::TWO
  }

  fn add_16_rr(&mut self, reg: Reg16, value_reg: Reg16) -> Cycle {
    let value = self.read16reg(&reg);
    let add_value = self.read16reg(&value_reg);
    let new_value = value.wrapping_add(add_value);
    self.write16reg(&reg, new_value);

    self.set_nf(false);
    // TODO: set carry flags here

    self.pc += 1;
    Cycle::TWO
  }

  fn inc_16(&mut self, reg: Reg16) -> Cycle {
    let num = self.read16reg(&reg);
    self.write16reg(&reg, num + 1);
    self.pc += 2;
    Cycle::TWO
  }

  fn inc_8(&mut self, reg: Reg8) -> Cycle {
    let value = self.read8reg(&reg);
    let new_value = value.wrapping_add(1);
    self.write8reg(&reg, new_value);

    self.set_zf(new_value == 0);
    self.set_nf(false);
    self.set_hf(value & 0xf == 0xf);

    self.pc += 1;
    Cycle::ONE
  }

  fn dec_16(&mut self, reg: Reg16) -> Cycle {
    let new_value = self.read16reg(&reg).wrapping_sub(1);
    self.write16reg(&reg, new_value);
    self.pc += 1;
    Cycle::TWO
  }

  fn dec_8(&mut self, reg: Reg8) -> Cycle {
    let value = self.read8reg(&reg);
    let new_value = value.wrapping_sub(1);
    self.write8reg(&reg, new_value);

    self.set_zf(new_value == 0);
    self.set_nf(true);
    self.set_hf(value & 0xf == 0);

    self.pc += 1;
    Cycle::ONE
  }
}

#[test]
fn nop_increments() {
  let mem = MemoryBus::new();
  let mut cpu = Cpu::new(mem);

  cpu.nop();

  assert_eq!(cpu.pc, 1)
}
