use crate::memory::MemoryBus;
pub struct Cpu {
  a: u8,
  f: u8,
  b: u8,
  c: u8,
  d: u8,
  e: u8,
  h: u8,
  l: u8,
  sp: u16,
  pc: u16,
  mem: MemoryBus
}

pub enum Reg16 {
  AF,
  BC,
  DE,
  HL
}

impl Cpu {
  pub fn new(mem: MemoryBus) -> Cpu {
    return Cpu {
      a: 0,
      b: 0,
      f: 0,
      c: 0,
      d: 0,
      e: 0,
      h: 0,
      l: 0,
      sp: 0,
      pc: 0,
      mem: mem
    }
  }

  fn read16reg(&mut self, reg: Reg16) -> u16 {
    use Reg16::*;
    match reg {
      AF => ((self.a as u16) << 8) | (self.f as u16),
      BC => ((self.b as u16) << 8) | (self.c as u16),
      DE => ((self.d as u16) << 8) | (self.e as u16),
      HL => ((self.h as u16) << 8) | (self.l as u16),
    }
  }

  fn write16reg(&mut self, reg: Reg16, value: u16) {
    use Reg16::*;
    match reg {
      AF => {
        self.a = (value >> 8) as u8;
        self.f = value as u8;
      },
      BC => {
        self.b = (value >> 8) as u8;
        self.c = value as u8;
      },
      DE => {
        self.d = (value >> 8) as u8;
        self.e = value as u8;
      },
      HL => {
        self.h = (value >> 8) as u8;
        self.l = value as u8;
      },
    }
  }

  pub fn execute(&mut self, opcode: u8) {
    use Reg16::*;
    let data = self.mem.get_addr(self.pc as usize);
    match opcode {
      0x00 => self.nop(),
      0x01 => self.ld_16(BC, data),
      _ => panic!("you need to handle opcode {}", opcode)
    }
  }

  fn nop(&mut self) {
    self.pc += 1;
  }
  
  fn ld_16(&mut self, reg: Reg16, value: u16) {
    self.pc += 1;
    self.write16reg(reg, value)
  }
}