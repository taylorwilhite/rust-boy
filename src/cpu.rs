pub struct Cpu {
  a: u8,
  b: u8,
  c: u8,
  d: u8,
  e: u8,
  h: u8,
  l: u8,
  sp: u16,
  pc: u16
}

impl Cpu {
  pub fn new() -> Cpu {
    return Cpu {
      a: 0,
      b: 0,
      c: 0,
      d: 0,
      e: 0,
      h: 0,
      l: 0,
      sp: 0,
      pc: 0
    }
  }

  pub fn execute(&mut self, opcode: u8) {
    match opcode {
      0x00 => nop(),
      _ => panic!("you need to handle opcode {}", opcode)
    }
  }

  fn nop(&mut self) {
    self.pc += 1;
  }
}