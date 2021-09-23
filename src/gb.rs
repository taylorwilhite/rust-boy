use crate::cpu::Cpu;
use crate::memory::MemoryBus;
use std::fs::File;
use std::io::Read;

pub struct Gb {
  cpu: Cpu,
}

impl Gb {
  pub fn new(f: File) -> Gb {
    let mut memory = MemoryBus::new();
    for (index, byte) in f.bytes().enumerate() {
      memory.write_addr(index, byte.unwrap())
    }
    return Gb {
      cpu: Cpu::new(memory),
    };
  }

  pub fn run(&mut self) {
    loop {
      let opcode = self.cpu.mem.get_addr(self.cpu.pc as usize);
      self.cpu.execute(opcode)
    }
  }
}
