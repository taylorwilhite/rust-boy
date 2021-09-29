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
      memory.write_addr(index + 0x0100, byte.unwrap())
    }
    return Gb {
      cpu: Cpu::new(memory),
    };
  }

  pub fn run(&mut self) {
    loop {
      if self.cpu.mem.get_addr(0xff02 as usize) == 0x81 {
        let printed = self.cpu.mem.get_addr(0xff01 as usize);
        println!("{}", printed as char)
      }
      let opcode = self.cpu.mem.get_addr(self.cpu.pc as usize);
      let lsb = self.cpu.mem.get_addr((self.cpu.pc + 1) as usize);
      let msb = self.cpu.mem.get_addr((self.cpu.pc + 2) as usize);
      let nn: u16 = (lsb as u16) & ((msb as u16) << 8);
      if opcode != 0x00 {
        println!("{:02X?} {:02X?} {:02X?} {:04X?}", opcode, lsb, msb, nn);
      }
      self.cpu.execute(opcode)
    }
  }
}
