use crate::cpu::Cpu;
use crate::memory::MemoryBus;
use std::fs::File;
use std::io::{Read, Write};

pub struct Gb {
  cpu: Cpu,
}

impl Gb {
  pub fn new(f: File) -> Gb {
    let mut memory = MemoryBus::new();
    let boot_rom = File::open("./roms/dmg_boot.bin").expect("couldn't open bootrom");
    for (index, byte) in boot_rom.bytes().enumerate() {
      memory.write_addr(index, byte.unwrap())
    }
    for (index, byte) in f.bytes().enumerate() {
      memory.write_addr(index + 0x0100, byte.unwrap())
    }
    return Gb {
      cpu: Cpu::new(memory),
    };
  }

  pub fn run(&mut self) {
    let mut output = File::create("./blarggsoutput.txt").expect("could not create file");
    loop {
      if self.cpu.pc == 0x100 {
        break;
      }
      let opcode = self.cpu.mem.get_addr(self.cpu.pc as usize);
      let lsb = self.cpu.mem.get_addr((self.cpu.pc + 1) as usize);
      let msb = self.cpu.mem.get_addr((self.cpu.pc + 2) as usize);
      let third = self.cpu.mem.get_addr((self.cpu.pc + 3) as usize);
      // let nn: u16 = (lsb as u16) | ((msb as u16) << 8);
      writeln!(
        output,
        "{:02X?} {:02X?} {:02X?} {:02X?} PC: {:04X?}",
        opcode, lsb, msb, third, self.cpu.pc
      );
      self.cpu.step();
    }
  }
}
