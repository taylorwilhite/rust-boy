use crate::cpu::Cpu;
use crate::memory::MemoryBus;
use std::fs::File;
use std::io::{Read, Write};

pub struct Gb {
  cpu: Cpu,
}

impl Gb {
  pub fn new(f: File) -> Gb {
    let boot_rom = File::open("./roms/dmg_boot.bin").expect("couldn't open bootrom");
    let boot_rom: Vec<u8> = boot_rom
      .bytes()
      .collect::<Result<Vec<u8>, std::io::Error>>()
      .unwrap();
    let cart = f
      .bytes()
      .collect::<Result<Vec<u8>, std::io::Error>>()
      .unwrap();
    let memory = MemoryBus::new(boot_rom, cart);

    return Gb {
      cpu: Cpu::new(memory),
    };
  }

  pub fn run(&mut self) {
    let mut print_logs = false;
    let mut output = File::create("./debuglogs/blarggsoutput.txt").expect("could not create file");
    loop {
      if self.cpu.pc == 0x100 {
        print_logs = true;
      }
      let opcode = self.cpu.mem.get_addr(self.cpu.pc as usize);
      let lsb = self.cpu.mem.get_addr((self.cpu.pc + 1) as usize);
      let msb = self.cpu.mem.get_addr((self.cpu.pc + 2) as usize);
      let third = self.cpu.mem.get_addr((self.cpu.pc + 3) as usize);
      // let nn: u16 = (lsb as u16) | ((msb as u16) << 8);
      if print_logs {
        writeln!(
          output,
          "A: {:02X?} F: {:02X?} B: {:02X?} C: {:02X?} D: {:02X?} E: {:02X?} H: {:02X?} L: {:02X?} SP: {:04X?} PC: 00:{:04X?} ({:02X?} {:02X?} {:02X?} {:02X?})",
          self.cpu.a, self.cpu.f.bits() as u8, self.cpu.b, self.cpu.c, self.cpu.d, self.cpu.e, self.cpu.h, self.cpu.l, self.cpu.sp, self.cpu.pc, opcode, lsb, msb, third
        ).expect("error writing to file");
      }
      self.cpu.step();
    }
  }
}
