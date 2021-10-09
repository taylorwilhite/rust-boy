pub struct MemoryBus {
  ram: Vec<u8>,
  boot_enabled: bool,
  boot_rom: Vec<u8>,
}

impl MemoryBus {
  pub fn new(boot_rom: Vec<u8>, cart: Vec<u8>) -> MemoryBus {
    let mut ram: Vec<u8> = vec![0; 65536];
    let mut boot: Vec<u8> = Vec::with_capacity(256);
    ram.splice(0..cart.len(), cart);
    boot.extend(boot_rom.iter());
    return MemoryBus {
      ram: ram,
      boot_enabled: true,
      boot_rom: boot,
    };
  }
  pub fn get_addr(&mut self, addr: usize) -> u8 {
    // Remove after Testing
    if addr == 0xff44 {
      return 0x90;
    }
    if self.boot_enabled && addr < 0x100 {
      self.boot_rom[addr]
    } else {
      self.ram[addr]
    }
  }

  pub fn write_addr(&mut self, addr: usize, value: u8) {
    if addr == 0xff02 {
      let printed = self.get_addr(0xff01 as usize);
      print!("{}", printed as char);
    }
    if addr == 0xff50 {
      self.boot_enabled = false;
      return;
    }
    // Remove after testing
    if addr == 0xff44 {
      return;
    }
    if self.boot_enabled && addr < 0x100 {
      self.boot_rom[addr] = value;
    } else {
      self.ram[addr] = value;
    }
  }

  pub fn get_word(&mut self, addr: usize) -> u16 {
    if self.boot_enabled && addr < 0x100 {
      (self.boot_rom[addr] as u16) | ((self.boot_rom[addr + 1] as u16) << 8)
    } else {
      (self.ram[addr] as u16) | ((self.ram[addr + 1] as u16) << 8)
    }
  }

  pub fn write_word(&mut self, addr: usize, value: u16) {
    self.write_addr(addr, (value & 0xFF) as u8);
    self.write_addr(addr + 1, (value >> 8) as u8)
  }
}
