pub struct MemoryBus {
  ram: [u8; 65535]
}

impl MemoryBus {
  pub fn new() -> MemoryBus {
    return MemoryBus {
      ram: [0; 65535]
    }
  }
  pub fn get_addr(&mut self, addr: usize) -> u8 {
    self.ram[addr]
  }

  pub fn write_addr(&mut self, addr: usize, value: u8) {
    self.ram[addr] = value
  }
}