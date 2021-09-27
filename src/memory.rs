pub struct MemoryBus {
  ram: [u8; 65535],
}

impl MemoryBus {
  pub fn new() -> MemoryBus {
    return MemoryBus { ram: [0; 65535] };
  }
  pub fn get_addr(&mut self, addr: usize) -> u8 {
    self.ram[addr]
  }

  pub fn write_addr(&mut self, addr: usize, value: u8) {
    self.ram[addr] = value
  }

  pub fn get_word(&mut self, addr: usize) -> u16 {
    (self.ram[addr] as u16) | ((self.ram[addr + 1] as u16) << 8)
  }

  pub fn write_word(&mut self, addr: usize, value: u16) {
    self.write_addr(addr, (value & 0xFF) as u8);
    self.write_addr(addr + 1, (value >> 8) as u8)
  }
}
