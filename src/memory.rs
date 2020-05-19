pub struct MemoryBus {
  ram: [u8; 65535]
}

impl MemoryBus {
  pub fn get_addr(&mut self, value: usize) -> u8 {
    self.ram[value]
  }
}