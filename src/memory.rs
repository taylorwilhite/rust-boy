pub struct MemoryBus {
  ram: [u16; 2048]
}

impl MemoryBus {
  pub fn get_addr(&mut self, value: usize) -> u16 {
    self.ram[value]
  }
}