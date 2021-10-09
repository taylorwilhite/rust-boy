use rust_boy::gb::Gb;
use std::env;
use std::fs::File;

fn main() {
  let args: Vec<String> = env::args().collect();
  let rom = &args[1];

  let f = File::open(rom).expect("couldn't open rom");
  let mut machine = Gb::new(f);
  machine.run();
}
