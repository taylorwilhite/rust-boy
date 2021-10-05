use crate::memory::MemoryBus;
use bitflags::bitflags;

bitflags!(
  pub struct InterruptFlags: u8 {
    const VBLANK = 1 << 0;
    const STAT = 1 << 1;
    const TIMER = 1 << 2;
    const SERIAL = 1 << 3;
    const JOYPAD = 1 << 4;
  }
);

bitflags!(
  pub struct Flags: u8 {
    const ZERO         = 0b_1000_0000;
    const ADD_SUBTRACT = 0b_0100_0000;
    const HALF_CARRY   = 0b_0010_0000;
    const CARRY        = 0b_0001_0000;
  }
);

pub struct Cpu {
  a: u8,
  f: Flags,
  b: u8,
  c: u8,
  d: u8,
  e: u8,
  h: u8,
  l: u8,
  sp: u16,
  pub pc: u16,
  pub mem: MemoryBus,
  ime: bool,
  halted: bool,
}

pub enum Reg8 {
  A,
  B,
  C,
  D,
  E,
  H,
  L,
}

pub enum Reg16 {
  AF,
  BC,
  DE,
  HL,
}

pub enum Cycle {
  ONE,
  TWO,
  THREE,
  FOUR,
  FIVE,
  SIX,
}

impl Cpu {
  pub fn new(mem: MemoryBus) -> Cpu {
    return Cpu {
      a: 0,
      b: 0,
      f: Flags::empty(),
      c: 0,
      d: 0,
      e: 0,
      h: 0,
      l: 0,
      sp: 0,
      pc: 0,
      mem: mem,
      ime: false,
      halted: false,
    };
  }

  fn read8reg(&mut self, reg: &Reg8) -> u8 {
    use Reg8::*;
    match reg {
      A => self.a,
      B => self.b,
      C => self.c,
      D => self.d,
      E => self.e,
      H => self.h,
      L => self.l,
    }
  }

  fn write8reg(&mut self, reg: &Reg8, value: u8) {
    use Reg8::*;
    match reg {
      A => self.a = value,
      B => self.b = value,
      C => self.c = value,
      D => self.d = value,
      E => self.e = value,
      H => self.h = value,
      L => self.l = value,
    }
  }

  fn read16reg(&mut self, reg: &Reg16) -> u16 {
    use Reg16::*;
    match reg {
      AF => ((self.a as u16) << 8) | (self.f.bits() as u16),
      BC => ((self.b as u16) << 8) | (self.c as u16),
      DE => ((self.d as u16) << 8) | (self.e as u16),
      HL => ((self.h as u16) << 8) | (self.l as u16),
    }
  }

  fn write16reg(&mut self, reg: &Reg16, value: u16) {
    use Reg16::*;
    match reg {
      AF => {
        self.a = (value >> 8) as u8;
        self.f = Flags::from_bits_truncate(value as u8);
      }
      BC => {
        self.b = (value >> 8) as u8;
        self.c = value as u8;
      }
      DE => {
        self.d = (value >> 8) as u8;
        self.e = value as u8;
      }
      HL => {
        self.h = (value >> 8) as u8;
        self.l = value as u8;
      }
    }
  }

  fn get_flag(&self, flag: Flags) -> bool {
    self.f.contains(flag)
  }

  fn set_zf(&mut self, set: bool) {
    self.f.set(Flags::ZERO, set);
  }
  fn set_nf(&mut self, set: bool) {
    self.f.set(Flags::ADD_SUBTRACT, set);
  }
  fn set_hf(&mut self, set: bool) {
    self.f.set(Flags::HALF_CARRY, set);
  }
  fn set_cf(&mut self, set: bool) {
    self.f.set(Flags::CARRY, set);
  }

  pub fn step(&mut self) -> Cycle {
    let opcode = self.mem.get_addr(self.pc as usize);
    let step_cycle = self.execute(opcode);
    self.check_interrupt();
    step_cycle
  }

  fn fire_interrupt(&mut self, addr: u16) {
    self.ime = false;
    self.sp = self.sp.wrapping_sub(2);
    self.mem.write_word(self.sp as usize, self.pc);
    self.pc = addr;
  }

  fn check_interrupt(&mut self) {
    if self.ime {
      let triggered_interrupts = self.mem.get_addr(0xff0f);
      let enabled_interrupts = self.mem.get_addr(0xffff);
      if triggered_interrupts != 0 {
        self.halted = false;
      }
      let successful_interrupt = triggered_interrupts & enabled_interrupts;
      let interrupts = InterruptFlags::from_bits_truncate(successful_interrupt);
      if interrupts.contains(InterruptFlags::VBLANK) {
        println!("vblank triggered!");
        self.fire_interrupt(0x40)
      } else if interrupts.contains(InterruptFlags::STAT) {
        println!("stat triggered!");
        self.fire_interrupt(0x48)
      } else if interrupts.contains(InterruptFlags::TIMER) {
        println!("timer triggered!");
        self.fire_interrupt(0x50)
      } else if interrupts.contains(InterruptFlags::SERIAL) {
        println!("serial triggered!");
        self.fire_interrupt(0x58)
      } else if interrupts.contains(InterruptFlags::JOYPAD) {
        println!("joypad triggered!");
        self.fire_interrupt(0x60)
      }
    }
  }

  pub fn execute(&mut self, opcode: u8) -> Cycle {
    use Reg16::*;
    use Reg8::*;
    let lsb = self.mem.get_addr((self.pc + 1) as usize);
    let msb = self.mem.get_addr((self.pc + 2) as usize);
    let nn: u16 = (lsb as u16) | ((msb as u16) << 8);
    match opcode {
      0x00 => self.nop(),
      0x01 => self.ld_16(BC, nn),
      0x02 => self.ld_r16_a(BC),
      0x03 => self.inc_16(BC),
      0x04 => self.inc_8(B),
      0x05 => self.dec_8(B),
      0x06 => self.ld_r8_n8(B, lsb),
      0x07 => self.rlc(A),
      0x08 => self.ld_nn_sp(nn, self.sp),
      0x09 => self.add_hl_rr(BC),
      0x0a => self.ld_a_rr(BC),
      0x0b => self.dec_16(BC),
      0x0c => self.inc_8(C),
      0x0d => self.dec_8(C),
      0x0e => self.ld_r8_n8(C, lsb),
      0x0f => self.rrc(A),
      0x10 => self.stop(),
      0x11 => self.ld_16(DE, nn),
      0x12 => self.ld_r16_a(DE),
      0x13 => self.inc_16(DE),
      0x14 => self.inc_8(D),
      0x15 => self.dec_8(D),
      0x16 => self.ld_r8_n8(D, lsb),
      0x17 => self.rla(),
      0x18 => self.jr_e8(lsb as i8),
      0x19 => self.add_hl_rr(DE),
      0x1a => self.ld_a_rr(DE),
      0x1b => self.dec_16(DE),
      0x1c => self.inc_8(E),
      0x1d => self.dec_8(E),
      0x1e => self.ld_r8_n8(E, lsb),
      0x1f => self.rra(),
      0x20 => self.jr_cc_e8(!self.get_flag(Flags::ZERO), lsb as i8),
      0x21 => self.ld_16(HL, nn),
      0x22 => self.ld_hli_a(),
      0x23 => self.inc_hl(),
      0x24 => self.inc_8(H),
      0x25 => self.dec_8(H),
      0x26 => self.ld_r8_n8(H, lsb),
      0x27 => self.daa(),
      0x28 => self.jr_cc_e8(self.get_flag(Flags::ZERO), lsb as i8),
      0x29 => self.add_hl_rr(HL),
      0x2a => self.ld_a_hli(),
      0x2b => self.dec_hl(),
      0x2c => self.inc_8(L),
      0x2d => self.dec_8(L),
      0x2e => self.ld_r8_n8(L, lsb),
      0x2f => self.cpl(),
      0x30 => self.jr_cc_e8(!self.get_flag(Flags::CARRY), lsb as i8),
      0x31 => self.ld_sp_nn(nn),
      0x32 => self.ld_hld_a(),
      0x33 => self.inc_sp(),
      0x34 => self.inc_hl(),
      0x35 => self.dec_hl(),
      0x36 => self.ld_hl_n8(lsb),
      0x37 => self.scf(),
      0x38 => self.jr_cc_e8(self.get_flag(Flags::CARRY), lsb as i8),
      0x39 => self.add_hl_sp(),
      0x3a => self.ld_a_hld(),
      0x3b => self.dec_sp(),
      0x3c => self.inc_8(A),
      0x3d => self.dec_8(A),
      0x3e => self.ld_r8_n8(A, lsb),
      0x3f => self.ccf(),
      0x40 => self.ld_r8_r8(B, B),
      0x41 => self.ld_r8_r8(B, C),
      0x42 => self.ld_r8_r8(B, D),
      0x43 => self.ld_r8_r8(B, E),
      0x44 => self.ld_r8_r8(B, H),
      0x45 => self.ld_r8_r8(B, L),
      0x46 => self.ld_r8_hl(B),
      0x47 => self.ld_r8_r8(B, A),
      0x48 => self.ld_r8_r8(C, B),
      0x49 => self.ld_r8_r8(C, C),
      0x4a => self.ld_r8_r8(C, D),
      0x4b => self.ld_r8_r8(C, E),
      0x4c => self.ld_r8_r8(C, H),
      0x4d => self.ld_r8_r8(C, L),
      0x4e => self.ld_r8_hl(C),
      0x4f => self.ld_r8_r8(C, A),
      0x50 => self.ld_r8_r8(D, B),
      0x51 => self.ld_r8_r8(D, C),
      0x52 => self.ld_r8_r8(D, D),
      0x53 => self.ld_r8_r8(D, E),
      0x54 => self.ld_r8_r8(D, H),
      0x55 => self.ld_r8_r8(D, L),
      0x56 => self.ld_r8_hl(D),
      0x57 => self.ld_r8_r8(D, A),
      0x58 => self.ld_r8_r8(E, B),
      0x59 => self.ld_r8_r8(E, C),
      0x5a => self.ld_r8_r8(E, D),
      0x5b => self.ld_r8_r8(E, E),
      0x5c => self.ld_r8_r8(E, H),
      0x5d => self.ld_r8_r8(E, L),
      0x5e => self.ld_r8_hl(E),
      0x5f => self.ld_r8_r8(E, A),
      0x60 => self.ld_r8_r8(H, B),
      0x61 => self.ld_r8_r8(H, C),
      0x62 => self.ld_r8_r8(H, D),
      0x63 => self.ld_r8_r8(H, E),
      0x64 => self.ld_r8_r8(H, H),
      0x65 => self.ld_r8_r8(H, L),
      0x66 => self.ld_r8_hl(H),
      0x67 => self.ld_r8_r8(H, A),
      0x68 => self.ld_r8_r8(L, B),
      0x69 => self.ld_r8_r8(L, C),
      0x6a => self.ld_r8_r8(L, D),
      0x6b => self.ld_r8_r8(L, E),
      0x6c => self.ld_r8_r8(L, H),
      0x6d => self.ld_r8_r8(L, L),
      0x6e => self.ld_r8_hl(L),
      0x6f => self.ld_r8_r8(L, A),
      0x70 => self.ld_hl_r8(B),
      0x71 => self.ld_hl_r8(C),
      0x72 => self.ld_hl_r8(D),
      0x73 => self.ld_hl_r8(E),
      0x74 => self.ld_hl_r8(H),
      0x75 => self.ld_hl_r8(L),
      0x76 => self.halt(),
      0x77 => self.ld_hl_r8(A),
      0x78 => self.ld_r8_r8(A, B),
      0x79 => self.ld_r8_r8(A, C),
      0x7a => self.ld_r8_r8(A, D),
      0x7b => self.ld_r8_r8(A, E),
      0x7c => self.ld_r8_r8(A, H),
      0x7d => self.ld_r8_r8(A, L),
      0x7e => self.ld_r8_hl(A),
      0x7f => self.ld_r8_r8(A, A),
      0x80 => self.add_r8(B),
      0x81 => self.add_r8(C),
      0x82 => self.add_r8(D),
      0x83 => self.add_r8(E),
      0x84 => self.add_r8(H),
      0x85 => self.add_r8(L),
      0x86 => self.add_a_hl(),
      0x87 => self.add_r8(A),
      0x88 => self.adc_r8(B),
      0x89 => self.adc_r8(C),
      0x8a => self.adc_r8(D),
      0x8b => self.adc_r8(E),
      0x8c => self.adc_r8(H),
      0x8d => self.adc_r8(L),
      0x8e => self.adc_hl(),
      0x8f => self.adc_r8(A),
      0x90 => self.sub_r8(B),
      0x91 => self.sub_r8(C),
      0x92 => self.sub_r8(D),
      0x93 => self.sub_r8(E),
      0x94 => self.sub_r8(H),
      0x95 => self.sub_r8(L),
      0x96 => self.sub_a_hl(),
      0x97 => self.sub_r8(A),
      0x98 => self.sbc_r8(B),
      0x99 => self.sbc_r8(C),
      0x9a => self.sbc_r8(D),
      0x9b => self.sbc_r8(E),
      0x9c => self.sbc_r8(H),
      0x9d => self.sbc_r8(L),
      0x9e => self.sbc_a_hl(),
      0x9f => self.sbc_r8(A),
      0xa0 => self.and_r8(B),
      0xa1 => self.and_r8(C),
      0xa2 => self.and_r8(D),
      0xa3 => self.and_r8(E),
      0xa4 => self.and_r8(H),
      0xa5 => self.and_r8(L),
      0xa6 => self.and_a_hl(),
      0xa7 => self.and_r8(A),
      0xa8 => self.xor_r8(B),
      0xa9 => self.xor_r8(C),
      0xaa => self.xor_r8(D),
      0xab => self.xor_r8(E),
      0xac => self.xor_r8(H),
      0xad => self.xor_r8(L),
      0xae => self.xor_a_hl(),
      0xaf => self.xor_r8(A),
      0xb0 => self.or_r8(B),
      0xb1 => self.or_r8(C),
      0xb2 => self.or_r8(D),
      0xb3 => self.or_r8(E),
      0xb4 => self.or_r8(H),
      0xb5 => self.or_r8(L),
      0xb6 => self.or_hl(),
      0xb7 => self.or_r8(A),
      0xb8 => self.cp_r8(B),
      0xb9 => self.cp_r8(C),
      0xba => self.cp_r8(D),
      0xbb => self.cp_r8(E),
      0xbc => self.cp_r8(H),
      0xbd => self.cp_r8(L),
      0xbe => self.cp_a_hl(),
      0xbf => self.cp_r8(A),
      0xc0 => self.ret_cc(!self.get_flag(Flags::ZERO)),
      0xc1 => self.pop_r16(BC),
      0xc2 => self.jp_cc_nn(!self.get_flag(Flags::ZERO), nn),
      0xc3 => self.jp_nn(nn),
      0xc4 => self.call_cc_nn(!self.get_flag(Flags::ZERO), nn),
      0xc5 => self.push_r16(BC),
      0xc6 => self.add_n8(lsb),
      0xc7 => self.rst(0x00),
      0xc8 => self.ret_cc(self.get_flag(Flags::ZERO)),
      0xc9 => self.ret(),
      0xca => self.jp_cc_nn(self.get_flag(Flags::ZERO), nn),
      0xcc => self.call_cc_nn(self.get_flag(Flags::ZERO), nn),
      0xcd => self.call_nn(nn),
      0xce => self.adc_n8(lsb),
      0xcf => self.rst(0x08),
      0xd0 => self.ret_cc(!self.get_flag(Flags::CARRY)),
      0xd1 => self.pop_r16(DE),
      0xd2 => self.jp_cc_nn(!self.get_flag(Flags::CARRY), nn),
      0xd3 => panic!(
        "Shouldn't be able to access opcode {}",
        format!("{:x}", opcode)
      ),
      0xd4 => self.call_cc_nn(!self.get_flag(Flags::CARRY), nn),
      0xd5 => self.push_r16(DE),
      0xd6 => self.sub_n8(lsb),
      0xd7 => self.rst(0x10),
      0xd8 => self.ret_cc(self.get_flag(Flags::CARRY)),
      0xd9 => self.reti(),
      0xda => self.jp_cc_nn(self.get_flag(Flags::CARRY), nn),
      0xdb => panic!(
        "Shouldn't be able to access opcode {}",
        format!("{:x}", opcode)
      ),
      0xdc => self.call_cc_nn(self.get_flag(Flags::CARRY), nn),
      0xdd => panic!(
        "Shouldn't be able to access opcode {}",
        format!("{:x}", opcode)
      ),
      0xde => self.sbc_n8(lsb),
      0xdf => self.rst(0x18),
      0xe0 => self.ldh_nn_a(nn),
      0xe1 => self.pop_r16(HL),
      0xe2 => self.ldh_c_a(),
      0xe5 => self.push_r16(HL),
      0xe6 => self.and_n8(lsb),
      0xe7 => self.rst(0x20),
      0xe8 => self.add_sp_e8(lsb as i8),
      0xe9 => self.jp_hl(),
      0xea => self.ld_nn_a(nn),
      0xee => self.xor_n8(lsb),
      0xef => self.rst(0x28),
      0xf0 => self.ldh_a_nn(nn),
      0xf1 => self.pop_af(),
      0xf2 => self.ldh_a_c(),
      0xf3 => self.di(),
      0xf5 => self.push_r16(AF),
      0xf6 => self.or_n8(lsb),
      0xf7 => self.rst(0x30),
      0xf8 => self.ld_hl_sp_plus(lsb as i8),
      0xf9 => self.ld_sp_hl(),
      0xfa => self.ld_nn(nn),
      0xfb => self.ei(),
      0xfe => self.cp_n8(lsb),
      0xff => self.rst(0x38),
      0xcb => {
        // self.pc += 1;
        let new_opcode = self.mem.get_addr((self.pc + 1) as usize);
        match new_opcode {
          0x00 => self.rlc(B),
          0x01 => self.rlc(C),
          0x02 => self.rlc(D),
          0x03 => self.rlc(E),
          0x04 => self.rlc(H),
          0x05 => self.rlc(L),
          0x06 => self.rlc_hl(),
          0x07 => self.rlc(A),
          0x08 => self.rrc(B),
          0x09 => self.rrc(C),
          0x0a => self.rrc(D),
          0x0b => self.rrc(E),
          0x0c => self.rrc(H),
          0x0d => self.rrc(L),
          0x0e => self.rrc_hl(),
          0x0f => self.rrc(A),
          0x10 => self.rl_r8(B),
          0x11 => self.rl_r8(C),
          0x12 => self.rl_r8(D),
          0x13 => self.rl_r8(E),
          0x14 => self.rl_r8(H),
          0x15 => self.rl_r8(L),
          0x16 => self.rl_hl(),
          0x17 => self.rl_r8(A),
          0x18 => self.rr_r8(B),
          0x19 => self.rr_r8(C),
          0x1a => self.rr_r8(D),
          0x1b => self.rr_r8(E),
          0x1c => self.rr_r8(H),
          0x1d => self.rr_r8(L),
          0x1e => self.rr_hl(),
          0x1f => self.rr_r8(A),
          0x20 => self.sla_r8(B),
          0x21 => self.sla_r8(C),
          0x22 => self.sla_r8(D),
          0x23 => self.sla_r8(E),
          0x24 => self.sla_r8(H),
          0x25 => self.sla_r8(L),
          0x26 => self.sla_hl(),
          0x27 => self.sla_r8(A),
          0x28 => self.sra_r8(B),
          0x29 => self.sra_r8(C),
          0x2a => self.sra_r8(D),
          0x2b => self.sra_r8(E),
          0x2c => self.sra_r8(H),
          0x2d => self.sra_r8(L),
          0x2e => self.sra_hl(),
          0x2f => self.sra_r8(A),
          0x30 => self.swap_r8(B),
          0x31 => self.swap_r8(C),
          0x32 => self.swap_r8(D),
          0x33 => self.swap_r8(E),
          0x34 => self.swap_r8(H),
          0x35 => self.swap_r8(L),
          0x36 => self.swap_hl(),
          0x37 => self.swap_r8(A),
          0x38 => self.srl_r8(B),
          0x39 => self.srl_r8(C),
          0x3a => self.srl_r8(D),
          0x3b => self.srl_r8(E),
          0x3c => self.srl_r8(H),
          0x3d => self.srl_r8(L),
          0x3e => self.srl_hl(),
          0x3f => self.srl_r8(A),
          0x40 => self.bit_u_r8(0, B),
          0x41 => self.bit_u_r8(0, C),
          0x42 => self.bit_u_r8(0, D),
          0x43 => self.bit_u_r8(0, E),
          0x44 => self.bit_u_r8(0, H),
          0x45 => self.bit_u_r8(0, L),
          0x46 => self.bit_u_hl(0),
          0x47 => self.bit_u_r8(0, A),
          0x48 => self.bit_u_r8(1, B),
          0x49 => self.bit_u_r8(1, C),
          0x4a => self.bit_u_r8(1, D),
          0x4b => self.bit_u_r8(1, E),
          0x4c => self.bit_u_r8(1, H),
          0x4d => self.bit_u_r8(1, L),
          0x4e => self.bit_u_hl(1),
          0x4f => self.bit_u_r8(1, A),
          0x50 => self.bit_u_r8(2, B),
          0x51 => self.bit_u_r8(2, C),
          0x52 => self.bit_u_r8(2, D),
          0x53 => self.bit_u_r8(2, E),
          0x54 => self.bit_u_r8(2, H),
          0x55 => self.bit_u_r8(2, L),
          0x56 => self.bit_u_hl(2),
          0x57 => self.bit_u_r8(2, A),
          0x58 => self.bit_u_r8(3, B),
          0x59 => self.bit_u_r8(3, C),
          0x5a => self.bit_u_r8(3, D),
          0x5b => self.bit_u_r8(3, E),
          0x5c => self.bit_u_r8(3, H),
          0x5d => self.bit_u_r8(3, L),
          0x5e => self.bit_u_hl(3),
          0x5f => self.bit_u_r8(3, A),
          0x60 => self.bit_u_r8(4, B),
          0x61 => self.bit_u_r8(4, C),
          0x62 => self.bit_u_r8(4, D),
          0x63 => self.bit_u_r8(4, E),
          0x64 => self.bit_u_r8(4, H),
          0x65 => self.bit_u_r8(4, L),
          0x66 => self.bit_u_hl(4),
          0x67 => self.bit_u_r8(4, A),
          0x68 => self.bit_u_r8(5, B),
          0x69 => self.bit_u_r8(5, C),
          0x6a => self.bit_u_r8(5, D),
          0x6b => self.bit_u_r8(5, E),
          0x6c => self.bit_u_r8(5, H),
          0x6d => self.bit_u_r8(5, L),
          0x6e => self.bit_u_hl(5),
          0x6f => self.bit_u_r8(5, A),
          0x70 => self.bit_u_r8(6, B),
          0x71 => self.bit_u_r8(6, C),
          0x72 => self.bit_u_r8(6, D),
          0x73 => self.bit_u_r8(6, E),
          0x74 => self.bit_u_r8(6, H),
          0x75 => self.bit_u_r8(6, L),
          0x76 => self.bit_u_hl(6),
          0x77 => self.bit_u_r8(6, A),
          0x78 => self.bit_u_r8(7, B),
          0x79 => self.bit_u_r8(7, C),
          0x7a => self.bit_u_r8(7, D),
          0x7b => self.bit_u_r8(7, E),
          0x7c => self.bit_u_r8(7, H),
          0x7d => self.bit_u_r8(7, L),
          0x7e => self.bit_u_hl(7),
          0x7f => self.bit_u_r8(7, A),
          0x80 => self.res_u_r8(0, B),
          0x81 => self.res_u_r8(0, C),
          0x82 => self.res_u_r8(0, D),
          0x83 => self.res_u_r8(0, E),
          0x84 => self.res_u_r8(0, H),
          0x85 => self.res_u_r8(0, L),
          0x86 => self.res_u_hl(0),
          0x87 => self.res_u_r8(0, A),
          0x88 => self.res_u_r8(1, B),
          0x89 => self.res_u_r8(1, C),
          0x8a => self.res_u_r8(1, D),
          0x8b => self.res_u_r8(1, E),
          0x8c => self.res_u_r8(1, H),
          0x8d => self.res_u_r8(1, L),
          0x8e => self.res_u_hl(1),
          0x8f => self.res_u_r8(1, A),
          0x90 => self.res_u_r8(2, B),
          0x91 => self.res_u_r8(2, C),
          0x92 => self.res_u_r8(2, D),
          0x93 => self.res_u_r8(2, E),
          0x94 => self.res_u_r8(2, H),
          0x95 => self.res_u_r8(2, L),
          0x96 => self.res_u_hl(2),
          0x97 => self.res_u_r8(2, A),
          0x98 => self.res_u_r8(3, B),
          0x99 => self.res_u_r8(3, C),
          0x9a => self.res_u_r8(3, D),
          0x9b => self.res_u_r8(3, E),
          0x9c => self.res_u_r8(3, H),
          0x9d => self.res_u_r8(3, L),
          0x9e => self.res_u_hl(3),
          0x9f => self.res_u_r8(3, A),
          0xa0 => self.res_u_r8(4, B),
          0xa1 => self.res_u_r8(4, C),
          0xa2 => self.res_u_r8(4, D),
          0xa3 => self.res_u_r8(4, E),
          0xa4 => self.res_u_r8(4, H),
          0xa5 => self.res_u_r8(4, L),
          0xa6 => self.res_u_hl(4),
          0xa7 => self.res_u_r8(4, A),
          0xa8 => self.res_u_r8(5, B),
          0xa9 => self.res_u_r8(5, C),
          0xaa => self.res_u_r8(5, D),
          0xab => self.res_u_r8(5, E),
          0xac => self.res_u_r8(5, H),
          0xad => self.res_u_r8(5, L),
          0xae => self.res_u_hl(5),
          0xaf => self.res_u_r8(5, A),
          0xb0 => self.res_u_r8(6, B),
          0xb1 => self.res_u_r8(6, C),
          0xb2 => self.res_u_r8(6, D),
          0xb3 => self.res_u_r8(6, E),
          0xb4 => self.res_u_r8(6, H),
          0xb5 => self.res_u_r8(6, L),
          0xb6 => self.res_u_hl(6),
          0xb7 => self.res_u_r8(6, A),
          0xb8 => self.res_u_r8(7, B),
          0xb9 => self.res_u_r8(7, C),
          0xba => self.res_u_r8(7, D),
          0xbb => self.res_u_r8(7, E),
          0xbc => self.res_u_r8(7, H),
          0xbd => self.res_u_r8(7, L),
          0xbe => self.res_u_hl(7),
          0xbf => self.res_u_r8(7, A),
          0xc0 => self.set_u_r8(0, B),
          0xc1 => self.set_u_r8(0, C),
          0xc2 => self.set_u_r8(0, D),
          0xc3 => self.set_u_r8(0, E),
          0xc4 => self.set_u_r8(0, H),
          0xc5 => self.set_u_r8(0, L),
          0xc6 => self.set_u_hl(0),
          0xc7 => self.set_u_r8(0, A),
          0xc8 => self.set_u_r8(1, B),
          0xc9 => self.set_u_r8(1, C),
          0xca => self.set_u_r8(1, D),
          0xcb => self.set_u_r8(1, E),
          0xcc => self.set_u_r8(1, H),
          0xcd => self.set_u_r8(1, L),
          0xce => self.set_u_hl(1),
          0xcf => self.set_u_r8(1, A),
          0xd0 => self.set_u_r8(2, B),
          0xd1 => self.set_u_r8(2, C),
          0xd2 => self.set_u_r8(2, D),
          0xd3 => self.set_u_r8(2, E),
          0xd4 => self.set_u_r8(2, H),
          0xd5 => self.set_u_r8(2, L),
          0xd6 => self.set_u_hl(2),
          0xd7 => self.set_u_r8(2, A),
          0xd8 => self.set_u_r8(3, B),
          0xd9 => self.set_u_r8(3, C),
          0xda => self.set_u_r8(3, D),
          0xdb => self.set_u_r8(3, E),
          0xdc => self.set_u_r8(3, H),
          0xdd => self.set_u_r8(3, L),
          0xde => self.set_u_hl(3),
          0xdf => self.set_u_r8(3, A),
          0xe0 => self.set_u_r8(4, B),
          0xe1 => self.set_u_r8(4, C),
          0xe2 => self.set_u_r8(4, D),
          0xe3 => self.set_u_r8(4, E),
          0xe4 => self.set_u_r8(4, H),
          0xe5 => self.set_u_r8(4, L),
          0xe6 => self.set_u_hl(4),
          0xe7 => self.set_u_r8(4, A),
          0xe8 => self.set_u_r8(5, B),
          0xe9 => self.set_u_r8(5, C),
          0xea => self.set_u_r8(5, D),
          0xeb => self.set_u_r8(5, E),
          0xec => self.set_u_r8(5, H),
          0xed => self.set_u_r8(5, L),
          0xee => self.set_u_hl(5),
          0xef => self.set_u_r8(5, A),
          0xf0 => self.set_u_r8(6, B),
          0xf1 => self.set_u_r8(6, C),
          0xf2 => self.set_u_r8(6, D),
          0xf3 => self.set_u_r8(6, E),
          0xf4 => self.set_u_r8(6, H),
          0xf5 => self.set_u_r8(6, L),
          0xf6 => self.set_u_hl(6),
          0xf7 => self.set_u_r8(6, A),
          0xf8 => self.set_u_r8(7, B),
          0xf9 => self.set_u_r8(7, C),
          0xfa => self.set_u_r8(7, D),
          0xfb => self.set_u_r8(7, E),
          0xfc => self.set_u_r8(7, H),
          0xfd => self.set_u_r8(7, L),
          0xfe => self.set_u_hl(7),
          0xff => self.set_u_r8(7, A),
        }
      }
      _ => panic!("you need to handle opcode {}", opcode),
    }
  }

  /* 8-BIT ARITHMETIC AND LOGIC */

  /// ADC A, r8
  ///
  /// Add the value in r8 plus the carry flag to A.
  fn adc_r8(&mut self, reg: Reg8) -> Cycle {
    let value = self.read8reg(&reg);
    let carry = self.get_flag(Flags::CARRY) as u8;
    let new_value = self
      .read8reg(&Reg8::A)
      .wrapping_add(value)
      .wrapping_add(carry);
    self.a = new_value;

    self.set_zf(new_value == 0);
    self.set_nf(false);
    self.set_hf((self.a & 0xf) + (value & 0xf) + carry > 0xf);
    self.set_cf(self.a as u16 + value as u16 + carry as u16 > 0xff);

    self.pc += 1;
    Cycle::ONE
  }

  /// ADC A, [HL]
  ///
  /// Add the byte pointed to by HL plus the carry flag to A
  fn adc_hl(&mut self) -> Cycle {
    let mem_value = self.read16reg(&Reg16::HL);
    let value = self.mem.get_addr(mem_value as usize);
    let carry = self.get_flag(Flags::CARRY) as u8;
    let new_value = self.a.wrapping_add(value).wrapping_add(carry);
    self.a = new_value;

    self.set_zf(new_value == 0);
    self.set_nf(false);
    self.set_hf((self.a & 0xf) + (value & 0xf) + carry > 0xf);
    self.set_cf(self.a as u16 + value as u16 + carry as u16 > 0xff);

    self.pc += 1;
    Cycle::TWO
  }

  /// ADC A, n8
  ///
  /// Add the value n8 plus the carry flag to A.
  fn adc_n8(&mut self, value: u8) -> Cycle {
    let carry = self.get_flag(Flags::CARRY) as u8;
    let new_value = self.a.wrapping_add(value).wrapping_add(carry);

    self.set_zf(new_value == 0);
    self.set_nf(false);
    self.set_hf((self.a & 0xf) + (value & 0xf) + carry > 0xf);
    self.set_cf(self.a as u16 + value as u16 + carry as u16 > 0xff);

    self.pc += 2;
    Cycle::TWO
  }

  /// ADD A, r8
  ///
  /// Add the value in r8 to A.
  fn add_r8(&mut self, reg: Reg8) -> Cycle {
    let value = self.read8reg(&reg);
    let (new_value, carry) = self.a.overflowing_add(value);
    let half_carry = (self.a & 0x0f).checked_add(value | 0xf0).is_none();
    self.a = new_value;

    self.set_zf(new_value == 0);
    self.set_nf(false);
    self.set_hf(half_carry);
    self.set_cf(carry);

    self.pc += 1;
    Cycle::ONE
  }

  /// ADD A,[HL]
  ///
  /// Add the byte pointed to by HL to A.
  fn add_a_hl(&mut self) -> Cycle {
    let addr = self.read16reg(&Reg16::HL);
    let value = self.mem.get_addr(addr as usize);
    let (new_value, carry) = self.a.overflowing_add(value);
    let half_carry = (self.a & 0x0f).checked_add(value | 0xf0).is_none();
    self.a = new_value;

    self.set_zf(new_value == 0);
    self.set_nf(false);
    self.set_hf(half_carry);
    self.set_cf(carry);

    self.pc += 1;
    Cycle::TWO
  }

  /// ADD A,n8
  ///
  /// Add the value n8 to A.
  fn add_n8(&mut self, value: u8) -> Cycle {
    let (new_value, carry) = self.a.overflowing_add(value);
    let half_carry = (self.a & 0x0f).checked_add(value | 0xf0).is_none();
    self.a = new_value;

    self.set_zf(new_value == 0);
    self.set_nf(false);
    self.set_hf(half_carry);
    self.set_cf(carry);

    self.pc += 2;
    Cycle::TWO
  }

  /// AND A,r8
  ///
  /// Bitwise AND between the value in r8 and A.
  fn and_r8(&mut self, reg: Reg8) -> Cycle {
    let new_value = self.a & self.read8reg(&reg);
    self.a = new_value;

    self.set_zf(new_value == 0);
    self.set_nf(false);
    self.set_hf(true);
    self.set_cf(false);

    self.pc += 1;
    Cycle::ONE
  }

  /// AND A,[HL]
  ///
  /// Bitwise AND between the byte pointed to by HL and A.
  fn and_a_hl(&mut self) -> Cycle {
    let addr = self.read16reg(&Reg16::HL);
    let new_value = self.a & self.mem.get_addr(addr as usize);

    self.set_zf(new_value == 0);
    self.set_nf(false);
    self.set_hf(true);
    self.set_cf(false);

    self.pc += 1;
    Cycle::TWO
  }

  /// AND A,n8
  ///
  /// Bitwise AND between the value in n8 and A.
  fn and_n8(&mut self, value: u8) -> Cycle {
    let new_value = self.a & value;

    self.set_zf(new_value == 0);
    self.set_nf(false);
    self.set_hf(true);
    self.set_cf(false);

    self.pc += 2;
    Cycle::TWO
  }

  /// CP A,r8
  ///
  /// Subtract the value in r8 from A and set flags accordingly, but don't store the result. This is useful for ComParing values.
  fn cp_r8(&mut self, reg: Reg8) -> Cycle {
    let value = self.read8reg(&reg);
    let new_value = self.a.wrapping_sub(value);

    self.set_zf(new_value == 0);
    self.set_nf(true);
    self.set_hf((self.a & 0xf).wrapping_sub(value & 0xf) & (0xf + 1) != 0); // This may be wrong, no idea
    self.set_cf(value > self.a);

    self.pc += 1;
    Cycle::ONE
  }

  /// CP A,[HL]
  ///
  /// Subtract the byte pointed to by HL from A and set flags accordingly, but don't store the result.
  fn cp_a_hl(&mut self) -> Cycle {
    let addr = self.read16reg(&Reg16::HL);
    let value = self.mem.get_addr(addr as usize);
    let new_value = self.a.wrapping_sub(value);

    self.set_zf(new_value == 0);
    self.set_nf(true);
    self.set_hf((self.a & 0xf).wrapping_sub(value & 0xf) & (0xf + 1) != 0); // This may be wrong, no idea
    self.set_cf(value > self.a);

    self.pc += 1;
    Cycle::TWO
  }

  /// CP A,n8
  ///
  /// Subtract the value n8 from A and set flags accordingly, but don't store the result.
  fn cp_n8(&mut self, value: u8) -> Cycle {
    let new_value = self.a.wrapping_sub(value);

    self.set_zf(new_value == 0);
    self.set_nf(true);
    self.set_hf((self.a & 0xf).wrapping_sub(value & 0xf) & (0xf + 1) != 0); // This may be wrong, no idea
    self.set_cf(value > self.a);

    self.pc += 2;
    Cycle::TWO
  }

  /// DEC r8
  ///
  /// Decrement value in register r8 by 1.
  fn dec_8(&mut self, reg: Reg8) -> Cycle {
    let value = self.read8reg(&reg);
    let new_value = value.wrapping_sub(1);
    self.write8reg(&reg, new_value);

    self.set_zf(new_value == 0);
    self.set_nf(true);
    self.set_hf(value & 0xf == 0);

    self.pc += 1;
    Cycle::ONE
  }

  /// DEC [HL]
  ///
  /// Decrement the byte pointed to by HL by 1.
  fn dec_hl(&mut self) -> Cycle {
    let addr = self.read16reg(&Reg16::HL);
    let value = self.mem.get_addr(addr as usize);
    let new_value = value.wrapping_sub(1);
    self.mem.write_addr(addr as usize, new_value);

    self.set_zf(new_value == 0);
    self.set_nf(true);
    self.set_hf(value & 0xf == 0);

    self.pc += 1;
    Cycle::THREE
  }

  /// INC r8
  ///
  /// Increment value in register r8 by 1.
  fn inc_8(&mut self, reg: Reg8) -> Cycle {
    let value = self.read8reg(&reg);
    let new_value = value.wrapping_add(1);
    self.write8reg(&reg, new_value);

    self.set_zf(new_value == 0);
    self.set_nf(false);
    self.set_hf(value & 0xf == 0xf);

    self.pc += 1;
    Cycle::ONE
  }

  /// INC [HL]
  ///
  /// Increment the byte pointed to by HL by 1.
  fn inc_hl(&mut self) -> Cycle {
    let addr = self.read16reg(&Reg16::HL);
    let value = self.mem.get_addr(addr as usize);
    let new_value = value.wrapping_sub(1);
    self.mem.write_addr(addr as usize, value);

    self.set_zf(new_value == 0);
    self.set_nf(false);
    self.set_hf(value & 0xf == 0xf);

    self.pc += 1;
    Cycle::THREE
  }

  /// OR A,r8
  ///
  /// Store into A the bitwise OR of the value in r8 and A.
  fn or_r8(&mut self, reg: Reg8) -> Cycle {
    let value = self.read8reg(&reg);
    let new_value = self.a | value;
    self.a = new_value;

    self.set_zf(new_value == 0);
    self.set_nf(false);
    self.set_hf(false);
    self.set_cf(false);

    self.pc += 1;
    Cycle::ONE
  }

  /// OR A,[HL]
  ///
  /// Store into A the bitwise OR of the byte pointed to by HL and A.
  fn or_hl(&mut self) -> Cycle {
    let addr = self.read16reg(&Reg16::HL) as usize;
    let value = self.mem.get_addr(addr);
    let new_value = self.a | value;
    self.a = new_value;

    self.set_zf(new_value == 0);
    self.set_nf(false);
    self.set_hf(false);
    self.set_cf(false);

    self.pc += 1;
    Cycle::TWO
  }

  /// OR A,n8
  ///
  /// Store into A the bitwise OR of n8 and A.
  fn or_n8(&mut self, value: u8) -> Cycle {
    let new_value = self.a | value;
    self.a = new_value;

    self.set_zf(new_value == 0);
    self.set_nf(false);
    self.set_hf(false);
    self.set_cf(false);

    self.pc += 2;
    Cycle::TWO
  }

  /// SBC A,r8
  ///
  /// Subtract the value in r8 and the carry flag from A.
  fn sbc_r8(&mut self, reg: Reg8) -> Cycle {
    let value = self.read8reg(&reg);
    let carry = self.get_flag(Flags::CARRY) as u8;
    let new_value = self.a.wrapping_sub(value).wrapping_sub(carry);

    self.set_zf(new_value == 0);
    self.set_nf(true);
    self.set_hf((self.a & 0xf).wrapping_sub(value & 0xf).wrapping_sub(carry) & (0xf + 1) != 0);
    self.set_cf((value + carry) > self.a);
    self.a = new_value;

    self.pc += 1;
    Cycle::ONE
  }

  /// SBC A,[HL]
  ///
  /// Subtract the byte pointed to by HL and the carry flag from A.
  fn sbc_a_hl(&mut self) -> Cycle {
    let addr = self.read16reg(&Reg16::HL) as usize;
    let value = self.mem.get_addr(addr);
    let carry = self.get_flag(Flags::CARRY) as u8;
    let new_value = self.a.wrapping_sub(value).wrapping_sub(carry);

    self.set_zf(new_value == 0);
    self.set_nf(true);
    self.set_hf((self.a & 0xf).wrapping_sub(value & 0xf).wrapping_sub(carry) & (0xf + 1) != 0);
    self.set_cf((value + carry) > self.a);
    self.a = new_value;

    self.pc += 1;
    Cycle::TWO
  }

  /// SBC A,n8
  ///
  /// Subtract the value n8 and the carry flag from A.
  fn sbc_n8(&mut self, value: u8) -> Cycle {
    let carry = self.get_flag(Flags::CARRY) as u8;
    let new_value = self.a.wrapping_sub(value).wrapping_sub(carry);

    self.set_zf(new_value == 0);
    self.set_nf(true);
    self.set_hf((self.a & 0xf).wrapping_sub(value & 0xf).wrapping_sub(carry) & (0xf + 1) != 0);
    self.set_cf((value + carry) > self.a);
    self.a = new_value;

    self.pc += 2;
    Cycle::TWO
  }

  /// SUB A,r8
  ///
  /// Subtract the value in r8 from A.
  fn sub_r8(&mut self, reg: Reg8) -> Cycle {
    let value = self.read8reg(&reg);
    let new_value = self.a.wrapping_sub(value);

    self.set_zf(new_value == 0);
    self.set_nf(true);
    self.set_hf((self.a & 0xf).wrapping_sub(value & 0xf) & (0xf + 1) != 0);
    self.set_cf(value > self.a);
    self.a = new_value;

    self.pc += 1;
    Cycle::ONE
  }

  /// SUB A,[HL]
  ///
  /// Subtract the byte pointed to by HL from A.
  fn sub_a_hl(&mut self) -> Cycle {
    let addr = self.read16reg(&Reg16::HL) as usize;
    let value = self.mem.get_addr(addr);
    let new_value = self.a.wrapping_sub(value);

    self.set_zf(new_value == 0);
    self.set_nf(true);
    self.set_hf((self.a & 0xf).wrapping_sub(value & 0xf) & (0xf + 1) != 0);
    self.set_cf(value > self.a);
    self.a = new_value;

    self.pc += 1;
    Cycle::TWO
  }

  /// SUB A,n8
  ///
  /// Subtract the value n8 from A.
  fn sub_n8(&mut self, value: u8) -> Cycle {
    let new_value = self.a.wrapping_sub(value);

    self.set_zf(new_value == 0);
    self.set_nf(true);
    self.set_hf((self.a & 0xf).wrapping_sub(value & 0xf) & (0xf + 1) != 0);
    self.set_cf(value > self.a);
    self.a = new_value;

    self.pc += 2;
    Cycle::TWO
  }

  /// XOR A,r8
  ///
  /// Bitwise XOR between the value in r8 and A.
  fn xor_r8(&mut self, reg: Reg8) -> Cycle {
    let value = self.read8reg(&reg);
    let new_value = self.a ^ value;
    self.a = new_value;

    self.set_zf(new_value == 0);
    self.set_nf(false);
    self.set_hf(false);
    self.set_cf(false);

    self.pc += 1;
    Cycle::ONE
  }

  /// XOR A,[HL]
  ///
  /// Bitwise XOR between the byte pointed to by HL and A.
  fn xor_a_hl(&mut self) -> Cycle {
    let addr = self.read16reg(&Reg16::HL) as usize;
    let value = self.mem.get_addr(addr);
    let new_value = self.a ^ value;
    self.a = new_value;

    self.set_zf(new_value == 0);
    self.set_nf(false);
    self.set_hf(false);
    self.set_cf(false);

    self.pc += 1;
    Cycle::TWO
  }

  /// XOR A,n8
  ///
  ///
  fn xor_n8(&mut self, value: u8) -> Cycle {
    let new_value = self.a ^ value;
    self.a = new_value;

    self.set_zf(new_value == 0);
    self.set_nf(false);
    self.set_hf(false);
    self.set_cf(false);

    self.pc += 2;
    Cycle::TWO
  }
  /* 16-bit Arithmetic Instructions */

  /// ADD HL,r16
  ///
  /// Add the value in r16 to HL.
  fn add_hl_rr(&mut self, value_reg: Reg16) -> Cycle {
    let value = self.read16reg(&Reg16::HL);
    let add_value = self.read16reg(&value_reg);
    let new_value = value.wrapping_add(add_value);
    self.write16reg(&Reg16::HL, new_value);

    self.set_nf(false);
    // TODO: set carry flags here

    self.pc += 1;
    Cycle::TWO
  }

  /// DEC r16
  ///
  /// Decrement value in register r16 by 1.
  fn dec_16(&mut self, reg: Reg16) -> Cycle {
    let new_value = self.read16reg(&reg).wrapping_sub(1);
    self.write16reg(&reg, new_value);
    self.pc += 1;
    Cycle::TWO
  }

  /// INC r16
  ///
  /// Increment value in register r16 by 1.
  fn inc_16(&mut self, reg: Reg16) -> Cycle {
    let num = self.read16reg(&reg);
    self.write16reg(&reg, num + 1);

    self.pc += 1;
    Cycle::TWO
  }

  /* Bit Operations Instructions */

  /// BIT u3,r8
  ///
  /// Test bit u3 in register r8, set the zero flag if bit not set.
  fn bit_u_r8(&mut self, bit: u8, reg: Reg8) -> Cycle {
    let value = self.read8reg(&reg) & (1 << bit);

    self.set_zf(value == 0);
    self.set_nf(false);
    self.set_hf(true);

    self.pc += 2;
    Cycle::TWO
  }

  /// BIT u3,[HL]
  ///
  /// Test bit u3 in the byte pointed by HL, set the zero flag if bit not set.
  fn bit_u_hl(&mut self, bit: u8) -> Cycle {
    let addr = self.read16reg(&Reg16::HL) as usize;
    let value = self.mem.get_addr(addr) & (1 << bit);

    self.set_zf(value == 0);
    self.set_nf(false);
    self.set_hf(true);

    self.pc += 2;
    Cycle::THREE
  }

  /// RES u3,r8
  ///
  /// Set bit u3 in register r8 to 0. Bit 0 is the rightmost one, bit 7 the leftmost one.
  fn res_u_r8(&mut self, bit: u8, reg: Reg8) -> Cycle {
    let value = self.read8reg(&reg) & !(1 << bit);
    self.write8reg(&reg, value);

    self.pc += 2;
    Cycle::TWO
  }

  /// RES u3,[HL]
  ///
  /// Set bit u3 in the byte pointed by HL to 0. Bit 0 is the rightmost one, bit 7 the leftmost one.
  fn res_u_hl(&mut self, bit: u8) -> Cycle {
    let addr = self.read16reg(&Reg16::HL) as usize;
    let value = self.mem.get_addr(addr) & !(1 << bit);
    self.mem.write_addr(addr, value);

    self.pc += 2;
    Cycle::FOUR
  }

  /// SET u3,r8
  ///
  /// Set bit u3 in register r8 to 1. Bit 0 is the rightmost one, bit 7 the leftmost one.
  fn set_u_r8(&mut self, bit: u8, reg: Reg8) -> Cycle {
    let value = self.read8reg(&reg) | (1 << bit);
    self.write8reg(&reg, value);
    self.pc += 2;
    Cycle::TWO
  }

  /// SET u3,[HL]
  ///
  /// Set bit u3 in the byte pointed by HL to 1. Bit 0 is the rightmost one, bit 7 the leftmost one.
  fn set_u_hl(&mut self, bit: u8) -> Cycle {
    let addr = self.read16reg(&Reg16::HL) as usize;
    let value = self.mem.get_addr(addr) | (1 << bit);
    self.mem.write_addr(addr, value);

    self.pc += 2;
    Cycle::FOUR
  }

  /// SWAP r8
  ///
  /// Swap upper 4 bits in register r8 and the lower 4 ones.
  fn swap_r8(&mut self, reg: Reg8) -> Cycle {
    let value = self.read8reg(&reg);
    let new_value = (value >> 4) | (value << 4);
    self.write8reg(&reg, new_value);

    self.set_zf(new_value == 0);
    self.set_nf(false);
    self.set_hf(false);
    self.set_cf(false);

    self.pc += 2;
    Cycle::TWO
  }

  /// SWAP [HL]
  ///
  /// Swap upper 4 bits in the byte pointed by HL and the lower 4 ones.
  fn swap_hl(&mut self) -> Cycle {
    let addr = self.read16reg(&Reg16::HL) as usize;
    let value = self.mem.get_addr(addr);
    let new_value = (value >> 4) | (value << 4);
    self.mem.write_addr(addr, new_value);

    self.set_zf(new_value == 0);
    self.set_nf(false);
    self.set_hf(false);
    self.set_cf(false);

    self.pc += 2;
    Cycle::FOUR
  }

  /* Bit Shift Instructions */

  /// RL r8
  ///
  /// Rotate bits in register r8 left through carry.
  fn rl_r8(&mut self, reg: Reg8) -> Cycle {
    let value = self.read8reg(&reg);
    let carry = self.get_flag(Flags::CARRY) as u8;
    let new_carry = (value & 0x80) >> 7 == 0x01;
    let new_value = (value << 1) + carry;

    self.set_zf(new_value == 0);
    self.set_nf(false);
    self.set_hf(false);
    self.set_zf(new_carry);

    self.pc += 2;
    Cycle::TWO
  }

  /// RL [HL]
  ///
  /// Rotate byte pointed to by HL left through carry.
  fn rl_hl(&mut self) -> Cycle {
    let addr = self.read16reg(&Reg16::HL);
    let value = self.mem.get_addr(addr as usize);
    let carry = self.get_flag(Flags::CARRY) as u8;
    let new_carry = (value & 0x80) >> 7 == 0x01;
    let new_value = (value << 1) + carry;

    self.set_zf(new_value == 0);
    self.set_nf(false);
    self.set_hf(false);
    self.set_zf(new_carry);

    self.pc += 2;
    Cycle::FOUR
  }

  /// RLA
  ///
  /// Rotate register A left through carry.
  fn rla(&mut self) -> Cycle {
    let value = self.a;
    let new_carry = (value & 0x80) >> 7 == 0x01;

    self.set_zf(false);
    self.set_nf(false);
    self.set_hf(false);
    self.set_zf(new_carry);

    self.pc += 1;
    Cycle::ONE
  }

  /// RLC r8, and RLCA
  ///
  /// Rotate register r8/A left.
  fn rlc(&mut self, reg: Reg8) -> Cycle {
    let value = self.read8reg(&reg);
    let carry = value & 0x80;
    let new_value = value.rotate_left(1);
    self.write8reg(&reg, new_value);
    let z = match reg {
      Reg8::A => false,
      _ => new_value == 0,
    };

    self.set_zf(z);
    self.set_nf(false);
    self.set_hf(false);
    self.set_cf(carry != 0);

    self.pc += match reg {
      Reg8::A => 1,
      _ => 2,
    };
    match reg {
      Reg8::A => Cycle::ONE,
      _ => Cycle::TWO,
    }
  }

  ///    RLC [HL]
  ///
  /// Rotate byte pointed to by HL left.
  fn rlc_hl(&mut self) -> Cycle {
    let addr = self.read16reg(&Reg16::HL) as usize;
    let value = self.mem.get_addr(addr);
    let carry = value & 0x80;
    let new_value = value.rotate_left(1);
    self.mem.write_addr(addr, new_value);

    self.set_zf(new_value == 0);
    self.set_nf(false);
    self.set_hf(false);
    self.set_cf(carry != 0);

    self.pc += 2;
    Cycle::FOUR
  }

  ///  RR r8
  ///
  /// Rotate register r8 right through carry.
  fn rr_r8(&mut self, reg: Reg8) -> Cycle {
    let value = self.read8reg(&reg);
    let carry = self.get_flag(Flags::CARRY);
    let new_carry = value & 0x01 == 0x01;
    let new_value = if carry {
      0x80 | (value >> 1)
    } else {
      value >> 1
    };
    self.write8reg(&reg, new_value);

    self.set_zf(new_value == 0);
    self.set_nf(false);
    self.set_hf(false);
    self.set_zf(new_carry);

    self.pc += 2;
    Cycle::TWO
  }

  ///  RR [HL]
  ///
  /// Rotate byte pointed to by HL right through carry.
  fn rr_hl(&mut self) -> Cycle {
    let addr = self.read16reg(&Reg16::HL) as usize;
    let value = self.mem.get_addr(addr);
    let carry = self.get_flag(Flags::CARRY);
    let new_carry = value & 0x01 == 0x01;
    let new_value = if carry {
      0x80 | (value >> 1)
    } else {
      value >> 1
    };
    self.mem.write_addr(addr, new_value);

    self.set_zf(new_value == 0);
    self.set_nf(false);
    self.set_hf(false);
    self.set_zf(new_carry);

    self.pc += 2;
    Cycle::FOUR
  }

  ///  RRA
  ///
  /// Rotate register A right through carry.
  fn rra(&mut self) -> Cycle {
    let value = self.a;
    let carry = self.get_flag(Flags::CARRY);
    let new_carry = value & 0x01 == 0x01;
    let new_value = if carry {
      0x80 | (value >> 1)
    } else {
      value >> 1
    };
    self.a = new_value;

    self.set_zf(false);
    self.set_nf(false);
    self.set_hf(false);
    self.set_zf(new_carry);

    self.pc += 1;
    Cycle::ONE
  }

  /// RRC r8, and RRCA
  ///
  /// Rotate register r8/A right.
  fn rrc(&mut self, reg: Reg8) -> Cycle {
    let value = self.read8reg(&reg);
    let carry = value & 0x01;
    let new_value = value.rotate_right(1);
    self.write8reg(&reg, new_value);
    let z = match reg {
      Reg8::A => false,
      _ => new_value == 0,
    };

    self.set_zf(z);
    self.set_nf(false);
    self.set_hf(false);
    self.set_cf(carry != 0);

    self.pc += match reg {
      Reg8::A => 1,
      _ => 2,
    };
    match reg {
      Reg8::A => Cycle::ONE,
      _ => Cycle::TWO,
    }
  }

  ///  RRC [HL]
  ///
  /// Rotate byte pointed to by HL right.
  fn rrc_hl(&mut self) -> Cycle {
    let addr = self.read16reg(&Reg16::HL) as usize;
    let value = self.mem.get_addr(addr);
    let carry = value & 0x01;
    let new_value = value.rotate_right(1);
    self.mem.write_addr(addr, new_value);

    self.set_zf(new_value == 0);
    self.set_nf(false);
    self.set_hf(false);
    self.set_cf(carry != 0);

    self.pc += 2;
    Cycle::FOUR
  }

  ///  SLA r8
  ///
  /// Shift Left Arithmetic register r8.
  fn sla_r8(&mut self, reg: Reg8) -> Cycle {
    let value = self.read8reg(&reg);
    let carry = value & 0x80;
    let new_value = value << 1;
    self.write8reg(&reg, new_value);

    self.set_zf(new_value == 0);
    self.set_nf(false);
    self.set_hf(false);
    self.set_cf(carry != 0);

    self.pc += 2;
    Cycle::TWO
  }

  ///  SLA [HL]
  ///
  /// Shift Left Arithmetic byte pointed to by HL.
  fn sla_hl(&mut self) -> Cycle {
    let addr = self.read16reg(&Reg16::HL) as usize;
    let value = self.mem.get_addr(addr);
    let carry = value & 0x80;
    let new_value = value << 1;
    self.mem.write_addr(addr, new_value);

    self.set_zf(new_value == 0);
    self.set_nf(false);
    self.set_hf(false);
    self.set_cf(carry != 0);

    self.pc += 2;
    Cycle::FOUR
  }

  ///  SRA r8
  ///
  /// Shift Right Arithmetic register r8.
  fn sra_r8(&mut self, reg: Reg8) -> Cycle {
    let value = self.read8reg(&reg);
    let carry = value & 0x01;
    let msb = value & 0x80;
    let new_value = value >> 1 | msb;
    self.write8reg(&reg, new_value);

    self.set_zf(new_value == 0);
    self.set_nf(false);
    self.set_hf(false);
    self.set_cf(carry != 0);

    self.pc += 2;
    Cycle::TWO
  }

  ///  SRA [HL]
  ///
  /// Shift Right Arithmetic byte pointed to by HL.
  fn sra_hl(&mut self) -> Cycle {
    let addr = self.read16reg(&Reg16::HL) as usize;
    let value = self.mem.get_addr(addr);
    let carry = value & 0x01;
    let msb = value & 0x80;
    let new_value = value >> 1 | msb;
    self.mem.write_addr(addr, new_value);

    self.set_zf(new_value == 0);
    self.set_nf(false);
    self.set_hf(false);
    self.set_cf(carry != 0);

    self.pc += 2;
    Cycle::FOUR
  }

  ///  SRL r8
  ///
  /// Shift Right Logic register r8.
  fn srl_r8(&mut self, reg: Reg8) -> Cycle {
    let value = self.read8reg(&reg);
    let carry = value & 0x01;
    let new_value = value >> 1;
    self.write8reg(&reg, new_value);

    self.set_zf(new_value == 0);
    self.set_nf(false);
    self.set_hf(false);
    self.set_cf(carry != 0);

    self.pc += 2;
    Cycle::TWO
  }
  ///  SRL [HL]
  ///
  /// Shift Right Logic byte pointed to by HL.
  fn srl_hl(&mut self) -> Cycle {
    let addr = self.read16reg(&Reg16::HL) as usize;
    let value = self.mem.get_addr(addr);
    let carry = value & 0x01;
    let new_value = value >> 1;
    self.mem.write_addr(addr, new_value);

    self.set_zf(new_value == 0);
    self.set_nf(false);
    self.set_hf(false);
    self.set_cf(carry != 0);

    self.pc += 2;
    Cycle::FOUR
  }

  /* Load Instructions */

  /// LD r8,r8
  ///
  /// Load (copy) value in register on the right into register on the left.
  fn ld_r8_r8(&mut self, reg: Reg8, copy_reg: Reg8) -> Cycle {
    let value = self.read8reg(&copy_reg);
    self.write8reg(&reg, value);

    self.pc += 1;
    Cycle::ONE
  }

  /// LD r8,n8
  ///
  /// Load value n8 into register r8.
  fn ld_r8_n8(&mut self, reg: Reg8, value: u8) -> Cycle {
    self.write8reg(&reg, value);

    self.pc += 2;
    Cycle::TWO
  }

  /// LD r16,n16
  ///
  /// Load value n16 into register r16.
  fn ld_16(&mut self, reg: Reg16, value: u16) -> Cycle {
    self.write16reg(&reg, value);

    self.pc += 3;
    Cycle::THREE
  }

  /// LD [HL],r8
  ///
  /// Store value in register r8 into byte pointed to by register HL.
  fn ld_hl_r8(&mut self, reg: Reg8) -> Cycle {
    let addr = self.read16reg(&Reg16::HL) as usize;
    let value = self.read8reg(&reg);
    self.mem.write_addr(addr, value);

    self.pc += 1;
    Cycle::TWO
  }

  /// LD [HL],n8
  ///
  /// Store value n8 into byte pointed to by register HL.
  fn ld_hl_n8(&mut self, value: u8) -> Cycle {
    let addr = self.read16reg(&Reg16::HL) as usize;
    self.mem.write_addr(addr, value);

    self.pc += 2;
    Cycle::THREE
  }

  /// LD r8,[HL]
  ///
  /// Load value into register r8 from byte pointed to by register HL.
  fn ld_r8_hl(&mut self, reg: Reg8) -> Cycle {
    let addr = self.read16reg(&Reg16::HL) as usize;
    let value = self.mem.get_addr(addr);
    self.write8reg(&reg, value);

    self.pc += 1;
    Cycle::TWO
  }

  /// LD [r16], A
  ///
  /// Store value in register A into byte pointed to by register r16.
  fn ld_r16_a(&mut self, reg: Reg16) -> Cycle {
    let value = self.a;
    let addr = self.read16reg(&reg);
    self.mem.write_addr(addr as usize, value);

    self.pc += 1;
    Cycle::TWO
  }

  /// LD [n16],A
  ///
  /// Store value in register A into byte at address n16.
  fn ld_nn_a(&mut self, addr: u16) -> Cycle {
    let value = self.a;
    self.mem.write_addr(addr as usize, value);

    self.pc += 3;
    Cycle::FOUR
  }

  /// LDH [n16],A
  ///
  /// Store value in register A into byte at address n16, provided it is between $FF00 and $FFFF.
  fn ldh_nn_a(&mut self, value: u16) -> Cycle {
    let addr = 0xff00 | self.mem.get_addr(value as usize) as u16;
    self.mem.write_addr(addr as usize, self.a);

    self.pc += 2;
    Cycle::THREE
  }

  /// LDH [C],A
  ///
  /// Store value in register A into byte at address $FF00+C.
  fn ldh_c_a(&mut self) -> Cycle {
    let addr = 0xff00 | self.c as u16;
    self.mem.write_addr(addr as usize, self.a);

    self.pc += 1;
    Cycle::TWO
  }

  /// LD A,[r16]
  ///
  /// Load value in register A from byte pointed to by register r16.
  fn ld_a_rr(&mut self, reg: Reg16) -> Cycle {
    let addr = self.read16reg(&reg);
    let value = self.mem.get_addr(addr as usize);
    self.write8reg(&Reg8::A, value);
    self.pc += 1;
    Cycle::TWO
  }

  /// LD A,[n16]
  ///
  /// Load value in register A from byte at address n16.
  fn ld_nn(&mut self, addr: u16) -> Cycle {
    let value = self.mem.get_addr(addr as usize);
    self.a = value;

    self.pc += 3;
    Cycle::FOUR
  }

  /// LDH A,[n16]
  ///
  /// Load value in register A from byte at address n16, provided it is between $FF00 and $FFFF.
  fn ldh_a_nn(&mut self, value: u16) -> Cycle {
    let addr = 0xff00 | value;
    self.a = self.mem.get_addr(addr as usize);

    self.pc += 2;
    Cycle::THREE
  }

  /// LDH A,[C]
  ///
  /// Load value in register A from byte at address $FF00+c
  fn ldh_a_c(&mut self) -> Cycle {
    let addr = 0xff00 | self.c as u16;
    self.a = self.mem.get_addr(addr as usize);

    self.pc += 1;
    Cycle::TWO
  }

  /// LD [HLI],A
  ///
  /// Store value in register A into byte pointed by HL and increment HL afterwards.
  fn ld_hli_a(&mut self) -> Cycle {
    let addr = self.read16reg(&Reg16::HL);
    self.mem.write_addr(addr as usize, self.a);
    self.write16reg(&Reg16::HL, addr + 1);

    self.pc += 1;
    Cycle::TWO
  }

  /// LD [HLD],A
  ///
  /// Store value in register A into byte pointed by HL and decrement HL afterwards.
  fn ld_hld_a(&mut self) -> Cycle {
    let addr = self.read16reg(&Reg16::HL);
    self.mem.write_addr(addr as usize, self.a);
    self.write16reg(&Reg16::HL, addr.wrapping_sub(1));

    self.pc += 1;
    Cycle::TWO
  }

  /// LD A,[HLI]
  ///
  /// Load value into register A from byte pointed by HL and decrement HL afterwards.
  fn ld_a_hli(&mut self) -> Cycle {
    let addr = self.read16reg(&Reg16::HL);
    let value = self.mem.get_addr(addr as usize);
    self.a = value;
    self.write16reg(&Reg16::HL, addr + 1);

    self.pc += 1;
    Cycle::TWO
  }
  /// LD A,[HLD]
  ///
  /// Load value into register A from byte pointed by HL and increment HL afterwards.
  fn ld_a_hld(&mut self) -> Cycle {
    let addr = self.read16reg(&Reg16::HL);
    let value = self.mem.get_addr(addr as usize);
    self.a = value;
    self.write16reg(&Reg16::HL, addr - 1);

    self.pc += 1;
    Cycle::TWO
  }

  /* Jumps and Subroutines */

  /// CALL n16
  ///
  /// Call address n16. This pushes the address of the instruction after the CALL on the stack, such that RET can pop it later; then, it executes an implicit JP n16.
  fn call_nn(&mut self, value: u16) -> Cycle {
    self.sp = self.sp.wrapping_sub(2);
    self.mem.write_word(self.sp as usize, self.pc);
    self.pc = value;

    Cycle::SIX
  }

  /// CALL cc,n16
  ///
  /// Call address n16 if condition cc is met.
  fn call_cc_nn(&mut self, condition: bool, value: u16) -> Cycle {
    if condition {
      self.sp -= 2;
      self.mem.write_word(self.sp as usize, self.pc);
      self.pc = value;

      Cycle::SIX
    } else {
      self.pc += 3;
      Cycle::THREE
    }
  }

  /// JP HL
  ///
  /// Jump to address in HL; effectively, load PC with value in register HL.
  fn jp_hl(&mut self) -> Cycle {
    let value = self.read16reg(&Reg16::HL);
    self.pc = value;

    Cycle::ONE
  }
  /// JP n16
  ///
  /// Jump to address n16; effectively, store n16 into PC.
  fn jp_nn(&mut self, value: u16) -> Cycle {
    self.pc = value;
    Cycle::FOUR
  }
  /// JP cc,n16
  ///
  /// Jump to address n16 if condition cc is met.
  fn jp_cc_nn(&mut self, condition: bool, value: u16) -> Cycle {
    if condition {
      self.pc = value;
      Cycle::FOUR
    } else {
      self.pc += 3;
      Cycle::THREE
    }
  }
  /// JR e8
  ///
  /// Relative Jump by adding e8 to the address of the instruction following the JR. To clarify, an operand of 0 is equivalent to no jumping.
  fn jr_e8(&mut self, value: i8) -> Cycle {
    self.pc = self.pc.wrapping_add(value as u16);
    Cycle::THREE
  }

  /// JR cc,e8
  ///
  /// Relative Jump by adding e8 to the current address if condition cc is met.
  fn jr_cc_e8(&mut self, condition: bool, value: i8) -> Cycle {
    if condition {
      self.pc = self.pc.wrapping_add(value as u16);
      self.pc += 2;
      Cycle::THREE
    } else {
      self.pc += 2;
      Cycle::TWO
    }
  }

  /// RET cc
  ///
  /// Return from subroutine if condition cc is met.
  fn ret_cc(&mut self, cond: bool) -> Cycle {
    if cond {
      self.pc = self.mem.get_word(self.sp as usize);
      self.sp += 2;
      self.pc += 1;
      Cycle::FIVE
    } else {
      self.pc += 1;
      Cycle::TWO
    }
  }

  /// RET
  ///
  /// Return from subroutine. This is basically a POP PC (if such an instruction existed). See POP r16 for an explanation of how POP works.
  fn ret(&mut self) -> Cycle {
    self.pc = self.mem.get_word(self.sp as usize);
    self.sp += 2;

    self.pc += 1;
    Cycle::FOUR
  }

  /// RETI
  ///
  /// Return from subroutine and enable interrupts. This is basically equivalent to executing EI then RET, meaning that IME is set right after this instruction.
  fn reti(&mut self) -> Cycle {
    let cycle = self.ret();
    self.ime = true;
    cycle
  }

  /// RST vec
  ///
  /// Call address vec. This is a shorter and faster equivalent to CALL for suitable values of vec.
  fn rst(&mut self, value: u8) -> Cycle {
    self.sp -= 2;
    self.mem.write_word(self.sp as usize, self.pc);
    self.pc = value as u16;

    Cycle::FOUR
  }

  /* Stack Operations Instructions */

  /// ADD HL,SP
  ///
  /// Add the value in SP to HL.
  fn add_hl_sp(&mut self) -> Cycle {
    let new_value = self.read16reg(&Reg16::HL).wrapping_add(self.sp);
    self.write16reg(&Reg16::HL, new_value);

    self.pc += 1;
    Cycle::TWO
  }

  ///  ADD SP,e8
  ///
  /// Add the signed value e8 to SP.
  fn add_sp_e8(&mut self, value: i8) -> Cycle {
    self.sp = self.sp.wrapping_add(value as u16);

    self.pc += 2;
    Cycle::FOUR
  }

  ///  DEC SP
  ///
  /// Decrement value in register SP by 1.
  fn dec_sp(&mut self) -> Cycle {
    self.sp -= 1;

    self.pc += 1;
    Cycle::TWO
  }

  ///  INC SP
  ///
  /// Increment value in register SP by 1.
  fn inc_sp(&mut self) -> Cycle {
    self.sp += 1;

    self.pc += 1;
    Cycle::TWO
  }

  ///  LD SP,n16
  ///
  /// Load value n16 into register SP.
  fn ld_sp_nn(&mut self, value: u16) -> Cycle {
    self.sp = value;

    self.pc += 3;
    Cycle::THREE
  }

  /// LD [n16],SP
  ///
  /// Store SP & $FF at address n16 and SP >> 8 at address n16 + 1.
  fn ld_nn_sp(&mut self, addr: u16, value: u16) -> Cycle {
    let low = value as u8;
    let high = (value >> 8) as u8;
    self.mem.write_addr(addr as usize, low);
    self.mem.write_addr((addr + 1) as usize, high);
    self.pc += 3;
    Cycle::FIVE
  }

  ///  LD HL,SP+e8
  ///
  /// Add the signed value e8 to SP and store the result in HL.
  fn ld_hl_sp_plus(&mut self, value: i8) -> Cycle {
    self.sp = self.sp.wrapping_add(value as u16);
    self.write16reg(&Reg16::HL, self.sp);

    self.set_zf(false);
    self.set_nf(false);
    // TODO: Write flags for hf and cf, too tired

    self.pc += 2;
    Cycle::THREE
  }

  ///  LD SP,HL
  ///
  /// Load register HL into register SP.
  fn ld_sp_hl(&mut self) -> Cycle {
    self.sp = self.read16reg(&Reg16::HL);

    self.pc += 1;
    Cycle::TWO
  }

  ///  POP AF
  ///
  /// Pop register AF from the stack.
  fn pop_af(&mut self) -> Cycle {
    let value = self.mem.get_word(self.sp as usize);
    self.write16reg(&Reg16::AF, value);
    self.sp += 2;

    // TODO: set flags, too tired right now

    self.pc += 1;
    Cycle::THREE
  }

  ///  POP r16
  ///
  /// Pop register r16 from the stack. This is roughly equivalent to the following imaginary instructions:
  ///
  /// ld LOW(r16), [sp] ; C, E or L
  ///
  /// inc sp
  ///
  /// ld HIGH(r16), [sp] ; B, D or H
  ///
  /// inc sp
  fn pop_r16(&mut self, reg: Reg16) -> Cycle {
    let value = self.mem.get_word(self.sp as usize);
    self.write16reg(&reg, value);
    self.sp += 2;

    self.pc += 1;
    Cycle::THREE
  }

  ///  PUSH AF
  ///
  /// Push register AF into the stack (Just use r16)

  ///  PUSH r16
  ///
  /// Push register r16 into the stack. (Also covers AF, no flags are different)
  fn push_r16(&mut self, reg: Reg16) -> Cycle {
    self.sp -= 2;
    let value = self.read16reg(&reg);
    self.mem.write_word(self.sp as usize, value);

    self.pc += 1;
    Cycle::FOUR
  }

  /* Miscellaneous Instructions */

  ///    CCF
  ///
  /// Complement Carry Flag.
  fn ccf(&mut self) -> Cycle {
    self.set_nf(false);
    self.set_hf(false);
    self.set_cf(!self.get_flag(Flags::CARRY));

    self.pc += 1;
    Cycle::ONE
  }

  ///  CPL
  ///
  /// ComPLement accumulator (A = ~A).
  fn cpl(&mut self) -> Cycle {
    self.a = !self.a;

    self.set_nf(false);
    self.set_hf(false);

    self.pc += 1;
    Cycle::ONE
  }

  ///  DAA
  ///
  /// Decimal Adjust Accumulator to get a correct BCD representation after an arithmetic instruction.
  fn daa(&mut self) -> Cycle {
    // I don't fully understand this instruction, so this is borrowed basically direct from mooneye
    let mut carry = false;
    if !self.get_flag(Flags::ADD_SUBTRACT) {
      if self.get_flag(Flags::CARRY) || self.a > 0x99 {
        self.a = self.a.wrapping_add(0x60);
        carry = true;
      }
      if self.get_flag(Flags::HALF_CARRY) || self.a & 0x0f > 0x09 {
        self.a = self.a.wrapping_add(0x06);
      }
    } else if self.get_flag(Flags::CARRY) {
      carry = true;
      self.a = self.a.wrapping_add(if self.get_flag(Flags::HALF_CARRY) {
        0x9a
      } else {
        0xa0
      });
    } else if self.get_flag(Flags::HALF_CARRY) {
      self.a = self.a.wrapping_add(0xfa);
    }

    self.set_zf(self.a == 0);
    self.set_hf(false);
    self.set_cf(carry);

    self.pc += 1;
    Cycle::ONE
  }

  ///  DI
  ///
  /// Disable Interrupts by clearing the IME flag.
  fn di(&mut self) -> Cycle {
    self.ime = false;

    self.pc += 1;
    Cycle::ONE
  }

  ///  EI
  ///
  /// Enable Interrupts by setting the IME flag. The flag is only set after the instruction following EI.
  fn ei(&mut self) -> Cycle {
    self.ime = true;

    self.pc += 1;
    Cycle::ONE
  }

  ///  HALT
  ///
  /// Enter CPU low-power consumption mode until an interrupt occurs. The exact behavior of this instruction depends on the state of the IME flag.
  ///
  /// - IME set:
  /// The CPU enters low-power mode until after an interrupt is about to be serviced. The handler is executed normally, and the CPU resumes execution after the HALT when that returns.
  ///
  /// - IME not set:
  /// The behavior depends on whether an interrupt is pending (i.e. ‘[IE] & [IF]’ is non-zero).
  ///
  /// - - None pending:
  ///     As soon as an interrupt becomes pending, the CPU resumes execution. This is like the above, except that the handler is not called.
  ///
  /// - - Some pending:
  ///     The CPU continues execution after the HALT, but the byte after it is read twice in a row (PC is not incremented, due to a hardware bug).
  fn halt(&mut self) -> Cycle {
    panic!("HALT!");
  }
  /// NOP
  ///
  /// No Operation
  fn nop(&mut self) -> Cycle {
    self.pc += 1;
    Cycle::ONE
  }

  /// SCF
  ///
  /// Set Carry Flag.
  fn scf(&mut self) -> Cycle {
    self.set_nf(false);
    self.set_hf(false);
    self.set_cf(true);

    self.pc += 1;
    Cycle::ONE
  }

  /// STOP
  ///
  ///  go into low power mode?
  fn stop(&mut self) -> Cycle {
    panic!("STOP");
  }
}

#[test]
fn nop_increments() {
  let mem = MemoryBus::new();
  let mut cpu = Cpu::new(mem);

  cpu.nop();

  assert_eq!(cpu.pc, 1)
}
