use crate::memory::MemoryBus;
use bitflags::bitflags;

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

enum Cycle {
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

  pub fn execute(&mut self, opcode: u8) {
    use Reg16::*;
    use Reg8::*;
    let lsb = self.mem.get_addr((self.pc + 1) as usize);
    let msb = self.mem.get_addr((self.pc + 2) as usize);
    let nn: u16 = (lsb as u16) & ((msb as u16) << 8);
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
      0x0A => self.ld_a_rr(BC),
      0x0B => self.dec_16(BC),
      0x0C => self.inc_8(C),
      0x0D => self.dec_8(C),
      0x0E => self.ld_r8_n8(C, lsb),
      0x0F => self.rrc(A),
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
      0x1A => self.ld_a_rr(DE),
      0x1B => self.dec_16(DE),
      0x1C => self.inc_8(E),
      0x1D => self.dec_8(E),
      0x1E => self.ld_r8_n8(E, lsb),
      0x1F => self.rra(),
      0x31 => self.ld_sp_nn(nn),
      0x49 => self.ld_r8_r8(C, C),
      _ => panic!("you need to handle opcode {}", opcode),
    };
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

  /// BIT u3,[HL]

  /// RES u3,r8

  /// RES u3,[HL]

  /// SET u3,r8

  /// SET u3,[HL]

  /// SWAP r8

  /// SWAP [HL]

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
    let carry = self.get_flag(Flags::CARRY) as u8;
    let new_carry = (value & 0x80) >> 7 == 0x01;
    let new_value = (value << 1) + carry;

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

  ///  RLCA

  ///  RR r8

  ///  RR [HL]

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

  ///  RRCA

  ///  SLA r8

  ///  SLA [HL]

  ///  SRA r8

  ///  SRA [HL]

  ///  SRL r8

  ///  SRL [HL]

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
  /* No idea what this or the next one does */

  /// LDH [C],A

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

  /// LDH A,[C]

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
    self.write16reg(&Reg16::HL, addr - 1);

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
    self.sp -= 2;
    self.mem.write_word(self.sp as usize, self.pc);
    self.pc = value;

    Cycle::SIX
  }

  /// CALL cc,n16
  ///
  /// Call address n16 if condition cc is met.
  fn call_cc_nn(&mut self, flag: Flags, value: u16) -> Cycle {
    let condition = self.get_flag(flag);
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
  fn jp_cc_nn(&mut self, flag: Flags, value: u16) -> Cycle {
    let condition = self.get_flag(flag);
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
  fn jr_cc_e8(&mut self, flag: Flags, value: i8) -> Cycle {
    let condition = self.get_flag(flag);
    if condition {
      self.pc = self.pc.wrapping_add(value as u16);
      Cycle::THREE
    } else {
      self.pc += 2;
      Cycle::TWO
    }
  }

  /// RET cc
  ///
  /// Return from subroutine if condition cc is met.

  /// RET
  ///
  /// Return from subroutine. This is basically a POP PC (if such an instruction existed). See POP r16 for an explanation of how POP works.

  /// RETI

  /// RST vec

  /* Stack Operations Instructions */

  ///    ADD HL,SP

  ///  ADD SP,e8

  ///  DEC SP

  ///  INC SP

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

  ///    LD HL,SP+e8

  ///  LD SP,HL

  ///  POP AF

  ///  POP r16

  ///  PUSH AF

  ///  PUSH r16

  /* Miscellaneous Instructions */

  ///    CCF

  ///  CPL

  ///  DAA

  ///  DI

  ///  EI

  ///  HALT

  /// NOP
  ///
  /// No Operation
  fn nop(&mut self) -> Cycle {
    self.pc += 1;
    Cycle::ONE
  }

  /// SCF

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
