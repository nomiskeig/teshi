use std::{
    fmt::{self, LowerHex},
    ops::{Add, Sub},
};

use crate::{cpu::Flags, memory::MemValue16};

fn get_first_8(val: Reg16Value) -> Reg8Value {
    return Reg8Value {
        val: (val.val >> 0x8) as u8,
    };
}

fn get_second_8(val: Reg16Value) -> Reg8Value {
    return Reg8Value {
        val: (val.val & 0xFF) as u8,
    };
}
fn set_first_8(val: Reg8Value, orig: Reg16Value) -> Reg16Value {
    return Reg16Value {
        val: ((val.val as u16) << 0x8) | (orig.val & 0xFF),
    };
}
fn set_second_8(val: Reg8Value, orig: Reg16Value) -> Reg16Value {
    return Reg16Value {
        val: val.val as u16 | (orig.val & 0xFF00),
    };
}

#[derive(Debug, Clone, Copy)]
pub enum Reg8 {
    A,
    F,
    B,
    C,
    D,
    E,
    H,
    L,
}



#[derive(Debug, Clone, Copy)]
pub enum Reg16 {
    AF,
    BC,
    DE,
    HL,
    SP,
    PC,
}
#[derive(Debug, Clone, Copy)]
pub enum Flag {
    N, Z, H, C
}
impl fmt::Display for Reg16 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Reg16::AF => write!(f, "af"),
            Reg16::BC => write!(f, "bc"),
            Reg16::DE => write!(f, "de"),
            Reg16::HL => write!(f, "hl"),
            Reg16::SP => write!(f, "sp"),
            Reg16::PC => write!(f, "pc"),
        }
    }
}
impl fmt::Display for Reg8 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Reg8::A => write!(f, "a"),
            Reg8::F => write!(f, "f"),
            Reg8::B => write!(f, "b"),
            Reg8::C => write!(f, "c"),
            Reg8::D => write!(f, "d"),
            Reg8::E => write!(f, "e"),
            Reg8::H => write!(f, "h"),
            Reg8::L => write!(f, "l"),
        }
    }
}
#[derive(Debug, Copy, Clone)]
pub struct Reg16Value {
    pub val: u16,
}
#[derive(Debug, Copy, Clone)]
pub struct Reg8Value {
    pub val: u8,
}

#[derive(Copy, Clone)]
pub struct Registers {
    af: Reg16Value,
    bc: Reg16Value,
    de: Reg16Value,
    hl: Reg16Value,
    sp: Reg16Value,
    pc: Reg16Value,
    flags: Flags,
}
fn get_flag_index(flag: Flag) -> i32{
        match flag {
            Flag::N => return 6,
            Flag::Z => return 7,
            Flag::H => return 5,
            Flag::C => return 4,
        };
    
}
impl Registers {
    pub fn new() -> Self {
        return Self {
            af: Reg16Value { val: 0x1180 },
            bc: Reg16Value { val: 0x0000 },
            de: Reg16Value { val: 0xFF56 },
            hl: Reg16Value { val: 0x000D },
            sp: Reg16Value { val: 0xFFFE },
            pc: Reg16Value { val: 0x0100 },
            flags: Flags {
                half_carry: false,
                carry: false,
                zero: false,
                sub: false,
            },
        };
    }
    pub fn get_flag(&self, flag: Flag) -> bool {

        let index = get_flag_index(flag);
        return self.af.val & (0x1 << index) != 0;
        
    }
    pub fn set_flag(&mut self, flag: Flag) {
        let index = get_flag_index(flag);
        self.af.val = self.af.val | (0x1 << index);
    }
    pub fn clear_flag(&mut self, flag: Flag) {
        let index = get_flag_index(flag);
        self.af.val = self.af.val & !(0x1 << index);
    }
    pub fn advance_pc(&mut self, amount: i32) {
        if (amount >= 0) {
        self.pc.val += amount as u16;
        } else  {
            self.pc.val -= -amount as u16;
        }
    }
    pub fn set_reg_16(&mut self, reg: Reg16, val: Reg16Value) {
        match reg {
            Reg16::AF => self.af = val,
            Reg16::BC => self.bc = val,
            Reg16::DE => self.de = val,
            Reg16::HL => self.hl = val,
            Reg16::SP => self.sp = val,
            Reg16::PC => self.pc = val,
        }
    }
    pub fn set_reg_8(&mut self, reg: Reg8, val: Reg8Value) {
        match reg {
            Reg8::A => self.af = set_first_8(val, self.af),
            Reg8::F => self.af = set_second_8(val, self.af),
            Reg8::B => self.bc = set_first_8(val, self.bc),
            Reg8::C => self.bc = set_second_8(val, self.bc),
            Reg8::D => self.de = set_first_8(val, self.de),
            Reg8::E => self.de = set_second_8(val, self.de),
            Reg8::H => self.hl = set_first_8(val, self.hl),
            Reg8::L => self.hl = set_second_8(val, self.hl),
        }
    }
    pub fn get_reg_16(&self, reg: Reg16) -> Reg16Value {
        match reg {
            Reg16::AF => self.af,
            Reg16::BC => self.bc,
            Reg16::DE => self.de,
            Reg16::HL => self.hl,
            Reg16::SP => self.sp,
            Reg16::PC => self.pc,
        }
    }
    pub fn get_reg_8(&self, reg: Reg8) -> Reg8Value {
        match reg {
            Reg8::A => Reg8Value {
                val: (self.af.val >> 8) as u8,
            },
            Reg8::F => Reg8Value {
                val: (self.af.val & 0xFF) as u8,
            },
            Reg8::B => Reg8Value {
                val: (self.bc.val >> 8) as u8,
            },
            Reg8::C => Reg8Value {
                val: (self.bc.val & 0xFF) as u8,
            },
            Reg8::D => Reg8Value {
                val: (self.de.val >> 8) as u8,
            },
            Reg8::E => Reg8Value {
                val: (self.de.val & 0xFF) as u8,
            },
            Reg8::H => Reg8Value {
                val: (self.hl.val >> 8) as u8,
            },
            Reg8::L => Reg8Value {
                val: (self.hl.val & 0xFF) as u8,
            },
        }
    }
}

impl LowerHex for Reg8Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:x}", self.val);
        Ok(())
    }
}

impl LowerHex for Reg16Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:x}", self.val);
        Ok(())
    }
}

impl Add<i32> for Reg16Value {
    type Output = Reg16Value;

    fn add(self, rhs: i32) -> Self::Output {
        return Self {
            val: self.val + rhs as u16,
        };
    }
}

impl Sub<i32> for Reg16Value {
    type Output = Reg16Value;

    fn sub(self, rhs: i32) -> Self::Output {
        return Self {
            val: self.val - rhs as u16,
        };
    }
}
