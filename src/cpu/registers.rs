use crate::cpu::Flags;

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
    SP,
    PC,
}
#[derive(Copy, Clone)]
pub struct Reg16Value {
    pub val: u16,
}
#[derive(Copy, Clone)]
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
impl Registers {
    pub fn new() -> Self {
        return Self {
            af: Reg16Value { val: 0x01B0 },
            bc: Reg16Value { val: 0x0013 },
            de: Reg16Value { val: 0x00D8 },
            hl: Reg16Value { val: 0x014D },
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
    pub fn advance_pc(&mut self, amount: usize) {
        self.pc.val += amount as u16;
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
