pub mod decode;
pub mod registers;

use std::fmt::{self, format};

use anyhow::anyhow;
use decode::decode_instruction;
use memmap2::Mmap;
use registers::{Flag, Reg8, Reg8Value, Reg16, Reg16Value, Registers};

use crate::memory::{Address, HIGH_MEM_START, MemValue8, MemValue16, Memory};

#[derive(Debug, Copy, Clone)]
pub enum LoadADirection {
    FromA,
    ToA,
}
#[derive(Debug, Copy, Clone)]
pub enum LDAAndHLChange {
    Increase,
    Decrease,
}

pub struct InstructionContext {
    instruction: Instruction,
    address: Address,
}
impl InstructionContext {
    pub fn new(instruction: Instruction, address: Address) -> Self {
        return Self {
            instruction,
            address,
        };
    }
}

impl fmt::Display for InstructionContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.instruction {
            Instruction::Nop => write!(f, "nop"),
            Instruction::Add => todo!(),
            Instruction::Jump(address) => write!(f, "jp ${:x}", address.val),
            Instruction::LDRegFromImm16(reg16, reg_value16) => {
                write!(f, "ld {}, ${:x}", reg16, reg_value16.val)
            }
            Instruction::LDReg8ToReg8 { source, dest } => write!(f, "ld {}, {}", dest, source),
            Instruction::CondJump (_, _) => todo!(),
            Instruction::LDAAndHL(dir, change) => {
                let sign = match change {
                    LDAAndHLChange::Increase => "+",
                    LDAAndHLChange::Decrease => "-",
                };
                match dir {
                    LoadADirection::FromA => write!(f, "ld [hl{}], a", sign),
                    LoadADirection::ToA => write!(f, "ld a, [hl{}]", sign),
                }
            }
            Instruction::LDHFromAToImmAddress(mem_value8) => todo!(),
            Instruction::LDRegFromImm8(reg8, reg8_value) => {
                write!(f, "ld {}, ${:x}", reg8, reg8_value.val)
            }
            Instruction::LDAIndirect(reg16, load_adirection) => match load_adirection {
                LoadADirection::FromA => write!(f, "ld [{}], a", reg16),
                LoadADirection::ToA => write!(f, "ld a, [{}]", reg16),
            },
            Instruction::IncReg(reg8) => write!(f, "inc {}", reg8),
            Instruction::CondJumpOffset(condition, mem_value8) => write!(f, "jr {}, ${:x}", condition, self.address.val as i32 + 2 + ((mem_value8.val as i8) as i32)),
        }
    }
}
impl fmt::Display for Condition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Condition::NZ => write!(f, "nz"),
        }
    }
}
#[derive(Debug)]
pub enum Instruction {
    Nop,
    Add,
    LDRegFromImm8(Reg8, Reg8Value),
    LDRegFromImm16(Reg16, Reg16Value),
    Jump(Address),
    LDReg8ToReg8 { source: Reg8, dest: Reg8 },
    CondJump (Condition,Address),
    CondJumpOffset (Condition,MemValue8),
    LDAAndHL(LoadADirection, LDAAndHLChange),
    LDHFromAToImmAddress(MemValue8),
    LDAIndirect(Reg16, LoadADirection),
    IncReg(Reg8),
}

#[derive(Debug, Copy, Clone)]
pub enum Condition {
    NZ,
}

impl Instruction {
    pub fn get_length(&self) -> anyhow::Result<usize> {
        match self {
            Instruction::Nop => Ok(1),
            Instruction::Jump(_) => Ok(0),
            Instruction::LDRegFromImm16(_, _) => Ok(3),
            Instruction::LDReg8ToReg8 { source: _, dest: _ } => Ok(1),
            Instruction::Add => todo!(),
            Instruction::CondJump ( _,_ ) => todo!(),
            Instruction::LDHFromAToImmAddress(_) => Ok(2),
            Instruction::LDAAndHL(_, _) => Ok(1),
            Instruction::LDRegFromImm8(_, _) => Ok(2),
            Instruction::LDAIndirect(_, _) => Ok(1),
            Instruction::IncReg(_) => Ok(1),
            Instruction::CondJumpOffset(_, _) => Ok(2),
        }
    }
}

#[derive(Copy, Clone)]
pub struct Flags {
    zero: bool,
    sub: bool,
    half_carry: bool,
    carry: bool,
}
pub struct CPU {
    memory: Memory,
    regs: Registers,
}

impl CPU {
    pub fn new(mmap: Mmap) -> Self {
        return Self {
            memory: Memory::new(mmap),
            regs: Registers::new(),
        };
    }
    pub fn step(&mut self) -> anyhow::Result<()> {
        let insContext = decode_instruction(&self.memory, self.regs.get_reg_16(Reg16::PC).into())?;
        println!("{} | {}", self, insContext);
        let ins = insContext.instruction;
        let length = ins.get_length()?;

        self.execute_instruction(ins)?;

        Ok(())
    }

    pub fn execute_instruction(self: &mut Self, instruction: Instruction) -> anyhow::Result<()> {
        let length = instruction.get_length()?;
        self.regs.advance_pc(length as i32);
        match instruction {
            Instruction::Nop => Ok(()),
            Instruction::Add => todo!(),
            Instruction::Jump(address) => {
                self.regs.set_reg_16(Reg16::PC, address.into());
                Ok(())
            }
            Instruction::LDRegFromImm16(reg, val) => {
                self.regs.set_reg_16(reg, val);
                Ok(())
            }
            Instruction::LDReg8ToReg8 { source, dest } => {
                let val = self.regs.get_reg_8(source);
                self.regs.set_reg_8(dest, val);
                Ok(())
            }
            Instruction::CondJump ( _, _ ) => todo!(),
            Instruction::LDHFromAToImmAddress(addr) => {
                self.memory.write_mem_8(
                    Address {
                        val: addr.val as u16 + 0xFF00 ,
                    },
                    self.regs.get_reg_8(Reg8::A).into(),
                );
                return Ok(());
            }
            Instruction::LDAAndHL(dir, change) => {
                let hl_value = self.regs.get_reg_16(Reg16::HL);
                let new_hl_value = match change {
                    LDAAndHLChange::Increase => hl_value + 1,
                    LDAAndHLChange::Decrease => hl_value - 1,
                };

                match dir {
                    LoadADirection::FromA => todo!(),
                    LoadADirection::ToA => {
                        self.regs
                            .set_reg_8(Reg8::A, self.memory.read_mem_8(hl_value.into())?.into());

                        self.regs.set_reg_16(Reg16::HL, new_hl_value);
                        return Ok(());
                    }
                }
            }
            Instruction::LDRegFromImm8(reg, value) => {
                self.regs.set_reg_8(reg, value);
                Ok(())
            }
            Instruction::LDAIndirect(reg16, load_adirection) => match load_adirection {
                LoadADirection::FromA => {
                    self.memory.write_mem_8(
                        self.regs.get_reg_16(reg16).into(),
                        self.regs.get_reg_8(Reg8::A).into(),
                    )?;
                    Ok(())
                }
                LoadADirection::ToA => todo!(),
            },
            Instruction::IncReg(reg8) => {
                let res: Reg8Value = self.regs.get_reg_8(reg8) + 1;
                self.regs.clear_flag(Flag::N);
                match res.val {
                    0 => self.regs.set_flag(Flag::Z),
                    _ => self.regs.clear_flag(Flag::Z)
                    
                }
                self.regs.set_reg_8(reg8, res);
                Ok(())
            }
            Instruction::CondJumpOffset(condition, mem_value8) =>
            {
                if self.cond_is_satisfied(condition) {
                    // this is a bit odd but we have to first cast the value to i8 so that we dont loose negative values
                    self.regs.advance_pc((mem_value8.val as i8).into() );
                }
                Ok(())

                
            },
        }
    }
    fn cond_is_satisfied(&self, cond: Condition) -> bool {
        match cond {
            Condition::NZ => return !self.regs.get_flag(Flag::Z),
        }
        
    }
}

impl fmt::Display for CPU {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "A: {:02X} F: {:02X} B: {:02X} C: {:02X} D: {:02X} E: {:02X} H: {:02X} L: {:02X} SP: {:X} PC: {:04x}",
            self.regs.get_reg_8(Reg8::A).val,
            self.regs.get_reg_8(Reg8::F).val,
            self.regs.get_reg_8(Reg8::B).val,
            self.regs.get_reg_8(Reg8::C).val,
            self.regs.get_reg_8(Reg8::D).val,
            self.regs.get_reg_8(Reg8::E).val,
            self.regs.get_reg_8(Reg8::H).val,
            self.regs.get_reg_8(Reg8::L).val,
            self.regs.get_reg_16(Reg16::SP).val,
            self.regs.get_reg_16(Reg16::PC).val
        );
        Ok(())
    }
}

impl fmt::Debug for MemValue16 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "MemValue16 {{val: {:#x}}}", self.val)
    }
}
