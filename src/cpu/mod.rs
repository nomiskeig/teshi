pub mod decode;
pub mod registers;

use std::fmt::{self, format};

use anyhow::anyhow;
use decode::decode_instruction;
use memmap2::Mmap;
use registers::{Reg8, Reg8Value, Reg16, Reg16Value, Registers};

use crate::memory::{Address, HIGH_MEM_START, MemValue8, MemValue16, Memory};

#[derive(Debug)]
pub enum LoadADirection {
    FromA,
    ToA,
}
#[derive(Debug)]
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
        write!(f, "{}", self.instruction)
    }
}
impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::Nop => write!(f, "nop"),
            Instruction::Add => todo!(),
            Instruction::Jump(address) => write!(f, "jp ${:x}", address.val),
            Instruction::LDRegFromImm16(reg16, reg_value16) => {
                write!(f, "ld {}, ${:x}", reg16, reg_value16.val)
            }
            Instruction::LDReg8ToReg8 { source, dest } => write!(f, "ld {}, {}", dest, source),
            Instruction::CondJump { cond, address } => todo!(),
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
    CondJump { cond: Condition, address: Address },
    LDAAndHL(LoadADirection, LDAAndHLChange),
    LDHFromAToImmAddress(MemValue8),
    LDAIndirect(Reg16, LoadADirection),
    IncReg(Reg8),
}

#[derive(Debug)]
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
            Instruction::CondJump { cond, address } => todo!(),
            Instruction::LDHFromAToImmAddress(_) => Ok(2),
            Instruction::LDAAndHL(_, _) => Ok(1),
            Instruction::LDRegFromImm8(_, _) => Ok(2),
            Instruction::LDAIndirect(_, _) => Ok(1),
            Instruction::IncReg(_) => Ok(1),
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
        self.regs.advance_pc(length);

        Ok(())
    }

    pub fn execute_instruction(self: &mut Self, instruction: Instruction) -> anyhow::Result<()> {
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
            Instruction::CondJump { cond, address } => todo!(),
            Instruction::LDHFromAToImmAddress(addr) => {
                self.memory.write_mem_8(
                    Address {
                        val: (addr.val + HIGH_MEM_START as u8) as u16,
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
                    );
                    Ok(())
                }
                LoadADirection::ToA => todo!(),
            },
            Instruction::IncReg(reg8) => {
                self.regs.set_reg_8(reg8, self.regs.get_reg_8(reg8) + 1);
                Ok(())
            }
        }
    }
}

impl fmt::Display for CPU {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "A: {:x} F: {:x} B: {:x} C: {:x} D: {:x} E: {:x} H: {:x} L: {:x} SP: {:x} PC: {:x}",
            self.regs.get_reg_8(Reg8::A),
            self.regs.get_reg_8(Reg8::F),
            self.regs.get_reg_8(Reg8::B),
            self.regs.get_reg_8(Reg8::C),
            self.regs.get_reg_8(Reg8::D),
            self.regs.get_reg_8(Reg8::E),
            self.regs.get_reg_8(Reg8::H),
            self.regs.get_reg_8(Reg8::L),
            self.regs.get_reg_16(Reg16::SP),
            self.regs.get_reg_16(Reg16::PC)
        );
        Ok(())
    }
}

impl fmt::Debug for MemValue16 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "MemValue16 {{val: {:#x}}}", self.val)
    }
}
