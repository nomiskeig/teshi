pub mod decode;
pub mod execute;
pub mod interrupts;
pub mod registers;

use anyhow::anyhow;
use decode::decode_instruction;
use interrupts::InterruptController;
use memmap2::Mmap;
use registers::{Flag, Reg8, Reg8Value, Reg16, Reg16Value, Registers};
use std::fmt::{self, format};

use crate::memory::{Address, HIGH_MEM_START, HIGH_RAM_START, MemValue8, MemValue16, Memory};

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
    bytes: Vec<MemValue8>,
}
impl InstructionContext {
    pub fn new(instruction: Instruction, address: Address, bytes: Vec<MemValue8>) -> Self {
        return Self {
            instruction,
            address,
            bytes,
        };
    }
}

impl fmt::Display for InstructionContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for i in 0..self.bytes.len() {
            write!(f, "{:02X}", self.bytes.get(i).unwrap().val);
        }
        write!(f, ": ");
        match self.instruction {
            Instruction::Nop => write!(f, "nop"),
            Instruction::Add => todo!(),
            Instruction::Jump(address) => write!(f, "jp ${:X}", address.val),
            Instruction::LDRegFromImm16(reg16, reg_value16) => {
                write!(f, "ld {}, ${:X}", reg16, reg_value16.val)
            }
            Instruction::LDReg8ToReg8 { source, dest } => write!(f, "ld {}, {}", dest, source),
            Instruction::CondJump(_, _) => todo!(),
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
            Instruction::LDHImmAddress(mem_value8, dir) => match dir {
                LoadADirection::FromA => {
                    write!(f, "ld [${:04X}], a", mem_value8.val as u16 + HIGH_MEM_START)
                }
                LoadADirection::ToA => {
                    write!(f, "ld a, [${:04X}]", mem_value8.val as u16 + HIGH_MEM_START)
                }
            },
            Instruction::LDRegFromImm8(reg8, reg8_value) => {
                write!(f, "ld {}, ${:02X}", reg8, reg8_value.val)
            }
            Instruction::LDAIndirect(reg16, load_adirection) => match load_adirection {
                LoadADirection::FromA => write!(f, "ld [{}], a", reg16),
                LoadADirection::ToA => write!(f, "ld a, [{}]", reg16),
            },
            Instruction::IncReg8(reg8) => write!(f, "inc {}", reg8),
            Instruction::CondJumpOffset(condition, mem_value8) => {
                let mut addr = self.address.val as i32 + 2 + ((mem_value8.val as i8) as i32);
                if (mem_value8.val == 0) {
                    addr = 0x0;
                }
                match condition {
                    Condition::Always => match mem_value8.val {
                        0 => write!(f, "jr, ${:02X}", addr),
                        _ => write!(f, "jr ${:02X}", addr),
                    },
                    _ => {
                        write!(f, "jr {}, ${:04X}", condition, addr)
                    }
                }
            }
            Instruction::DecReg(reg8) => write!(f, "dec {}", reg8),
            Instruction::DI => write!(f, "di"),
            Instruction::LDAFromAddress(mem_value16, load_adirection) => match load_adirection {
                LoadADirection::FromA => write!(f, "ld [${:04X}], a", mem_value16.val),
                LoadADirection::ToA => todo!(),
            },
            Instruction::Call(mem_value16) => write!(f, "call ${:04X}", mem_value16.val),
            Instruction::Ret => write!(f, "ret"),
            Instruction::Push(reg16) => write!(f, "push {}", reg16),
            Instruction::Pop(reg16) => write!(f, "pop {}", reg16),
            Instruction::IncReg16(reg16) => write!(f, "inc {}", reg16),
            Instruction::OrReg(reg8) => write!(f, "or {}", reg8),
            Instruction::CPImm8(mem_value8) => write!(f, "cp ${:02X}", mem_value8.val),
        }
    }
}
impl fmt::Display for Condition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Condition::NZ => write!(f, "nz"),
            Condition::Always => write!(f, ""),
            Condition::Z => write!(f, "z"),
        }
    }
}
#[derive(Debug)]
pub enum Instruction {
    Nop,
    Add,
    DI,
    LDRegFromImm8(Reg8, Reg8Value),
    LDRegFromImm16(Reg16, Reg16Value),
    Jump(Address),
    LDReg8ToReg8 { source: Reg8, dest: Reg8 },
    CondJump(Condition, Address),
    CondJumpOffset(Condition, MemValue8),
    LDAAndHL(LoadADirection, LDAAndHLChange),
    LDHImmAddress(MemValue8, LoadADirection),
    LDAIndirect(Reg16, LoadADirection),
    LDAFromAddress(MemValue16, LoadADirection),
    IncReg8(Reg8),
    IncReg16(Reg16),
    DecReg(Reg8),
    Call(MemValue16),
    Ret,
    OrReg(Reg8),
    Push(Reg16),
    Pop(Reg16),
    CPImm8(MemValue8),
}

#[derive(Debug, Copy, Clone)]
pub enum Condition {
    NZ,
    Z,
    Always,
}
#[derive(Debug, Copy, Clone)]
pub struct VariingTInstructionCycles {
    taken: usize,
    notTaken: usize,
}
pub enum TInstructionCycles {
    Default(usize),
    Variing(VariingTInstructionCycles),
}

impl Instruction {
    pub fn get_length(&self) -> anyhow::Result<usize> {
        match self {
            Instruction::Nop => Ok(1),
            Instruction::Jump(_) => Ok(3),
            Instruction::LDRegFromImm16(_, _) => Ok(3),
            Instruction::LDReg8ToReg8 { source: _, dest: _ } => Ok(1),
            Instruction::Add => todo!(),
            Instruction::CondJump(_, _) => todo!(),
            Instruction::LDHImmAddress(_, _) => Ok(2),
            Instruction::LDAAndHL(_, _) => Ok(1),
            Instruction::LDRegFromImm8(_, _) => Ok(2),
            Instruction::LDAIndirect(_, _) => Ok(1),
            Instruction::IncReg8(_) => Ok(1),
            Instruction::CondJumpOffset(_, _) => Ok(2),
            Instruction::DecReg(_) => Ok(1),
            Instruction::DI => Ok(1),
            Instruction::LDAFromAddress(_, _) => Ok(3),
            Instruction::Call(_) => Ok(3),
            Instruction::Ret => Ok(1),
            Instruction::Push(_) => Ok(1),
            Instruction::Pop(_) => Ok(1),
            Instruction::IncReg16(_) => Ok(1),
            Instruction::OrReg(_) => Ok(1),
            Instruction::CPImm8(_) => Ok(2),
        }
    }
    pub fn get_t_cycles(&self) -> TInstructionCycles {
        match self {
            Instruction::Nop => TInstructionCycles::Default(4),
            Instruction::Add => todo!(),
            Instruction::DI => TInstructionCycles::Default(4),
            Instruction::LDRegFromImm8(_, _) => TInstructionCycles::Default(8),
            Instruction::LDRegFromImm16(_, _) => TInstructionCycles::Default(12),
            Instruction::Jump(_) => TInstructionCycles::Default(16),
            Instruction::LDReg8ToReg8 { source: _, dest: _ } => TInstructionCycles::Default(4),
            Instruction::CondJump(_, _) => TInstructionCycles::Variing(VariingTInstructionCycles {
                taken: 16,
                notTaken: 12,
            }),
            Instruction::CondJumpOffset(_, _) => {
                TInstructionCycles::Variing(VariingTInstructionCycles {
                    taken: 12,
                    notTaken: 8,
                })
            }
            Instruction::LDAAndHL(_, _) => TInstructionCycles::Default(8),
            Instruction::LDHImmAddress(_, _) => TInstructionCycles::Default(12),
            Instruction::LDAIndirect(_, _) => TInstructionCycles::Default(8),
            Instruction::LDAFromAddress(_, _) => TInstructionCycles::Default(16),
            Instruction::IncReg8(_) => TInstructionCycles::Default(4),
            Instruction::IncReg16(_) => TInstructionCycles::Default(8),
            Instruction::DecReg(_) => TInstructionCycles::Default(4),
            Instruction::Call(_) => TInstructionCycles::Default(24),

            Instruction::Ret => TInstructionCycles::Default(24),
            Instruction::OrReg(_) => TInstructionCycles::Default(4),
            Instruction::Push(_) => TInstructionCycles::Default(16),
            Instruction::Pop(_) => TInstructionCycles::Default(12),
            Instruction::CPImm8(_) => TInstructionCycles::Default(8),
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
    regs: Registers,
    interrupt_controller: InterruptController,
}

impl CPU {
    pub fn new() -> Self {
        return Self {
            regs: Registers::new(),
            interrupt_controller: InterruptController::new(),
        };
    }
    pub fn step(&mut self, memory: &mut Memory) -> anyhow::Result<usize> {
        let insContext = decode_instruction(memory, self.regs.get_reg_16(Reg16::PC).into())?;
        println!("{} | {}", self, insContext);

        let ins = insContext.instruction;
        let length = ins.get_length()?;
        let m_cycles = self.execute_instruction(ins, memory)?;

        Ok(m_cycles)
    }
    fn push_to_stack(&mut self, value: u16, memory: &mut Memory) -> anyhow::Result<()> {
        self.regs
            .set_reg_16(Reg16::SP, self.regs.get_reg_16(Reg16::SP) - 1);

        memory.write_mem_8(
            self.regs.get_reg_16(Reg16::SP).into(),
            MemValue8 {
                val: ((value & 0xFF00) >> 0x8) as u8,
            },
        )?;
        self.regs
            .set_reg_16(Reg16::SP, self.regs.get_reg_16(Reg16::SP) - 1);

        memory.write_mem_8(
            self.regs.get_reg_16(Reg16::SP).into(),
            MemValue8 {
                val: (value & 0xFF) as u8,
            },
        )?;
        return Ok(());
    }
    fn pop_from_stack(&mut self, memory: &Memory) -> anyhow::Result<u16> {
        let lsb = memory.read_mem_8(self.regs.get_reg_16(Reg16::SP).into())?;
        self.regs
            .set_reg_16(Reg16::SP, self.regs.get_reg_16(Reg16::SP) + 1);
        let msb = memory.read_mem_8(self.regs.get_reg_16(Reg16::SP).into())?;
        self.regs
            .set_reg_16(Reg16::SP, self.regs.get_reg_16(Reg16::SP) + 1);

        return Ok(lsb.val as u16 | (msb.val as u16) << 0x8);
    }

    pub fn execute_instruction(
        self: &mut Self,
        instruction: Instruction,
        memory: &mut Memory,
    ) -> anyhow::Result<usize> {
        let length = instruction.get_length()?;
        self.regs.advance_pc(length as i32);
        let t_cycles = instruction.get_t_cycles();
        let taken = false;
        match instruction {
            Instruction::Nop => {}
            Instruction::Add => todo!(),
            Instruction::Jump(address) => {
                self.regs.set_reg_16(Reg16::PC, address.into());
            }
            Instruction::LDRegFromImm16(reg, val) => {
                self.regs.set_reg_16(reg, val);
            }
            Instruction::LDReg8ToReg8 { source, dest } => {
                let val = self.regs.get_reg_8(source);
                self.regs.set_reg_8(dest, val);
            }
            Instruction::CondJump(_, _) => todo!(),
            Instruction::LDHImmAddress(addr, dir) => match dir {
                LoadADirection::FromA => {
                    memory.write_mem_8(
                        Address {
                            val: addr.val as u16 + HIGH_MEM_START,
                        },
                        self.regs.get_reg_8(Reg8::A).into(),
                    )?;
                }
                LoadADirection::ToA => {
                    self.regs.set_reg_8(
                        Reg8::A,
                        memory
                            .read_mem_8(Address {
                                val: addr.val as u16 + HIGH_MEM_START,
                            })?
                            .into(),
                    );
                }
            },
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
                            .set_reg_8(Reg8::A, memory.read_mem_8(hl_value.into())?.into());

                        self.regs.set_reg_16(Reg16::HL, new_hl_value);
                    }
                }
            }
            Instruction::LDRegFromImm8(reg, value) => {
                self.regs.set_reg_8(reg, value);
            }
            Instruction::LDAIndirect(reg16, load_adirection) => match load_adirection {
                LoadADirection::FromA => {
                    memory.write_mem_8(
                        self.regs.get_reg_16(reg16).into(),
                        self.regs.get_reg_8(Reg8::A).into(),
                    )?;
                }
                LoadADirection::ToA => todo!(),
            },
            Instruction::IncReg8(reg8) => {
                // TODO: this is missing handling of the H flag
                let val = self.regs.get_reg_8(reg8);
                let res: Reg8Value = val + 1;
                self.regs.clear_flag(Flag::N);
                match res.val {
                    0 => self.regs.set_flag(Flag::Z),
                    _ => self.regs.clear_flag(Flag::Z),
                }
                match ((val.val & 0xF) + 1) & 0x10 {
                    0x10 => self.regs.set_flag(Flag::H),
                    _ => self.regs.clear_flag(Flag::H),
                }
                self.regs.set_reg_8(reg8, res);
            }
            Instruction::DecReg(reg8) => {
                // TODO: this is missing handling of the H flag
                let val = self.regs.get_reg_8(reg8);
                let res: Reg8Value = val - 1;
                self.regs.set_flag(Flag::N);
                match res.val {
                    0 => self.regs.set_flag(Flag::Z),
                    _ => self.regs.clear_flag(Flag::Z),
                }
                match (val.val & 0xF).wrapping_sub(1) & 0x10 {
                    0x10 => self.regs.set_flag(Flag::H),
                    _ => self.regs.clear_flag(Flag::H),
                }
                self.regs.set_reg_8(reg8, res);
            }
            Instruction::CondJumpOffset(condition, mem_value8) => {
                if self.cond_is_satisfied(condition) {
                    // this is a bit odd but we have to first cast the value to i8 so that we dont loose negative values
                    self.regs.advance_pc((mem_value8.val as i8).into());
                }
            }
            Instruction::DI => {
                self.interrupt_controller.disable_interrupts();
            }
            Instruction::LDAFromAddress(address, load_adirection) => match load_adirection {
                LoadADirection::FromA => {
                    memory.write_mem_8(address.into(), self.regs.get_reg_8(Reg8::A).into())?
                }
                LoadADirection::ToA => todo!(),
            },
            Instruction::Call(addr) => {
                let pc = self.regs.get_reg_16(Reg16::PC).val;
                self.push_to_stack(pc, memory)?;
                self.regs.set_reg_16(Reg16::PC, addr.into());
            }
            Instruction::Ret => {
                let addr = self.pop_from_stack(memory)?;
                self.regs.set_reg_16(Reg16::PC, Reg16Value { val: addr });
            }
            Instruction::Push(reg16) => {
                let reg_val = self.regs.get_reg_16(reg16).val;
                self.push_to_stack(reg_val, memory)?;
            }
            Instruction::Pop(reg16) => {
                let val = self.pop_from_stack(memory)?;
                self.regs.set_reg_16(reg16, Reg16Value { val: val });
            }
            Instruction::IncReg16(reg16) => {
                let new_val = self.regs.get_reg_16(reg16) + 1;
                self.regs.set_reg_16(reg16, new_val);
            }
            Instruction::OrReg(reg8) => {
                let val_reg = self.regs.get_reg_8(reg8);
                let val_a = self.regs.get_reg_8(Reg8::A);
                let new_val = val_reg.val | val_a.val;
                self.regs.clear_flags();
                if new_val == 0 {
                    self.regs.set_flag(Flag::Z);
                }
                self.regs.set_reg_8(Reg8::A, Reg8Value { val: new_val });
            }
            Instruction::CPImm8(mem_value8) => {
                let val_a = self.regs.get_reg_8(Reg8::A).val;
                let res = val_a.wrapping_sub(mem_value8.val);
                self.regs.clear_flags();
                self.regs.set_flag(Flag::N);
                if res == 0 {
                    self.regs.set_flag(Flag::Z);
                }
                if (res & (0x1 << 0x3)) > 0 {
                    self.regs.set_flag(Flag::H);
                }
                if (res & (0x1 << 0x7)) > 0 {
                    self.regs.set_flag(Flag::C);
                }
            }
        }
        let t_cycles_res = match t_cycles {
            TInstructionCycles::Default(cycles) => cycles,

            TInstructionCycles::Variing(variing_tinstruction_cycles) => {
                match taken {
                    true => variing_tinstruction_cycles.taken,
                    false => variing_tinstruction_cycles.notTaken,   
                }
            }
        };
        return Ok(t_cycles_res);
    }
    fn cond_is_satisfied(&self, cond: Condition) -> bool {
        match cond {
            Condition::NZ => return !self.regs.get_flag(Flag::Z),
            Condition::Always => return true,
            Condition::Z => return self.regs.get_flag(Flag::Z),
        }
    }
}

impl fmt::Display for CPU {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "A: {:02X} F: {:02X} B: {:02X} C: {:02X} D: {:02X} E: {:02X} H: {:02X} L: {:02X} SP: {:X} PC: 00:{:04X}",
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
