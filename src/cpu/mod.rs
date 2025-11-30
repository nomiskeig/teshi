pub mod decode;
pub mod registers;

use anyhow::anyhow;
use decode::decode_instruction;
use memmap2::Mmap;
use registers::{Reg16, Registers};

use crate::memory::{Address, Memory, ROM};

#[derive(Debug)]
pub enum Instruction {
    Nop,
    Add,
    Jump(Address),
    CondJump { cond: Condition, address: Address },
}

#[derive(Debug)]
pub enum Condition {
    NZ,
}

impl Instruction {
    pub fn get_length(&self) -> anyhow::Result<usize> {
        match self {
            Instruction::Nop => Ok(1),
	    Instruction::Jump(_) => Ok(3),
            _ => Err(anyhow!("Could not get length of instruction {:?}", self)),
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
        let ins = decode_instruction(&self.memory, self.regs.get_reg_16(Reg16::PC).into())?;
        println!("Found instruction {:?}", ins);
        self.regs.advance_pc(ins.get_length()?);
	self.execute_instruction(ins);



	

        Ok(())
    }
}
