use crate::cpu::registers::{Reg8, Reg16};
use crate::cpu::{Instruction, LDAAndHLChange, LoadADirection, Condition};
use crate::memory::{Address, Memory};
use anyhow::anyhow;

use super::InstructionContext;

pub fn decode_instruction(memory: &Memory, address: Address) -> anyhow::Result<InstructionContext> {
    let byte = memory.read_mem_8(address)?;

    let instruction: Instruction = match byte.val {
        0x00 => Instruction::Nop,
        0x0E => Instruction::LDRegFromImm8(Reg8::C, memory.read_mem_8(address + 1)?.into()),
        0x11 => Instruction::LDRegFromImm16(Reg16::DE, memory.read_mem_16(address + 1)?.into()),
        0x12 => Instruction::LDAIndirect(Reg16::DE, LoadADirection::FromA),
        0x14 => Instruction::IncReg(Reg8::D),
        0x1C => Instruction::IncReg(Reg8::E),
        0x20 => Instruction::CondJumpOffset(Condition::NZ, memory.read_mem_8(address + 1)?),
        0x21 => Instruction::LDRegFromImm16(Reg16::HL, memory.read_mem_16(address + 1)?.into()),
        0x2A => Instruction::LDAAndHL(LoadADirection::ToA, LDAAndHLChange::Increase),
        0x47 => Instruction::LDReg8ToReg8 {
            dest: Reg8::B,
            source: Reg8::A,
        },
        0xC3 => Instruction::Jump(memory.read_mem_16(address + 1)?.into()),

        _ => {
            return Err(anyhow!(
                "could not decode the byte {:b}b, 0x{:x}, 0o{:o}",
                byte.val,
                byte.val,
                byte.val
            ));
        }
    };
    return Ok(InstructionContext::new(instruction, address));
}
