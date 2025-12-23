use crate::cpu::registers::{Reg8, Reg16};
use crate::cpu::{Condition, Instruction, LDAAndHLChange, LoadADirection};
use crate::memory::{Address, MemValue8, Memory};
use anyhow::anyhow;

use super::InstructionContext;

pub fn decode_instruction(memory: &Memory, address: Address) -> anyhow::Result<InstructionContext> {
    let byte = memory.read_mem_8(address)?;

    let instruction: Instruction = match byte.val {
        0x00 => Instruction::Nop,
        0x01 => Instruction::LDRegFromImm16(Reg16::BC, memory.read_mem_16(address + 1)?.into()),
        0x03 => Instruction::IncReg16(Reg16::BC),
        0x0D => Instruction::DecReg(Reg8::C),
        0x0E => Instruction::LDRegFromImm8(Reg8::C, memory.read_mem_8(address + 1)?.into()),
        0x11 => Instruction::LDRegFromImm16(Reg16::DE, memory.read_mem_16(address + 1)?.into()),
        0x12 => Instruction::LDAIndirect(Reg16::DE, LoadADirection::FromA),
        0x14 => Instruction::IncReg8(Reg8::D),
        0x18 => {
            Instruction::CondJumpOffset(Condition::Always, memory.read_mem_8(address + 1)?.into())
        }
        0x1C => Instruction::IncReg8(Reg8::E),
        0x20 => Instruction::CondJumpOffset(Condition::NZ, memory.read_mem_8(address + 1)?),
        0x21 => Instruction::LDRegFromImm16(Reg16::HL, memory.read_mem_16(address + 1)?.into()),
        0x23 => Instruction::IncReg16(Reg16::HL),
        0x28 => Instruction::CondJumpOffset(Condition::Z, memory.read_mem_8(address + 1)?.into()),
        0x2A => Instruction::LDAAndHL(LoadADirection::ToA, LDAAndHLChange::Increase),
        0x31 => Instruction::LDRegFromImm16(Reg16::SP, memory.read_mem_16(address + 1)?.into()),
        0x3E => Instruction::LDRegFromImm8(Reg8::A, memory.read_mem_8(address + 1)?.into()),

        0x47 => Instruction::LDReg8ToReg8 {
            dest: Reg8::B,
            source: Reg8::A,
        },
        0x78 => Instruction::LDReg8ToReg8 {
            dest: Reg8::A,
            source: Reg8::B,
        },
        0x7C => Instruction::LDReg8ToReg8 {
            dest: Reg8::A,
            source: Reg8::H,
        },
        0x7D => Instruction::LDReg8ToReg8 {
            dest: Reg8::A,
            source: Reg8::L,
        },
        0xB1 => Instruction::OrReg(Reg8::C),
        0xC3 => Instruction::Jump(memory.read_mem_16(address + 1)?.into()),
        0xC5 => Instruction::Push(Reg16::BC),
        0xC9 => Instruction::Ret,
        0xCD => Instruction::Call(memory.read_mem_16(address + 1)?.into()),
        0xE0 => Instruction::LDHImmAddress(
            memory.read_mem_8(address + 1)?.into(),
            LoadADirection::FromA,
        ),
        0xE1 => Instruction::Pop(Reg16::HL),
        0xE5 => Instruction::Push(Reg16::HL),
        0xEA => Instruction::LDAFromAddress(
            memory.read_mem_16(address + 1)?.into(),
            LoadADirection::FromA,
        ),
        0xF0 => {
            Instruction::LDHImmAddress(memory.read_mem_8(address + 1)?.into(), LoadADirection::ToA)
        }
        0xF1 => Instruction::Pop(Reg16::AF),
        0xF3 => Instruction::DI,
        0xF5 => Instruction::Push(Reg16::AF),
        0xFE => Instruction::CPImm8(memory.read_mem_8(address + 1)?.into()),

        _ => {
            return Err(anyhow!(
                "could not decode the byte {:b}b, 0x{:x}, 0o{:o}",
                byte.val,
                byte.val,
                byte.val
            ));
        }
    };

    let mut bytes = vec![MemValue8 { val: 0 }; 0];
    for i in 0..instruction.get_length()? {
        bytes.push(memory.read_mem_8(address + i)?);
    }
    return Ok(InstructionContext::new(instruction, address, bytes));
}
