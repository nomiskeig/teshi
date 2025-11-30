use crate::cpu::Instruction;
use crate::memory::{Address, Memory};
use anyhow::anyhow;

pub fn decode_instruction(memory: &Memory, address: Address) -> anyhow::Result<Instruction> {
    let byte = memory.read_mem_8(address);
    println!(
        "Byte is {:b}b, 0x{:x}, 0o{:o}",
        byte.val, byte.val, byte.val
    );
    match byte.val {
        0x0 => return Ok(Instruction::Nop),
        0xC3 => return Ok(Instruction::Jump(memory.read_mem_16(address + 1).into())),

        _ => return Err(anyhow!("could not decode the byte")),
    }
}
