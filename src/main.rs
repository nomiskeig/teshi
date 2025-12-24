use std::fs::File;

use anyhow::Context;
use cpu::{InstructionContext, CPU};
use memmap2::Mmap;
use memory::Memory;

mod cpu;
mod memory;
mod ppu;
fn main() -> anyhow::Result<()> {

    let file =
        File::open("/home/simon/dev/teshi/04-op.gb").context("Could not open file with rom")?;

    let mmap = unsafe { Mmap::map(&file)? };

    let mut memory = Memory::new(mmap);
    let mut cpu = CPU::new();
    loop {
        cpu.step(&mut memory)?;
    }
}


fn execute_instruction(instruction_context: InstructionContext, cpu: &mut CPU, memory: &mut Memory) {
    
}
