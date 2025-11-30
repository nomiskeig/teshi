use std::fs::File;

use anyhow::Context;
use cpu::CPU;
use memmap2::Mmap;

mod cpu;
mod memory;


fn main() -> anyhow::Result<()> {
    println!("Hello, world, test!");

    let file =
        File::open("/home/simon/dev/teshi/04-op.gb").context("Could not open file with rom")?;

    let mmap = unsafe { Mmap::map(&file)? };

    let mut cpu = CPU::new(mmap);
    loop {
        cpu.step()?;
    }
}

