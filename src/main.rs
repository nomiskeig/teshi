use std::fs::File;
use std::io::Read;
use std::ops::Add;
use std::ops::Index;

use anyhow::anyhow;
use anyhow::Context;
use derive_more::Add;
use derive_more::From;
use derive_more::Into;
use memmap2::Mmap;

trait ExtractBits {
    fn extract_bits(self, start: usize, end: usize) -> usize;
}

impl ExtractBits for u8 {
    fn extract_bits(self, start: usize, end: usize) -> usize {
        let mut res = self >> (8 - end);
        res = res & ((1 << end - start) - 1);
        return res.into();
    }
}

struct ROM {
    mmap: Mmap,
}

impl ROM {
    pub fn new(mmap: Mmap) -> Self {
        return Self { mmap: mmap };
    }

    pub fn get_instruction(&mut self, address: Address) -> anyhow::Result<Instruction> {
        let byte = self.mmap[address.val as usize];
        println!("Byte is {:b}b, 0x{:x}, 0o{:o}", byte, byte, byte);
        match byte.extract_bits(0, 2) {
            0x0 => return Ok(Instruction::Nop),
            0x3 => return self.get_instruction_block_3(byte, address),
            _ => {
                return Err(anyhow!(
                    "Instruction decode of {:b} not implemented, extract_bits is {}",
                    byte,
                    byte.extract_bits(0, 2)
                ))
            }
        }
    }
    fn read_mem_16(&self, address: Address) -> MemValue16 {
        return MemValue16 {
            val: ((self.mmap[address] as u16) << 0x8) | (self.mmap[address + 1] as u16),
        };
    }

    pub fn get_instruction_block_3(
        &self,
        byte: u8,
        address: Address,
    ) -> anyhow::Result<Instruction> {
        let last = byte.extract_bits(5, 8);
        match last {
            0x2 => self.get_cond_jump_or_load_high(byte, address),
            _ => Err(anyhow!("get_instruction_block_3 last not implrmented")),
        };
        Err(anyhow!("get_instruction_block_3 not implemented"))
    }
    pub fn get_cond_jump_or_load_high(
        &self,
        byte: u8,
        address: Address,
    ) -> anyhow::Result<Instruction> {
        match byte.extract_bits(2, 5) {
            0x1 => Ok(Instruction::CondJump {
                cond: Condition::NZ,
                address: Address {
                    val: self.read_mem_16(address + 1).into(),
                },
            }),
	    _ => Err(anyhow!("get_cond_jump_or_load_high not implemented"))
        }
    }
}
enum Reg8 {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
}
enum Reg16 {
    AF,
    BC,
    DE,
    HL,
    SP,
    PC,
}
struct MemValue16 {
    val: u16,
}

#[derive(Copy, Clone)]
struct Reg16Value {
    val: u16,
}
#[derive(Copy, Clone)]
struct Reg8Value {
    val: u8,
}
#[derive(Copy, Clone)]
struct Flags {
    zero: bool,
    sub: bool,
    half_carry: bool,
    carry: bool,
}
#[derive(Copy, Clone)]
struct Registers {
    af: Reg16Value,
    bc: Reg16Value,
    de: Reg16Value,
    hl: Reg16Value,
    sp: Reg16Value,
    pc: Reg16Value,
    flags: Flags,
}
struct CPU {
    rom: ROM,
    regs: Registers,
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
impl CPU {
    pub fn new(rom: ROM) -> Self {
        return Self {
            rom: rom,
            regs: Registers::new(),
        };
    }
    fn step(&mut self) -> anyhow::Result<()> {
        let ins = self
            .rom
            .get_instruction(self.regs.get_reg_16(Reg16::PC).into())?;
        println!("Found instruction {:?}", ins);

        self.regs.advance_pc(ins.get_length()?);
        Ok(())
    }
}

#[derive(Debug, Into, Add, From, Copy, Clone)]
struct Address {
    val: u16,
}
impl Index<Address> for [u8] {
    type Output = u8;

    fn index(&self, index: Address) -> &Self::Output {
        return &self[index.val as usize];
    }
}

impl From<Reg16Value> for Address {
    fn from(value: Reg16Value) -> Self {
        return Address { val: value.val };
    }
}

impl Add<u16> for Address {
    type Output = Self;
    fn add(self, other: u16) -> Self {
        Self {
            val: self.val + other,
        }
    }
}

#[derive(Debug)]
enum Condition {
    NZ,
}

#[derive(Debug)]
enum Instruction {
    Nop,
    Add,
    CondJump { cond: Condition, address: Address },
}

impl Instruction {
    pub fn get_length(&self) -> anyhow::Result<usize> {
        match self {
            Instruction::Nop => Ok(1),
            _ => Err(anyhow!("Could not get length of instruction {:?}", self)),
        }
    }
}

fn main() -> anyhow::Result<()> {
    println!("Hello, world, test!");

    let file =
        File::open("/home/simon/dev/teshi/04-op.gb").context("Could not open file with rom")?;

    let mmap = unsafe { Mmap::map(&file)? };

    let rom = ROM::new(mmap);
    let mut cpu = CPU::new(rom);
    loop {
        cpu.step()?;
    }
}
impl From<MemValue16> for u16 {
    fn from(value: MemValue16) -> Self {
        return value.val;
    }
}
