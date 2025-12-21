use std::{
    fmt,
    ops::{Add, Index, IndexMut, Sub},
};
pub const HIGH_MEM_START: usize = 0xFF80;
pub const HIGH_MEM_END: usize = 0xFFFE;
use anyhow::anyhow;
use derive_more::{Add, From, Into};
use memmap2::Mmap;

use crate::cpu::registers::{Reg8Value, Reg16Value};

#[derive(Debug)]
pub struct ROM {
    mmap: Mmap,
}

pub struct MemValue16 {
    pub val: u16,
}

#[derive(Debug, Clone, Copy)]
pub struct MemValue8 {
    pub val: u8,
}

#[derive(Debug)]
pub struct HighMem {
    data: Vec<MemValue8>,
}
#[derive(Debug)]
pub struct Memory {
    rom: ROM,
    high_mem: HighMem,
}
enum ValWidth {
    WIDTH8,
    WIDTH16,
}

impl HighMem {
    pub fn new() -> Self {
        return Self {
            data: vec![MemValue8 { val: 0 }; HIGH_MEM_END - HIGH_MEM_START],
        };
    }
    pub fn read_8(&self, address: Address) -> MemValue8 {
        return self.data[address];
    }
    pub fn read_16(&self, address: Address) -> MemValue16 {
        MemValue16 {
            val: ((self.data[address + 1 as i32].val as u16) << 0x8) | (self.data[address].val as u16),
        }
    }
    pub fn write_8(&mut self, address: Address, value: MemValue8) {
        self.data[address] = value;
    }
}
impl IndexMut<Address> for Vec<MemValue8> {
    fn index_mut(&mut self, index: Address) -> &mut Self::Output {
        return &mut self[index.val as usize];
        
    }
}

impl Index<Address> for Vec<MemValue8> {
    type Output = MemValue8;

    fn index(&self, index: Address) -> &Self::Output {
        return &self[index.val as usize];
    }
}
impl Memory {
    pub fn new(mmap: Mmap) -> Self {
        return Self {
            rom: ROM::new(mmap),
            high_mem: HighMem::new(),
        };
    }

    pub fn read_mem_8(&self, address: Address) -> anyhow::Result<MemValue8> {
        match address.val {
            0x00..0x7FFF => Ok(self.rom.read_8(address)),
            0xFF80..0xFFFE => Ok(self.high_mem.read_8(address - 0xFF80)),
            _ => Err(anyhow!("could not read address {:?}", address)),
        }
    }
    pub fn read_mem_16(&self, address: Address) -> anyhow::Result<MemValue16> {
        match address.val {
            0x00..0x7FFF => return Ok(self.rom.read_16(address)),
            0xFF80..0xFFFE => return Ok(self.high_mem.read_16(address - 0xFF80)),
            _ => Err(anyhow!("could not read address {:?}", address)),
        }
    }
    pub fn write_mem_8(&mut self, address: Address, value: MemValue8) -> anyhow::Result<()> {
        match address.val {
            0x00..0xFFF => Err(anyhow!("cannot write to rom")),
            0xFF80..0xFFFE => {
                self.high_mem
                    .write_8(address - HIGH_MEM_START as u16, value);
                return Ok(());
            }
            _ => Err(anyhow!("could not read address {:?}", address)),
        }
    }
}
#[derive(Into, Copy, Clone)]
pub struct Address {
    pub val: u16,
}
impl Into<Reg16Value> for Address {
    fn into(self) -> Reg16Value {
        return Reg16Value { val: self.val };
    }
}

impl fmt::Debug for Address {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Address {{val: {:#x}}}", self.val)
    }
}

impl Address {
    pub fn new(val: u16) -> Self {
        return Address { val: val };
    }
}
impl Index<Address> for [u8] {
    type Output = u8;

    fn index(&self, index: Address) -> &Self::Output {
        return &self[index.val as usize];
    }
}

impl From<Reg16Value> for Address {
    fn from(value: Reg16Value) -> Self {
        return Address::new(value.val);
    }
}

impl From<MemValue16> for Address {
    fn from(value: MemValue16) -> Self {
        return Address::new(value.val);
    }
}
impl From<u16> for Address {
    fn from(value: u16) -> Self {
        return Address::new(value);
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
impl Add<usize> for Address {
    type Output = Self;
    fn add(self, other: usize) -> Self {
        Self {
            val: self.val + other as u16,
        }
    }
}
impl Add<usize> for MemValue8 {
    type Output = Self;
    fn add(self, other: usize) -> Self {
        Self {
            val: self.val + other as u8,
        }
    }
}
impl Add<i32> for Address {
    type Output = Self;
    fn add(self, other: i32) -> Self {
        Self {
            val: self.val + other as u16,
        }
    }
}
impl Sub<u16> for Address {
    type Output = Self;

    fn sub(self, rhs: u16) -> Self::Output {
        Self {
            val: self.val - rhs,
        }
    }
}

impl ROM {
    pub fn new(mmap: Mmap) -> Self {
        return Self { mmap: mmap };
    }
    pub fn read_8(self: &Self, address: Address) -> MemValue8 {
        return MemValue8 {
            val: self.mmap[address],
        };
    }
    pub fn read_16(self: &Self, address: Address) -> MemValue16 {
        return MemValue16 {
            val: ((self.mmap[address + 1 as i32] as u16) << 0x8) | (self.mmap[address] as u16),
        };
    }
}

pub trait ExtractBits {
    fn extract_bits(self, start: usize, end: usize) -> usize;
}

impl ExtractBits for MemValue8 {
    fn extract_bits(self, start: usize, end: usize) -> usize {
        let mut res = self.val >> (8 - end);
        res = res & ((1 << end - start) - 1);
        return res.into();
    }
}

impl From<MemValue16> for u16 {
    fn from(value: MemValue16) -> Self {
        return value.val;
    }
}

impl From<MemValue16> for Reg16Value {
    fn from(value: MemValue16) -> Self {
        return Reg16Value { val: value.val };
    }
}

impl From<MemValue8> for Reg8Value {
    fn from(value: MemValue8) -> Self {
        return Reg8Value { val: value.val };
    }
}

impl From<Reg8Value> for MemValue8 {
    fn from(value: Reg8Value) -> Self {
        return MemValue8 { val: value.val };
    }
}
