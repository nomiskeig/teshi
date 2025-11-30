use std::{fmt, ops::{Add, Index}};

use derive_more::{Add, From, Into};
use memmap2::Mmap;

use crate::cpu::registers::Reg16Value;

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
pub struct Memory {
    rom: ROM,
}
impl Memory {
    pub fn new(mmap: Mmap) -> Self {
        return Self {
            rom: ROM::new(mmap),
        };
    }

    pub fn read_mem_8(&self, address: Address) -> MemValue8 {
        return MemValue8 {
            val: self.rom.mmap[address],
        };
    }
    pub fn read_mem_16(&self, address: Address) -> MemValue16 {
        return MemValue16 {
            val: ((self.rom.mmap[address] as u16) << 0x8) | (self.rom.mmap[address + 1] as u16),
        };
    }
}
#[derive( Into, Add, Copy, Clone)]
pub struct Address {
    val: u16,
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

impl ROM {
    pub fn new(mmap: Mmap) -> Self {
        return Self { mmap: mmap };
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
