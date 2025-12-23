use std::{
    fmt,
    ops::{Add, Index, IndexMut, Sub},
};
pub const HIGH_MEM_START: u16 = 0xFF00;
pub const HIGH_RAM_START: u16 = 0xFF80;
pub const HIGH_RAM_END: u16 = 0xFFFE;
pub const WORKING_RAM_START: u16 = 0xC000;
pub const WORKING_RAM_END: u16 = 0xCFFF;
pub const SWITCHABLE_WORKING_RAM_START: u16 = 0xD000;
pub const SWITCHABLE_WORKING_RAM_END: u16 = 0xDFFF;
pub const IO_START: u16 = 0xFF00;
pub const IO_END: u16 = 0xFF7F;

use anyhow::anyhow;
use derive_more::{Add, From, Into};
use memmap2::Mmap;

use crate::cpu::registers::{Reg8Value, Reg16Value};

#[derive(Debug)]
pub struct RAM {
    data: Vec<MemValue8>,
}

impl RAM {
    pub fn new(size: usize) -> Self {
        return Self {
            data: vec![MemValue8 { val: 0 }; size],
        };
    }
}
#[derive(Debug)]
pub struct TimerIO {
    tac: MemValue8,
}

impl TimerIO {
    pub fn new() -> Self {
        return Self {
            tac: MemValue8 { val: 0 },
        };
    }
}
#[derive(Debug)]
pub struct InterruptIO {
    i_f: MemValue8,
    ie: MemValue8,
}
impl InterruptIO {
    pub fn new() -> Self {
        return Self {
            i_f: MemValue8 { val: 0 },
            ie: MemValue8 { val: 0 },
        };
    }
}

#[derive(Debug)]
pub struct AudioIO {
    nr_52: MemValue8,
    nr_51: MemValue8,
    nr_50: MemValue8,
}
impl AudioIO {
    pub fn new() -> Self {
        return Self {
            nr_52: MemValue8 { val: 0 },
            nr_51: MemValue8 { val: 0 },
            nr_50: MemValue8 { val: 0 },
        };
    }
}
#[derive(Debug)]
pub struct GraphicIO {
    ly: MemValue8,
}
impl GraphicIO {
    pub fn new() -> Self {
        return Self {
            ly: MemValue8 { val: 0 },
        };
    }
}

#[derive(Debug)]
pub struct IO {
    timer_io: TimerIO,
    interrupt_io: InterruptIO,
    audio_io: AudioIO,
    graphic_io: GraphicIO,
}

impl IO {
    pub fn new() -> Self {
        return Self {
            timer_io: TimerIO::new(),
            interrupt_io: InterruptIO::new(),
            audio_io: AudioIO::new(),
            graphic_io: GraphicIO::new(),
        };
    }
    pub fn read_8(&self, address: Address) -> anyhow::Result<MemValue8> {
        match address.val {
            0xFF07 => return Ok(self.timer_io.tac),
            0xFF0F => return Ok(self.interrupt_io.i_f),
            0xFF44 => return Ok(self.graphic_io.ly),
            _ => Err(anyhow!("could not read address 8 in IO")),
        }
    }
    pub fn read_16(&self, address: Address) -> anyhow::Result<MemValue16> {
        Err(anyhow!("could not read address 16 in IO"))
    }
    pub fn write_8(&mut self, address: Address, value: MemValue8) -> anyhow::Result<()> {
        match address.val {
            0xFF07 => self.timer_io.tac = value,
            0xFF0F => self.interrupt_io.i_f = value,
            0xFF24 => self.audio_io.nr_50 = value,
            0xFF25 => self.audio_io.nr_51 = value,
            0xFF26 => self.audio_io.nr_52 = value,
            0xFFFF => self.interrupt_io.ie = value,
            _ => {
                return Err(anyhow!(
                    "could not write address 8 in IO at address {:x}",
                    address.val
                ));
            }
        }
        return Ok(());
    }
}

#[derive(Debug)]
pub struct ROM {
    mmap: Mmap,
}

#[derive(Clone, Copy)]
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
    high_mem: RAM,
    working_ram: RAM,
    switchable_working_ram: RAM,
    io: IO,
}
enum ValWidth {
    WIDTH8,
    WIDTH16,
}

impl RAM {
    pub fn read_8(&self, address: Address) -> MemValue8 {
        return self.data[address];
    }
    pub fn read_16(&self, address: Address) -> MemValue16 {
        MemValue16 {
            val: ((self.data[address + 1 as i32].val as u16) << 0x8)
                | (self.data[address].val as u16),
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
            high_mem: RAM::new((HIGH_RAM_END - HIGH_RAM_START + 1) as usize),
            working_ram: RAM::new((WORKING_RAM_END - WORKING_RAM_START + 1) as usize),
            switchable_working_ram: RAM::new(
                (SWITCHABLE_WORKING_RAM_END - SWITCHABLE_WORKING_RAM_START + 1) as usize,
            ),
            io: IO::new(),
        };
    }

    pub fn read_mem_8(&self, address: Address) -> anyhow::Result<MemValue8> {
        match address.val {
            0x00..0x7FFF => Ok(self.rom.read_8(address)),
            WORKING_RAM_START..=WORKING_RAM_END => {
                Ok(self.working_ram.read_8(address - WORKING_RAM_START))
            }
            HIGH_RAM_START..=HIGH_RAM_END => Ok(self.high_mem.read_8(address - HIGH_RAM_START)),
            IO_START..=IO_END => return self.io.read_8(address),
            SWITCHABLE_WORKING_RAM_START..=SWITCHABLE_WORKING_RAM_END => {
                return Ok(self
                    .switchable_working_ram
                    .read_8(address - SWITCHABLE_WORKING_RAM_START));
            }
            _ => Err(anyhow!("could not read address {:?}", address)),
        }
    }
    pub fn read_mem_16(&self, address: Address) -> anyhow::Result<MemValue16> {
        match address.val {
            0x00..0x7FFF => return Ok(self.rom.read_16(address)),
            HIGH_RAM_START..=HIGH_RAM_END => {
                return Ok(self.high_mem.read_16(address - HIGH_RAM_START));
            }
            WORKING_RAM_START..=WORKING_RAM_END => {
                return Ok(self.working_ram.read_16(address - WORKING_RAM_START));
            }
            SWITCHABLE_WORKING_RAM_START..=SWITCHABLE_WORKING_RAM_END => {
                return Ok(self
                    .switchable_working_ram
                    .read_16(address - SWITCHABLE_WORKING_RAM_START));
            }
            _ => Err(anyhow!("could not read address {:?}", address)),
        }
    }
    pub fn write_mem_8(&mut self, address: Address, value: MemValue8) -> anyhow::Result<()> {
        match address.val {
            0x00..0xFFF => Err(anyhow!("cannot write to rom")),
            HIGH_RAM_START..=HIGH_RAM_END => {
                self.high_mem.write_8(address - HIGH_RAM_START, value);
                return Ok(());
            }
            WORKING_RAM_START..=WORKING_RAM_END => {
                self.working_ram.write_8(address - WORKING_RAM_START, value);
                return Ok(());
            }
            SWITCHABLE_WORKING_RAM_START..=SWITCHABLE_WORKING_RAM_END => {
                self.switchable_working_ram
                    .write_8(address - SWITCHABLE_WORKING_RAM_START, value);
                return Ok(());
            }
            IO_START..=IO_END => return self.io.write_8(address, value),
            0xFFFF => return self.io.write_8(address, value),
            _ => Err(anyhow!("could not write address {:?}", address)),
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
impl Add<i32> for Reg8Value {
    type Output = Reg8Value;

    fn add(self, rhs: i32) -> Self::Output {
        return Reg8Value {
            val: self.val.wrapping_add(rhs as u8),
        };
    }
}
impl Sub<i32> for Reg8Value {
    type Output = Reg8Value;

    fn sub(self, rhs: i32) -> Self::Output {
        return Reg8Value {
            val: self.val.wrapping_sub(rhs as u8),
        };
    }
}

impl From<Reg8Value> for MemValue8 {
    fn from(value: Reg8Value) -> Self {
        return MemValue8 { val: value.val };
    }
}
