use std::borrow::Cow;

pub trait Encode {
    fn encode(&self, sink: &mut Vec<u8>);
}

impl<T: Encode + ?Sized> Encode for &T {
    fn encode(&self, sink: &mut Vec<u8>) {
        T::encode(self, sink)
    }
}

impl<T: Encode> Encode for [T] {
    fn encode(&self, sink: &mut Vec<u8>) {
        self.len().encode(sink);
        for item in self {
            item.encode(sink);
        }
    }
}

impl Encode for [u8] {
    fn encode(&self, sink: &mut Vec<u8>) {
        self.len().encode(sink);
        sink.extend(self);
    }
}

impl Encode for str {
    fn encode(&self, sink: &mut Vec<u8>) {
        self.len().encode(sink);
        sink.extend_from_slice(self.as_bytes());
    }
}

impl Encode for usize {
    fn encode(&self, sink: &mut Vec<u8>) {
        (*self as u32).encode(sink);
    }
}

impl Encode for u32 {
    fn encode(&self, sink: &mut Vec<u8>) {
        encode_u32(*self, sink);
    }
}

impl Encode for i32 {
    fn encode(&self, sink: &mut Vec<u8>) {
        encode_i32(*self, sink);
    }
}

impl Encode for u64 {
    fn encode(&self, sink: &mut Vec<u8>) {
        encode_u64(*self, sink);
    }
}

impl Encode for i64 {
    fn encode(&self, sink: &mut Vec<u8>) {
        encode_i64(*self, sink);
    }
}

impl Encode for Option<u32> {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            Some(value) => {
                sink.push(0x01);
                value.encode(sink);
            }
            None => sink.push(0x00),
        }
    }
}

fn encode_u32(mut value: u32, sink: &mut Vec<u8>) {
    loop {
        let byte = (value & 0x7F) as u8;
        value >>= 7;
        if value == 0 {
            sink.push(byte);
            break;
        } else {
            sink.push(byte | 0x80);
        }
    }
}

fn encode_u64(mut value: u64, sink: &mut Vec<u8>) {
    loop {
        let byte = (value & 0x7F) as u8;
        value >>= 7;
        if value == 0 {
            sink.push(byte);
            break;
        } else {
            sink.push(byte | 0x80);
        }
    }
}

fn encode_i32(value: i32, sink: &mut Vec<u8>) {
    let mut value = value as i64;
    loop {
        let byte = (value & 0x7F) as u8;
        value >>= 7;
        let sign_bit = (byte & 0x40) != 0;
        let done = (value == 0 && !sign_bit) || (value == -1 && sign_bit);
        if done {
            sink.push(byte);
            break;
        } else {
            sink.push(byte | 0x80);
        }
    }
}

fn encode_i64(value: i64, sink: &mut Vec<u8>) {
    let mut value = value;
    loop {
        let byte = (value & 0x7F) as u8;
        value >>= 7;
        let sign_bit = (byte & 0x40) != 0;
        let done = (value == 0 && !sign_bit) || (value == -1 && sign_bit);
        if done {
            sink.push(byte);
            break;
        } else {
            sink.push(byte | 0x80);
        }
    }
}

fn encode_section(sink: &mut Vec<u8>, count: u32, bytes: &[u8]) {
    let mut section = Vec::new();
    count.encode(&mut section);
    section.extend_from_slice(bytes);
    (section.len() as u32).encode(sink);
    sink.extend(section);
}

pub trait Section: Encode {
    fn id(&self) -> u8;
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(u8)]
pub enum SectionId {
    Type = 1,
    Import = 2,
    Function = 3,
    Memory = 5,
    Global = 6,
    Export = 7,
    Code = 10,
    Data = 11,
}

impl From<SectionId> for u8 {
    fn from(id: SectionId) -> u8 {
        id as u8
    }
}

#[derive(Clone, Debug)]
pub struct Module {
    bytes: Vec<u8>,
}

impl Module {
    #[rustfmt::skip]
    pub const HEADER: [u8; 8] = [
        0x00, 0x61, 0x73, 0x6D,
        0x01, 0x00, 0x00, 0x00,
    ];

    pub fn new() -> Self {
        Self {
            bytes: Self::HEADER.to_vec(),
        }
    }

    pub fn section(&mut self, section: &impl Section) -> &mut Self {
        self.bytes.push(section.id());
        section.encode(&mut self.bytes);
        self
    }

    pub fn finish(self) -> Vec<u8> {
        self.bytes
    }
}

impl Default for Module {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Debug)]
pub struct SubType {
    pub is_final: bool,
    pub supertype_idx: Option<u32>,
    pub composite_type: CompositeType,
}

impl Encode for SubType {
    fn encode(&self, sink: &mut Vec<u8>) {
        if self.supertype_idx.is_some() || !self.is_final {
            sink.push(if self.is_final { 0x4F } else { 0x50 });
            self.supertype_idx.encode(sink);
        }
        self.composite_type.encode(sink);
    }
}

#[derive(Clone, Debug)]
pub struct CompositeType {
    pub inner: CompositeInnerType,
    pub shared: bool,
}

impl Encode for CompositeType {
    fn encode(&self, sink: &mut Vec<u8>) {
        if self.shared {
            sink.push(0x65);
        }
        match &self.inner {
            CompositeInnerType::Struct(ty) => {
                TypeSection::encode_struct(sink, ty.fields.iter().cloned())
            }
            CompositeInnerType::Array(ArrayType(field)) => {
                TypeSection::encode_array(sink, &field.element_type, field.mutable)
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum CompositeInnerType {
    Array(ArrayType),
    Struct(StructType),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct ArrayType(pub FieldType);

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct StructType {
    pub fields: Box<[FieldType]>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct FieldType {
    pub element_type: StorageType,
    pub mutable: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum StorageType {
    I8,
    Val(ValType),
}

impl Encode for StorageType {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            StorageType::I8 => sink.push(0x78),
            StorageType::Val(vt) => vt.encode(sink),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum ValType {
    I32,
    Ref(RefType),
}

impl Encode for ValType {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            ValType::I32 => sink.push(0x7F),
            ValType::Ref(rt) => rt.encode(sink),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct RefType {
    pub nullable: bool,
    pub heap_type: HeapType,
}

impl Encode for RefType {
    fn encode(&self, sink: &mut Vec<u8>) {
        if self.nullable {
            sink.push(0x63);
        } else {
            sink.push(0x64);
        }
        self.heap_type.encode(sink);
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum HeapType {
    Concrete(u32),
}

impl Encode for HeapType {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            HeapType::Concrete(index) => i64::from(*index).encode(sink),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct TypeSection {
    bytes: Vec<u8>,
    num_added: u32,
}

impl TypeSection {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn function<P, R>(&mut self, params: P, results: R) -> &mut Self
    where
        P: IntoIterator<Item = ValType>,
        P::IntoIter: ExactSizeIterator,
        R: IntoIterator<Item = ValType>,
        R::IntoIter: ExactSizeIterator,
    {
        Self::encode_function(&mut self.bytes, params, results);
        self.num_added += 1;
        self
    }

    pub fn rec<T>(&mut self, types: T) -> &mut Self
    where
        T: IntoIterator<Item = SubType>,
        T::IntoIter: ExactSizeIterator,
    {
        let types = types.into_iter();
        self.bytes.push(0x4E);
        types.len().encode(&mut self.bytes);
        for ty in types {
            ty.encode(&mut self.bytes);
        }
        self.num_added += 1;
        self
    }

    fn encode_function<P, R>(sink: &mut Vec<u8>, params: P, results: R)
    where
        P: IntoIterator<Item = ValType>,
        P::IntoIter: ExactSizeIterator,
        R: IntoIterator<Item = ValType>,
        R::IntoIter: ExactSizeIterator,
    {
        let params = params.into_iter();
        let results = results.into_iter();
        sink.push(0x60);
        params.len().encode(sink);
        params.for_each(|p| p.encode(sink));
        results.len().encode(sink);
        results.for_each(|r| r.encode(sink));
    }

    fn encode_array(sink: &mut Vec<u8>, ty: &StorageType, mutable: bool) {
        sink.push(0x5E);
        Self::encode_field(sink, ty, mutable);
    }

    fn encode_struct<F>(sink: &mut Vec<u8>, fields: F)
    where
        F: IntoIterator<Item = FieldType>,
        F::IntoIter: ExactSizeIterator,
    {
        let fields = fields.into_iter();
        sink.push(0x5F);
        fields.len().encode(sink);
        for field in fields {
            Self::encode_field(sink, &field.element_type, field.mutable);
        }
    }

    fn encode_field(sink: &mut Vec<u8>, ty: &StorageType, mutable: bool) {
        ty.encode(sink);
        sink.push(mutable as u8);
    }
}

impl Encode for TypeSection {
    fn encode(&self, sink: &mut Vec<u8>) {
        encode_section(sink, self.num_added, &self.bytes);
    }
}

impl Section for TypeSection {
    fn id(&self) -> u8 {
        SectionId::Type.into()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EntityType {
    Function(u32),
    Memory(MemoryType),
    Global(GlobalType),
}

impl Encode for EntityType {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            EntityType::Function(index) => {
                sink.push(0x00);
                index.encode(sink);
            }
            EntityType::Memory(ty) => {
                sink.push(0x02);
                ty.encode(sink);
            }
            EntityType::Global(ty) => {
                sink.push(0x03);
                ty.encode(sink);
            }
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct ImportSection {
    bytes: Vec<u8>,
    num_added: u32,
}

impl ImportSection {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn import(&mut self, module: &str, field: &str, ty: EntityType) -> &mut Self {
        module.encode(&mut self.bytes);
        field.encode(&mut self.bytes);
        ty.encode(&mut self.bytes);
        self.num_added += 1;
        self
    }
}

impl Encode for ImportSection {
    fn encode(&self, sink: &mut Vec<u8>) {
        encode_section(sink, self.num_added, &self.bytes);
    }
}

impl Section for ImportSection {
    fn id(&self) -> u8 {
        SectionId::Import.into()
    }
}

#[derive(Clone, Debug, Default)]
pub struct FunctionSection {
    bytes: Vec<u8>,
    num_added: u32,
}

impl FunctionSection {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn function(&mut self, type_index: u32) -> &mut Self {
        type_index.encode(&mut self.bytes);
        self.num_added += 1;
        self
    }
}

impl Encode for FunctionSection {
    fn encode(&self, sink: &mut Vec<u8>) {
        encode_section(sink, self.num_added, &self.bytes);
    }
}

impl Section for FunctionSection {
    fn id(&self) -> u8 {
        SectionId::Function.into()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct MemoryType {
    pub minimum: u64,
    pub maximum: Option<u64>,
    pub memory64: bool,
    pub shared: bool,
    pub page_size_log2: Option<u32>,
}

impl Encode for MemoryType {
    fn encode(&self, sink: &mut Vec<u8>) {
        let mut flags = 0;
        if self.maximum.is_some() {
            flags |= 0b0001;
        }
        if self.shared {
            flags |= 0b0010;
        }
        if self.memory64 {
            flags |= 0b0100;
        }
        if self.page_size_log2.is_some() {
            flags |= 0b1000;
        }
        sink.push(flags);
        self.minimum.encode(sink);
        if let Some(max) = self.maximum {
            max.encode(sink);
        }
        if let Some(page) = self.page_size_log2 {
            page.encode(sink);
        }
    }
}

#[derive(Clone, Default, Debug)]
pub struct MemorySection {
    bytes: Vec<u8>,
    num_added: u32,
}

impl MemorySection {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn memory(&mut self, memory_type: MemoryType) -> &mut Self {
        memory_type.encode(&mut self.bytes);
        self.num_added += 1;
        self
    }
}

impl Encode for MemorySection {
    fn encode(&self, sink: &mut Vec<u8>) {
        encode_section(sink, self.num_added, &self.bytes);
    }
}

impl Section for MemorySection {
    fn id(&self) -> u8 {
        SectionId::Memory.into()
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct GlobalType {
    pub val_type: ValType,
    pub mutable: bool,
    pub shared: bool,
}

impl Encode for GlobalType {
    fn encode(&self, sink: &mut Vec<u8>) {
        self.val_type.encode(sink);
        let mut flags = 0;
        if self.mutable {
            flags |= 0b01;
        }
        if self.shared {
            flags |= 0b10;
        }
        sink.push(flags);
    }
}

#[derive(Clone, Default, Debug)]
pub struct GlobalSection {
    bytes: Vec<u8>,
    num_added: u32,
}

impl GlobalSection {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn global(&mut self, global_type: GlobalType, init_expr: &ConstExpr) -> &mut Self {
        global_type.encode(&mut self.bytes);
        init_expr.encode(&mut self.bytes);
        self.num_added += 1;
        self
    }
}

impl Encode for GlobalSection {
    fn encode(&self, sink: &mut Vec<u8>) {
        encode_section(sink, self.num_added, &self.bytes);
    }
}

impl Section for GlobalSection {
    fn id(&self) -> u8 {
        SectionId::Global.into()
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(u8)]
pub enum ExportKind {
    Func = 0x00,
    Memory = 0x02,
    Global = 0x03,
}

impl Encode for ExportKind {
    fn encode(&self, sink: &mut Vec<u8>) {
        sink.push(*self as u8);
    }
}

#[derive(Clone, Debug, Default)]
pub struct ExportSection {
    bytes: Vec<u8>,
    num_added: u32,
}

impl ExportSection {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn export(&mut self, name: &str, kind: ExportKind, index: u32) -> &mut Self {
        name.encode(&mut self.bytes);
        kind.encode(&mut self.bytes);
        index.encode(&mut self.bytes);
        self.num_added += 1;
        self
    }
}

impl Encode for ExportSection {
    fn encode(&self, sink: &mut Vec<u8>) {
        encode_section(sink, self.num_added, &self.bytes);
    }
}

impl Section for ExportSection {
    fn id(&self) -> u8 {
        SectionId::Export.into()
    }
}

#[derive(Clone, Default, Debug)]
pub struct CodeSection {
    bytes: Vec<u8>,
    num_added: u32,
}

impl CodeSection {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn function(&mut self, func: &Function) -> &mut Self {
        func.encode(&mut self.bytes);
        self.num_added += 1;
        self
    }
}

impl Encode for CodeSection {
    fn encode(&self, sink: &mut Vec<u8>) {
        encode_section(sink, self.num_added, &self.bytes);
    }
}

impl Section for CodeSection {
    fn id(&self) -> u8 {
        SectionId::Code.into()
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Function {
    bytes: Vec<u8>,
}

impl Function {
    pub fn new<L>(locals: L) -> Self
    where
        L: IntoIterator<Item = (u32, ValType)>,
        L::IntoIter: ExactSizeIterator,
    {
        let locals = locals.into_iter();
        let mut bytes = Vec::new();
        locals.len().encode(&mut bytes);
        for (count, ty) in locals {
            count.encode(&mut bytes);
            ty.encode(&mut bytes);
        }
        Self { bytes }
    }

    pub fn instruction(&mut self, instruction: &Instruction) -> &mut Self {
        instruction.encode(&mut self.bytes);
        self
    }
}

impl Encode for Function {
    fn encode(&self, sink: &mut Vec<u8>) {
        self.bytes.encode(sink);
    }
}

#[derive(Clone, Copy, Debug)]
pub struct MemArg {
    pub offset: u64,
    pub align: u32,
    pub memory_index: u32,
}

impl Encode for MemArg {
    fn encode(&self, sink: &mut Vec<u8>) {
        if self.memory_index == 0 {
            self.align.encode(sink);
            self.offset.encode(sink);
        } else {
            (self.align | (1 << 6)).encode(sink);
            self.memory_index.encode(sink);
            self.offset.encode(sink);
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum BlockType {
    Empty,
    Result(ValType),
}

impl Encode for BlockType {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            BlockType::Empty => sink.push(0x40),
            BlockType::Result(ty) => ty.encode(sink),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Instruction<'a> {
    Unreachable,
    Block(BlockType),
    Loop(BlockType),
    If(BlockType),
    Else,
    End,
    Br(u32),
    Return,
    Call(u32),
    Drop,
    LocalGet(u32),
    LocalSet(u32),
    LocalTee(u32),
    I32Load(MemArg),
    I32Load8U(MemArg),
    I32Store(MemArg),
    I32Store8(MemArg),
    I32Const(i32),
    I32Eqz,
    I32Eq,
    I32Ne,
    I32LtS,
    I32LtU,
    I32GtS,
    I32GtU,
    I32LeS,
    I32LeU,
    I32GeS,
    I32GeU,
    I32Add,
    I32Sub,
    I32Mul,
    I32DivS,
    I32DivU,
    I32And,
    I32Xor,
    StructNew(u32),
    StructGet {
        struct_type_index: u32,
        field_index: u32,
    },
    StructGetU {
        struct_type_index: u32,
        field_index: u32,
    },
    StructSet {
        struct_type_index: u32,
        field_index: u32,
    },
    ArrayNewFixed {
        array_type_index: u32,
        array_size: u32,
    },
    ArrayGet(u32),
    ArrayGetU(u32),
    ArraySet(u32),
    _Marker(Cow<'a, [u8]>),
}

impl Encode for Instruction<'_> {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            Instruction::Unreachable => sink.push(0x00),
            Instruction::Block(ty) => {
                sink.push(0x02);
                ty.encode(sink);
            }
            Instruction::Loop(ty) => {
                sink.push(0x03);
                ty.encode(sink);
            }
            Instruction::If(ty) => {
                sink.push(0x04);
                ty.encode(sink);
            }
            Instruction::Else => sink.push(0x05),
            Instruction::End => sink.push(0x0B),
            Instruction::Br(depth) => {
                sink.push(0x0C);
                depth.encode(sink);
            }
            Instruction::Return => sink.push(0x0F),
            Instruction::Call(index) => {
                sink.push(0x10);
                index.encode(sink);
            }
            Instruction::Drop => sink.push(0x1A),
            Instruction::LocalGet(index) => {
                sink.push(0x20);
                index.encode(sink);
            }
            Instruction::LocalSet(index) => {
                sink.push(0x21);
                index.encode(sink);
            }
            Instruction::LocalTee(index) => {
                sink.push(0x22);
                index.encode(sink);
            }
            Instruction::I32Load(memarg) => {
                sink.push(0x28);
                memarg.encode(sink);
            }
            Instruction::I32Load8U(memarg) => {
                sink.push(0x2D);
                memarg.encode(sink);
            }
            Instruction::I32Store(memarg) => {
                sink.push(0x36);
                memarg.encode(sink);
            }
            Instruction::I32Store8(memarg) => {
                sink.push(0x3A);
                memarg.encode(sink);
            }
            Instruction::I32Const(value) => {
                sink.push(0x41);
                value.encode(sink);
            }
            Instruction::I32Eqz => sink.push(0x45),
            Instruction::I32Eq => sink.push(0x46),
            Instruction::I32Ne => sink.push(0x47),
            Instruction::I32LtS => sink.push(0x48),
            Instruction::I32LtU => sink.push(0x49),
            Instruction::I32GtS => sink.push(0x4A),
            Instruction::I32GtU => sink.push(0x4B),
            Instruction::I32LeS => sink.push(0x4C),
            Instruction::I32LeU => sink.push(0x4D),
            Instruction::I32GeS => sink.push(0x4E),
            Instruction::I32GeU => sink.push(0x4F),
            Instruction::I32Add => sink.push(0x6A),
            Instruction::I32Sub => sink.push(0x6B),
            Instruction::I32Mul => sink.push(0x6C),
            Instruction::I32DivS => sink.push(0x6D),
            Instruction::I32DivU => sink.push(0x6E),
            Instruction::I32And => sink.push(0x71),
            Instruction::I32Xor => sink.push(0x73),
            Instruction::StructNew(index) => {
                sink.push(0xFB);
                sink.push(0x00);
                index.encode(sink);
            }
            Instruction::StructGet {
                struct_type_index,
                field_index,
            } => {
                sink.push(0xFB);
                sink.push(0x02);
                struct_type_index.encode(sink);
                field_index.encode(sink);
            }
            Instruction::StructGetU {
                struct_type_index,
                field_index,
            } => {
                sink.push(0xFB);
                sink.push(0x04);
                struct_type_index.encode(sink);
                field_index.encode(sink);
            }
            Instruction::StructSet {
                struct_type_index,
                field_index,
            } => {
                sink.push(0xFB);
                sink.push(0x05);
                struct_type_index.encode(sink);
                field_index.encode(sink);
            }
            Instruction::ArrayNewFixed {
                array_type_index,
                array_size,
            } => {
                sink.push(0xFB);
                sink.push(0x08);
                array_type_index.encode(sink);
                array_size.encode(sink);
            }
            Instruction::ArrayGet(type_index) => {
                sink.push(0xFB);
                sink.push(0x0B);
                type_index.encode(sink);
            }
            Instruction::ArrayGetU(type_index) => {
                sink.push(0xFB);
                sink.push(0x0D);
                type_index.encode(sink);
            }
            Instruction::ArraySet(type_index) => {
                sink.push(0xFB);
                sink.push(0x0E);
                type_index.encode(sink);
            }
            Instruction::_Marker(_) => {}
        }
    }
}

#[derive(Debug)]
pub struct ConstExpr {
    bytes: Vec<u8>,
}

impl ConstExpr {
    pub fn i32_const(value: i32) -> Self {
        let mut bytes = Vec::new();
        Instruction::I32Const(value).encode(&mut bytes);
        Self { bytes }
    }
}

impl Encode for ConstExpr {
    fn encode(&self, sink: &mut Vec<u8>) {
        sink.extend(&self.bytes);
        Instruction::End.encode(sink);
    }
}

#[derive(Clone, Default, Debug)]
pub struct DataSection {
    bytes: Vec<u8>,
    num_added: u32,
}

impl DataSection {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn active<D>(&mut self, memory_index: u32, offset: &ConstExpr, data: D) -> &mut Self
    where
        D: IntoIterator<Item = u8>,
        D::IntoIter: ExactSizeIterator,
    {
        if memory_index == 0 {
            self.bytes.push(0x00);
            offset.encode(&mut self.bytes);
        } else {
            self.bytes.push(0x02);
            memory_index.encode(&mut self.bytes);
            offset.encode(&mut self.bytes);
        }
        let data = data.into_iter();
        data.len().encode(&mut self.bytes);
        self.bytes.extend(data);
        self.num_added += 1;
        self
    }
}

impl Encode for DataSection {
    fn encode(&self, sink: &mut Vec<u8>) {
        encode_section(sink, self.num_added, &self.bytes);
    }
}

impl Section for DataSection {
    fn id(&self) -> u8 {
        SectionId::Data.into()
    }
}
