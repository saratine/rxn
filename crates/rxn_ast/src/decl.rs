use crate::module::ModulePath;
use crate::Vis;

pub enum Decl {
    Use(UseDecl),
    Fn(FnDecl),
    Struct(StructDecl),
    Union(UnionDecl),
    Enum(EnumDecl),
}

pub struct UseDecl {
    pub vis: Vis,
    pub module_path: ModulePath,
    pub decl_names: Vec<String>,
}

pub struct FnDecl {
    pub vis: Vis,
    pub name: String,
    pub params: FnParams,
    pub return_type: DataType,
}

pub struct FnParams {
    pub vec: FnParam,
}

pub struct FnParam {
    pub name: String,
    pub is_mutable: bool,
    pub data_type: DataType,
}

#[derive(Debug, Clone)]
pub enum DataType {
    Unit,
    U8,
    U16,
    U32,
    U64,
    U128,
    Uint,
    I8,
    I16,
    I32,
    I64,
    I128,
    Int,
    F32,
    F64,
    Bool,
    Char,
    Str,
    Map(Box<MapType>),
    Ref(String),
}

#[derive(Debug, Clone)]
pub struct MapType {
    pub key: DataType,
    pub value: DataType,
}

pub struct StructDecl {
    pub vis: Vis,
    pub name: String,
    pub fields: Vec<StructField>,
}

pub struct StructField {
    pub vis: Vis,
    pub name: String,
    pub data_type: DataType,
}

pub struct UnionDecl {
    pub vis: Vis,
    pub name: String,
    pub variants: UnionVariant,
}

pub struct UnionVariant {
    pub name: String,
    pub fields: Vec<StructField>,
}

pub struct EnumDecl {
    pub vis: Vis,
    pub name: String,
    pub variants: Vec<String>,
}
