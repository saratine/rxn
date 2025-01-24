use crate::decl::Decl;
use std::path::PathBuf;

/// An abstract syntax-tree for an Rxn source file.
pub struct Module {
    pub descriptor: ModuleDescriptor,
    pub decls: Vec<Decl>,
}

pub struct ModuleDescriptor {
    pub path_buf: PathBuf,
    pub name: String,
    pub parent: ModulePath,
}

impl ModuleDescriptor {
    pub fn standalone(path_buf: PathBuf) -> Option<Self> {
        Some(Self {
            name: path_buf.file_name()?.to_str()?.to_owned(),
            parent: ModulePath(vec![]),
            path_buf,
        })
    }
}

pub struct ModulePath(pub Vec<String>);
