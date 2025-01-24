#![feature(iterator_try_collect)]

pub mod decl;
mod expr;
pub mod module;
pub mod parse;

/// The visibility declares which modules can access the declaration it is applied to.
pub enum Vis {
    /// The declaration is visible from any module.
    Public,
    /// The declaration is visible from any module within the current compilation unit.
    Local,
    /// The declaration is visible only within the module it is defined. This is the default
    /// visibility.
    Private,
}
