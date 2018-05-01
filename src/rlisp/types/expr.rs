use std::fmt;

use super::{Expression, Operand};

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub operand: Option<Operand>,
    pub args: Vec<Box<Expression>>,
    pub parent: Option<Box<Expr>>,
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut disp;
        let mut end = ")";
        let mut is_scope = false;
        let mut spacer;

        if let Some(ref op) = self.operand {
            disp = format!("({} ", op);
            if op == &Operand::Scope {
                end = "\n)";
                is_scope = true;
            }
        } else {
            disp = "(".to_string();
        }

        let len = self.args.len();
        for (i, e) in self.args.iter().enumerate() {
            if is_scope {
                spacer = "\n";
            } else {
                spacer = " ";
            }

            if i == len - 1 {
                spacer = "";
            }
            disp.push_str(&format!("{}{}", e, spacer));
        }
        return write!(f, "{}{}", disp, end);
    }
}
