use std::{collections::HashMap, hash::Hash};

use crate::parse::Constant;

use super::*;

#[test]
fn test_replace_pseudo_with_stack() {

    let mut symbol_table = HashMap::new();
    symbol_table.insert("a".to_string(), AsmSymTabEntry::ObjEntry(AssemblyType::LongWord, false));
    symbol_table.insert("b".to_string(), AsmSymTabEntry::ObjEntry(AssemblyType::LongWord, false));

    let body = vec![
        Instruction::Mov {
            assembly_type: AssemblyType::LongWord,
            src: Operand::Pseudo("a".to_string()),
            dst: Operand::Register(Reg::AX),
        },
        Instruction::Mov {
            assembly_type: AssemblyType::LongWord,
            src: Operand::Pseudo("b".to_string()),
            dst: Operand::Register(Reg::AX),
        },
        Instruction::Mov {
            assembly_type: AssemblyType::LongWord,
            src: Operand::Pseudo("a".to_string()),
            dst: Operand::Register(Reg::AX),
        },
    ];
    let (new_body, stack_size) = rewrite_pseudo_with_stack(body, &symbol_table);
    assert_eq!(
        new_body,
        vec![
            Instruction::Mov {
                assembly_type: AssemblyType::LongWord,
                src: Operand::Stack(-4),
                dst: Operand::Register(Reg::AX)
            },
            Instruction::Mov {
                assembly_type: AssemblyType::LongWord,
                src: Operand::Stack(-8),
                dst: Operand::Register(Reg::AX)
            },
            Instruction::Mov {
                assembly_type: AssemblyType::LongWord,
                src: Operand::Stack(-4),
                dst: Operand::Register(Reg::AX)
            },
        ]
    );
    assert_eq!(stack_size, 2);
}

#[test]
fn fixup_binary_pseudo_with_stack() {

    let mut symbol_table = HashMap::new();
    symbol_table.insert("a".to_string(), AsmSymTabEntry::ObjEntry(AssemblyType::LongWord, false));
    symbol_table.insert("b".to_string(), AsmSymTabEntry::ObjEntry(AssemblyType::LongWord, false));

    let body = vec![Instruction::Binary {
        op: BinaryOp::Add,
        assembly_type: AssemblyType::LongWord,
        src2: Operand::Pseudo("a".to_string()),
        dst: Operand::Pseudo("b".to_string()),
    }];
    let (new_body, _) = rewrite_pseudo_with_stack(body, &symbol_table);
    assert_eq!(
        new_body,
        vec![Instruction::Binary {
            op: BinaryOp::Add,
            assembly_type: AssemblyType::LongWord,
            src2: Operand::Stack(-4),
            dst: Operand::Stack(-8)
        }]
    );
}

#[test]
fn fixup_idiv_stack() {
    let body = vec![Instruction::Idiv {
        assembly_type: AssemblyType::LongWord,
        src: Operand::Immediate {
            imm: Constant::Int(3),
        },
    }];
    let new_body = fixup_stack_operations(body);
    assert_eq!(
        new_body,
        vec![
            Instruction::Mov {
                assembly_type: AssemblyType::LongWord,
                src: Operand::Immediate {
                    imm: Constant::Int(3)
                },
                dst: Operand::Register(Reg::R10)
            },
            Instruction::Idiv {
                assembly_type: AssemblyType::LongWord,
                src: Operand::Register(Reg::R10)
            }
        ]
    );
}

#[test]
fn fixup_mul_stack() {
    let body = vec![Instruction::Binary {
        op: BinaryOp::Mult,
        assembly_type: AssemblyType::LongWord,
        src2: Operand::Immediate {
            imm: Constant::Int(3),
        },
        dst: Operand::Stack(4),
    }];
    let new_body = fixup_stack_operations(body);
    assert_eq!(
        new_body,
        vec![
            Instruction::Mov {
                assembly_type: AssemblyType::LongWord,
                src: Operand::Stack(4),
                dst: Operand::Register(Reg::R11)
            },
            Instruction::Binary {
                op: BinaryOp::Mult,
                assembly_type: AssemblyType::LongWord,
                src2: Operand::Immediate {
                    imm: Constant::Int(3)
                },
                dst: Operand::Register(Reg::R11)
            },
            Instruction::Mov {
                assembly_type: AssemblyType::LongWord,
                src: Operand::Register(Reg::R11),
                dst: Operand::Stack(4)
            }
        ]
    );
}

#[test]
fn fixup_add_stack() {
    let body = vec![Instruction::Binary {
        op: BinaryOp::Add,
        assembly_type: AssemblyType::LongWord,
        src2: Operand::Stack(4),
        dst: Operand::Stack(8),
    }];
    let new_body = fixup_stack_operations(body);
    assert_eq!(
        new_body,
        vec![
            Instruction::Mov {
                assembly_type: AssemblyType::LongWord,
                src: Operand::Stack(4),
                dst: Operand::Register(Reg::R10)
            },
            Instruction::Binary {
                op: BinaryOp::Add,
                assembly_type: AssemblyType::LongWord,
                src2: Operand::Register(Reg::R10),
                dst: Operand::Stack(8)
            },
        ]
    );
}

#[test]
fn fixup_sub_stack() {
    let body = vec![Instruction::Binary {
        op: BinaryOp::Sub,
        assembly_type: AssemblyType::LongWord,
        src2: Operand::Stack(4),
        dst: Operand::Stack(8),
    }];
    let new_body = fixup_stack_operations(body);
    assert_eq!(
        new_body,
        vec![
            Instruction::Mov {
                assembly_type: AssemblyType::LongWord,
                src: Operand::Stack(4),
                dst: Operand::Register(Reg::R10)
            },
            Instruction::Binary {
                op: BinaryOp::Sub,
                assembly_type: AssemblyType::LongWord,
                src2: Operand::Register(Reg::R10),
                dst: Operand::Stack(8)
            },
        ]
    );
}

#[test]
fn test_fixup_invalid_instructions() {
    let body = vec![Instruction::Mov {
        assembly_type: AssemblyType::LongWord,
        src: Operand::Stack(0),
        dst: Operand::Stack(1),
    }];
    let new_body = fixup_stack_operations(body);
    assert_eq!(
        new_body,
        vec![
            Instruction::Mov {
                assembly_type: AssemblyType::LongWord,
                src: Operand::Stack(0),
                dst: Operand::Register(Reg::R10)
            },
            Instruction::Mov {
                assembly_type: AssemblyType::LongWord,
                src: Operand::Register(Reg::R10),
                dst: Operand::Stack(1)
            },
        ]
    );
}
