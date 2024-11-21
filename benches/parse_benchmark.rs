extern crate c_compiler;

use anyhow::Result;
use c_compiler::{
    lex::Tokenizer, parse::parse_program, tacky::Tacky,
    validate::semantic_analysis::SemanticAnalysis,
};
use criterion::{black_box, criterion_group, criterion_main, Criterion};

static SOURCE: &str = "
int fib(int n) {
    if (n == 0 || n == 1) {
        return n;
    } else {
        return fib(n - 1) + fib(n - 2);
    }
}

int main(void) {
    int n = 6;
    return fib(n);
}";

fn parse(tokenizer: &Tokenizer) -> Result<()> {
    let tokens = tokenizer.tokenize(&SOURCE)?;
    let _ast = parse_program(&tokens)?;
    Ok(())
}

fn parse_and_validate(tokenizer: &Tokenizer) -> Result<()> {
    let tokens = tokenizer.tokenize(SOURCE)?;
    let ast = parse_program(&tokens)?;
    let _validated_ast = SemanticAnalysis::semantic_validation(ast)?;
    Ok(())
}

fn parse_and_validate_and_gen_tacky(tokenizer: &Tokenizer) -> Result<()> {
    let tokens = tokenizer.tokenize(SOURCE)?;
    let ast = parse_program(&tokens)?;
    let validated_ast = SemanticAnalysis::semantic_validation(ast)?;
    let mut tacky = Tacky::new(validated_ast);
    let _tacky = tacky.emit_tacky()?;
    Ok(())
}

fn lex_source(c: &mut Criterion) {
    let tokenizer = Tokenizer::new();
    c.bench_function("Lex Fibonacci Success", |b| {
        b.iter(|| {
            for _ in 0..100 {
                let _ = black_box(tokenizer.tokenize(SOURCE));
            }
        })
    });
}

fn parse_source(c: &mut Criterion) {
    let tokenizer = Tokenizer::new();
    c.bench_function("Parse Fibonacci Success", |b| {
        b.iter(|| {
            for _ in 0..100 {
                let _ = black_box(parse(&tokenizer));
            }
        })
    });
}

fn parse_and_validate_source(c: &mut Criterion) {
    let tokenizer = Tokenizer::new();
    c.bench_function("Parse & Validate Fibonacci Success", |b| {
        b.iter(|| {
            for _ in 0..100 {
                let _ = black_box(parse_and_validate(&tokenizer));
            }
        })
    });
}

fn parse_and_validate_and_gen_tacky_source(c: &mut Criterion) {
    let tokenizer = Tokenizer::new();
    c.bench_function("Parse & Validate & Generate Tacky Fibonacci Success", |b| {
        b.iter(|| {
            for _ in 0..100 {
                let _ = black_box(parse_and_validate_and_gen_tacky(&tokenizer));
            }
        })
    });
}

criterion_group!(
    benches,
    lex_source,
    parse_source,
    parse_and_validate_source,
    parse_and_validate_and_gen_tacky_source
);
criterion_main!(benches);
