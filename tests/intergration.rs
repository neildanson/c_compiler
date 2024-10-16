use std::process::{Command, ExitStatus};

use c_compiler::*;

fn run_gcc(input: &str, output: &str) -> bool {
    let output = Command::new("gcc")
        .args([input, "-o", output])
        .output()
        .expect("failed to execute process");

    println!("status: {}", output.status);
    let err = String::from_utf8_lossy(&output.stderr);
    println!("stderr: {}", err);
    output.status.success()
}

fn run_output(filename: &str) -> ExitStatus {
    let output = Command::new(format!("./{}", filename))
        .output()
        .unwrap_or_else(|_| panic!("failed to execute process {}", filename));

    output.status
}

fn run_test(input: &str) {
    let path = "tests";
    let c_file = format!("{}/source/{}.c", path, input);
    let _dont_care_if_fails = std::fs::create_dir(format!("{}/temp", path));
    let s_file = format!("{}/temp/{}.s", path, input);
    let my_out_file = format!("{}/temp/my_{}.out", path, input);
    let gcc_out_file = format!("{}/temp/gcc_{}.out", path, input);

    let generate_assembly = codegen(&c_file).unwrap();
    write_asm(&s_file, &generate_assembly).unwrap();
    let my_status = run_gcc(&s_file, &my_out_file);
    let gcc_status = run_gcc(&c_file, &gcc_out_file);
    assert!(my_status, "GCC Compilation failed (my assembler)");
    assert!(gcc_status, "GCC Compilation failed");

    let my_result = run_output(&my_out_file);
    let gcc_result = run_output(&gcc_out_file);

    assert_eq!(my_result, gcc_result, "App difference");
}

#[test]
fn add() {
    run_test("add");
}

#[test]
fn subtraction() {
    run_test("subtraction");
}

#[test]
fn for_loops() {
    run_test("for");
}

#[test]
fn pre_increment() {
    run_test("pre_increment");
}

#[test]
fn post_increment() {
    run_test("post_increment");
}

#[test]
fn function_call_6_params() {
    run_test("function_call_6_params");
}

#[test]
fn function_call_7_params() {
    run_test("function_call_7_params");
}

#[test]
fn function_call_8_params() {
    run_test("function_call_8_params");
}
