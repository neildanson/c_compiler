use std::process::{Command, ExitStatus};

use c_compiler::*;

fn run_gcc(input: &str, output: &str) -> bool {
    let output = Command::new("gcc")
        .args([input, "-o", &output])
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
        .expect(format!("failed to execute process {}", filename).as_str());

    output.status
}

fn run_test(input: &str) {
    let path = "tests";
    let c_file = format!("{}/source/{}.c", path, input);
    let s_file = format!("{}/temp/{}.s", path, input);
    let my_out_file = format!("{}/temp/my_{}.out", path, input);
    let gcc_out_file = format!("{}/temp/gcc_{}.out", path, input);

    let generate_assembly = codegen(&c_file).unwrap();
    write_asm(&s_file, &generate_assembly).unwrap();
    let my_status = run_gcc(&s_file, &my_out_file);
    let gcc_status = run_gcc(&c_file, &gcc_out_file);
    assert_eq!(true, my_status, "GCC Compilation failed (my assembler)");
    assert_eq!(true, gcc_status, "GCC Compilation failed");

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
