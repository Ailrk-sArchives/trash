use error_chain;
use regex::Regex;
use std::collections::HashSet;
use std::fs::File;
use std::{
    io::Write,
    process::{Command, Stdio},
};

#[derive(PartialEq, Default, Clone, Debug)]
struct Commit {
    hash: String,
    message: String,
}

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn get_git_log() -> Result<()> {
    let output = Command::new("git").arg("log").arg("--oneline").output()?;
    if !output.status.success() {
        error_chain::bail!("Command executed with failing error code");
    }

    let pattern = Regex::new(r"(?x) ([0-9a-fA-F]+) (.*)")?;

    String::from_utf8(output.stdout)?
        .lines()
        .filter_map(|line| pattern.captures(line))
        .map(|cap| Commit {
            hash: cap[1].to_string(),
            message: cap[2].trim().to_string(),
        })
        .take(5)
        .for_each(|x| println!("{:?}", x));

    Ok(())
}

fn run_command_check_error_code() -> Result<()> {
    // pipe all standard fd to rust process.
    let mut child = Command::new("python")
        .stdin(Stdio::piped())
        .stderr(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()?;

    child
        .stdin
        .as_mut()
        .ok_or("Child process stdin has not been captured!")?
        .write_all(b"import this; copyright(); credits(); exit()")?;

    let output = child.wait_with_output()?;

    if output.status.success() {
        let raw_output = String::from_utf8(output.stdout)?;
        let words = raw_output
            .split_whitespace()
            .map(|word| word.to_lowercase())
            .collect::<HashSet<_>>();
        println!("Found {} unique words", words.len());
        println!("{:#?}", words);
        Ok(())
    } else {
        let err = String::from_utf8(output.stderr)?;
        error_chain::bail!("External command error: \n {}", err)
    }
}

fn run_piped_commands() -> Result<()> {
    let directory = std::env::current_dir()?;
    let mut du_output_child = Command::new("du")
        .arg("-ah")
        .arg(&directory)
        .stdout(Stdio::piped())
        .spawn()?;

    if let Some(du_output) = du_output_child.stdout.take() {
        let mut sort_output_child = Command::new("sort")
            .arg("-hr")
            .stdin(du_output)
            .stdout(Stdio::piped())
            .spawn()?;
        du_output_child.wait()?;

        if let Some(sort_output) = sort_output_child.stdout.take() {
            let head_output_child = Command::new("head")
                .args(&["-n", "10"])
                .stdin(sort_output)
                .stdout(Stdio::piped())
                .spawn()?;

            let head_stdout = head_output_child.wait_with_output()?;
            sort_output_child.wait()?;

            println!(
                "Top 10 biggest files and directories in '{}': \n {}",
                directory.display(),
                String::from_utf8(head_stdout.stdout).unwrap()
            );
        }
    }
    Ok(())
}

fn redirect_stdout_stderr_to_samefile() -> std::result::Result<(), std::io::Error> {
    let outputs = File::create("output")?;
    let errors = outputs.try_clone()?;

    // redirect stdout and stderr to the file oops.
    // oops doesn't exists thus will appear in file output.
    Command::new("ls")
        .args(&[".", "oops"])
        .stdout(Stdio::from(outputs))
        .stderr(Stdio::from(errors))
        .spawn()?
        .wait_with_output()?;
    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test1() {
        get_git_log();
    }

    #[test]
    fn test2() {
        run_command_check_error_code();
    }

    #[test]
    fn test3() {
        run_piped_commands();
    }
}
