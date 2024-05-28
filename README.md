# ⚙ SPROCKET ⚙

`sprocket` is a developer-friendly programming language for industrial controls and automation.

This repository is the official toolkit for the `sprocket` language. It provides a command line application written in [rust](https://www.rust-lang.org/). It includes a REPL for basic exploration of the language, and an interpretter for running `sprocket` source files.

`sprocket` is very early in development; it is by no means a complete language, but it will be expanded over time.

## Quickstart

The easiest way to get started with `sprocket` is using the REPL. First, clone this repo using git:

```bash
git clone https://github.com/mrrogge/sprocket.git
cd sprocket
```

You can install `rust` by following the instructions here: https://www.rust-lang.org/tools/install.

Use `cargo` to install dependencies and run the REPL through the CLI:

```bash
cargo run
```

If all goes well, you should see a `sprocket` shell in your terminal:

```
⚙ >
```

From here, you can freely experiment with the `sprocket` language. For example, use it to evaluate numeric expressions:

```
⚙ > 1 + 1;
2
⚙ > 9 + 64 / 8 - 100;
-83
⚙ > 19 < 20;
true
```

You can also define and work with tags:

```
⚙ > tag foo:i32 := 42;
⚙ > foo;
42
⚙ > tag bar:bool;
⚙ > bar;
false
⚙ > bar := foo > 0;
⚙ > bar;
true
```
