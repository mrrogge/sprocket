# Changelog

## v0.1.1

Released 2024-05-29

This is the first public release of the `sprocket` toolkit! It is an early development version and can't do much at this point, but this seemed like a good point to get a build out.

Here's a summary of what is currently supported:

* The toolkit can parse and run single files containing top-level statements.
* Tag assign statements are supported, e.g. `some_tag := 42;`
* Top-level expressions can be executed as statements, e.g. `2 + 2;` Note the semicolon is required.
* Several boolean and numeric binary operations are supported; see `AstBinop` in `ast.rs`.
* The "not" unary operator is supported in boolean expressions.
* 