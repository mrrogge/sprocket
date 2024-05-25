```ebnf
program = (tag_decl | fn_decl | statement | newline | COMMENT)*;
tag_decl = ["global"], "tag", ID, ":", type, [":=", expr], ";";
type = [AND], ID;
newline = "\n" | "\r\n";

// function declarations
fn_decl = "fn", ID, "(", fn_param_decls, ")", [":", type], "{", (tag_decl | statement | newline | COMMENT)*, "}";
fn_param_decls = [param_decl, (",", param_decl)*];
param_decl = in_param_decl | out_param_decl | inout_param_decl;
in_param_decl = "<", ID, ":", type;
out_param_decl = ">", ID, ":", type;
inout_param_decl = "<>", ID, ":", type;

// statements
statement = table_stmt | assign_stmt | expr_stmt | return_stmt;
table_stmt = (table_row)+, newline;
table_row = ("|", [expr])+, newline;
assign_stmt = [ASTERISK], ID, ":=", expr, ";";
expr_stmt = expr, ";";
return_stmt = "return", [expr], ";";

// expressions
expr = ID | binop_expr | unop_expr | BOOL_LIT | STR_LIT | NUM_LIT | paren_expr;
paren_expr = "(", expr, ")";
binop_expr = expr, BINOP, expr;
unop_expr = UNOP, expr;
```