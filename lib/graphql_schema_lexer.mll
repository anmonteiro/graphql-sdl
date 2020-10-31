{
open Lexing
open Graphql_schema_parser
}

let ignored = [' ' '\t' ',']+
let comment = '#' [^ '\n' '\r']*
let newline = '\r' | '\n' | "\r\n"

let digit = ['0'-'9']
let int = '-'? '0' | '-'? ['1'-'9'] digit*
let fract_part = '.' digit+
let exp_part = ['e' 'E'] ['+' '-']? digit+
let float = int fract_part | int exp_part | int fract_part exp_part

let name = ['_' 'A'-'Z' 'a'-'z'] ['_' '0'-'9' 'A'-'Z' 'a'-'z']*

rule token = parse
  | ignored { token lexbuf }
  | comment { token lexbuf }
  | newline { new_line lexbuf; token lexbuf }

  | int   { INT (int_of_string (lexeme lexbuf)) }
  | float { FLOAT (float_of_string (lexeme lexbuf)) }
  | '"' | "\"\"\"" {
    let terminator = Lexing.lexeme lexbuf in
    read_string (Buffer.create 17) terminator lexbuf
  }

  | "false"        { BOOL false }
  | "null"         { NULL }
  | "on"           { ON }
  | "true"         { BOOL true }
  | "type"         { TYPE }
  | "input"        { INPUT_TYPE }
  | "enum"         { ENUM }
  | "union"        { UNION }
  | "interface"    { INTERFACE }
  | "implements"   { IMPLEMENTS }
  | "scalar"       { SCALAR }
  | "schema"       { SCHEMA }
  | "query"        { QUERY }
  | "mutation"     { MUTATION }
  | "subscription" { SUBSCRIPTION }
  | name           { NAME (lexeme lexbuf) }

  | '!'   { BANG }
  | '('   { LPAREN }
  | ')'   { RPAREN }
  | ':'   { COLON }
  | '='   { EQUAL }
  | '@'   { AT }
  | '['   { LBRACK }
  | ']'   { RBRACK }
  | '{'   { LBRACE }
  | '}'   { RBRACE }
  | '|'   { PIPE }
  | eof   { EOF }

and read_string buf terminator = parse
  | "\"\"\"" | '"' {
    let lexeme = Lexing.lexeme lexbuf in

    if lexeme = terminator then
      STRING (Buffer.contents buf)
    else
      let _ = Buffer.add_string buf lexeme in
      read_string buf terminator lexbuf
  }
  (* | '\\' '"'  { Buffer.add_char buf    '"'; read_string buf terminator lexbuf }
  | '\\' '\\' { Buffer.add_char buf   '\\'; read_string buf terminator lexbuf }
  | '\\' '/'  { Buffer.add_char buf    '/'; read_string buf terminator lexbuf }
  | '\\' 'b'  { Buffer.add_char buf   '\b'; read_string buf terminator lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf terminator lexbuf }
  | '\\' 'n'  { Buffer.add_char buf   '\n'; read_string buf terminator lexbuf }
  | '\\' 'r'  { Buffer.add_char buf   '\r'; read_string buf terminator lexbuf }
  | '\\' 't'  { Buffer.add_char buf   '\t'; read_string buf terminator lexbuf } *)
  | [^ '"' '\\' (* '\n' *) '\r']+
    {
      Buffer.add_string buf (lexeme lexbuf);
      read_string buf terminator lexbuf
    }
