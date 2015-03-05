{
Open Lexing
Open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}


(* Abbreviations  *)
let int = '-'? ['0'-'9']+
let space = [' ' '\t']+
let eol = "\r\n"|'\n'|'\r'
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let digit = ['0'-'9']
let nondigit = ['_' 'a'-'z' 'A'-'Z']
let hexadecimal_digit = [digit 'A'-'F' 'a'-'f']
(* identifiers *)
let  id = id_nondigit [id_nondigit digit]*
and rule id_nondigit = nondigit | universal_character_name
and rule universal_character_name = '\\' 'u' hex_quad
and hex_quad = hexadecimal_digit hexadecimal_digit hexadecimal_digit hexadecimal_digit

(* Rules *)

rule read =
  parse
  | space    { read lexbuf }
  | eol  { next_line lexbuf; read lexbuf }
  | int      { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float    { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | id       { ID (Lexing.lexeme lexbuf) }
  | "true"   { TRUE }
  | "false"  { FALSE }
  | "null"   { NULL }
  | '"'      { read_string (Buffer.create 17) lexbuf }
  | '{'      { LBRACE }
  | '}'      { RBRACE }
  | '['      { LBRACK }
  | ']'      { RBRACK }
  | ':'      { COLON }
  | ','      { COMMA }
  | eof      { EOF }
  |"auto"    { KEYWORD (Lexing.lexeme lexbuf) }
  |"enum"  { KEYWORD (Lexing.lexeme lexbuf) }
  |"restrict"  { KEYWORD (Lexing.lexeme lexbuf) }
  |"break"  { BREAK }
  |"extern"  { KEYWORD (Lexing.lexeme lexbuf) }
  |"return"  { KEYWORD (Lexing.lexeme lexbuf) }
  |"case"  { KEYWORD (Lexing.lexeme lexbuf) }
  |"float"  { TYPE (Lexing.lexeme lexbuf) }
  |"short"  { TYPE (Lexing.lexeme lexbuf) }
  |"char"  { TYPE (Lexing.lexeme lexbuf) }
  |"for"  { FOR }
  |"signed"  { KEYWORD (Lexing.lexeme lexbuf) }
  |"const"  { KEYWORD (Lexing.lexeme lexbuf) }
  |"goto"  { KEYWORD (Lexing.lexeme lexbuf) }
  |"sizeof"  { KEYWORD (Lexing.lexeme lexbuf) }
  |"continue"  { CONTINUE }
  |"if"  { IF }
  |"static"  { KEYWORD (Lexing.lexeme lexbuf) }
  |"default"  { KEYWORD (Lexing.lexeme lexbuf) }
  |"inline"  { KEYWORD (Lexing.lexeme lexbuf) }
  |"struct"  { KEYWORD (Lexing.lexeme lexbuf) }
  |"do"  { DO }
  |"int"  { TYPE (Lexing.lexeme lexbuf) }
  |"switch"  { KEYWORD (Lexing.lexeme lexbuf) }
  |"double"  { TYPE (Lexing.lexeme lexbuf) }
  |"long"  { TYPE (Lexing.lexeme lexbuf) }
  |"typedef"  { KEYWORD (Lexing.lexeme lexbuf) }
  |"else"  { ELSE }
  |"register"  { KEYWORD (Lexing.lexeme lexbuf) }
  |"union"  { KEYWORD (Lexing.lexeme lexbuf) }
  |"unsigned"  { TYPE (Lexing.lexeme lexbuf) }
  |"void"  { TYPE (Lexing.lexeme lexbuf) }
  |"volatile"  { KEYWORD (Lexing.lexeme lexbuf) }
  |"while"  { WHILE }
  |"_Bool"  { KEYWORD (Lexing.lexeme lexbuf) }
  |"_Complex"  { KEYWORD (Lexing.lexeme lexbuf) }
  |"_Imaginary"  { KEYWORD (Lexing.lexeme lexbuf) }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }
