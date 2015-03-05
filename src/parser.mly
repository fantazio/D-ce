(* tokens to add *)
%token <string> ID (* self explanatory *)
%token <string> KEYWORD (* for not token defined keyword *)
%token <string> TYPE (* for basics types (int, float, char, double, ...) *)
%token ADDASSIGN (* += *)
%token ADDR (* & *)
%token AND (* & *)
%token ANDASSIGN (* &= *)
%token ARRAY
%token ASSIGN (* = *)
%token BREAK
%token STRUCT
%token COLON (* : *)
%token COMMA (* , *)
%token CONTINUE
%token DIVASSIGN  (* -= *)
%token DEC (* -- *)
%token DIVIDE (* / *)
%token DO
%token DOT (* . *)
%token ELSE
%token EQ (* == *)
%token FOR
%token GE (* >= *)
%token GT (* > *)
%token IF
%token INC (* ++ *)
%token INCLUDE
%token LBRACE (* { *)
%token LBRACK (* [ *)
%token LE (* <= *)
%token LET
%token LOGAND (* && *)
%token LOGOR (* || *)
%token LOGNOT (* ! *)
%token LPAREN (* ( *)
%token LS (* << *)
%token LSASSIGN (* <<= *)
%token LT (* < *)
%token MINASSIGN (* -= *)
%token MODASSIGN (* -= *)
%token MINUS (* - *)
%token MOD (* % *)
%token MULASSIGN (* *= *)
%token NE (* != *)
%token NOT (* ~ *)
%token NULL
%token OF
%token OR (* | *)
%token ORASSIGN (* |= *)
%token PLUS (* + *)
%token RBRACE (* } *)
%token RBRACK (* ] *)
%token RPAREN (* ) *)
%token RS (* >> *)
%token RSASSIGN (* >>= *)
%token SEMI (* ; *)
%token THEN
%token TIMES (* * *)
%token TO
%token WHILE
%token XOR (* ^ *)
%token XORASSIGN (* ^= *)
%token EOF

(* Priorities/associativities *)

%nonassoc DO THEN OF
%nonassoc ELSE
%left ASSIGN ANDASSIGN ADDASSIGN DIVASSIGN LSASSIGN MINASSIGN MODASSIGN MULASSIGN ORASSIGN RSASSIGN XORASSIGN
%left OR AND
%nonassoc GE GT LE LT EQ NE
%left PLUS MINUS
%left TIMES DIVIDE


(*determine the start point as in the example:
 %start <Json.value option> prog*)

%start <None> program

%%

program:
  |EOF { None }
  ;


token:
  |keyword
  |identifier
  |constant
  |string_literal
  |punctuator
  ;

preprocessing_token:
  |header_name
  |identifier
  |pp_number
  |character_constant
  |string_literal
  |punctuator
  ;

keyword:
  |KEYWORD
  |TYPE
  |IF
  |BREAK
  |FOR
  |CONTINUE
  |IF
  |DO
  |ELSE
  |WHILE
  ;

(* transfered to lexer ..
  identifier:
  |identifier_nondigit
  |identifier identifier_nondigit
  |identifier digit
  ;

identifier_nondigit:
  |nondigit
  |universal_character_name
  ;

nondigit: 
  |(* ['_''a'-'z''A'-'Z'] *)
  ;

digit:
  |(*['0'-'9']*)
  ;

universal_character_name: 
  |(* "\\u" hex_quad
  |"\\U" hex_quad hex_quad *)
  ;

hex_quad:
  |hexadecimal_digit hexadecimal_digit hexadecimal_digit hexadecimal_digit
  ; *)

constant:
  |integer_constant
  |floating_constant
  |enumeration_constant
  |character_constant
  ;
integersuffixopt:
|
|integer_suffix
;

integer_constant:
  |decimal_constant integer_suffixopt
  |octal_constant integer_suffixopt
  |hexadecimal_constant integer_suffixopt
  ;

decimal_constant:
  |nonzero_digit
  |decimal_constant digit
  ;

octal_constant:
  |(* "0" *)
  |octal_constant octal_digit
  ;

hexadecimal_constant:
  |hexadecimal_prefix hexadecimal_digit
  |hexadecimal_constant hexadecimal_digit
  ;

hexadecimal_prefix:
  |(* "0x"
  |"0X" *)
  ;

nonzero_digit:
  |(* ['1'-'9'] *)
  ;

octal_digit:
  |(* ['0'-'7'] *)
  ;

(* Moved to lexer ..
  hexadecimal_digit:
  |(* ['0'-'9''a'-'f''A'-'F'] *)
  ; *)

integer_suffix:
  |unsigned_suffix long_suffixopt
  |unsigned_suffix long_long_suffix
  |long_suffix unsigned_suffixopt
  |long_long_suffix unsigned_suffixopt
  ;

long_suffixopt:
  |
  |long_suffix
  ;

unsigned_suffixopt:
  |
  |unsigned_suffix
  ;

unsigned_suffix:
  |(* "u"
  |"U" *)
  ;

long_suffix:
  |(* "l"
  |"L" *)
  ;

long_long_suffix:
  |(* "ll"
  |"LL" *)
  ;

floating_constant:
  |decimal_floating_constant
  |hexadecimal_floating_constant
  ;

decimal_floating_constant:
  |fractional_constant exponent_partopt floating_suffixopt
  |digit_sequence exponent_part floating_suffixopt
  ;

exponent_partopt:
  |
  |exponent_part
  ;

floating_suffixopt:
  |
  |floating_suffix
  ;

hexadecimal_floating_constant:
  |hexadecimal_prefix hexadecimal_fractional_constant binary_exponent_part floating_suffixopt
  |hexadecimal_prefix hexadecimal_digit_sequence binary_exponent_part floating_suffixopt
  ;

fractional_constant:
  |(* digit_sequenceopt "." digit_sequence
  |digit_sequence "." *)
  ;

digit_sequenceopt:
  |
  |digit_sequence
  ;

exponent_part:
  |(* "e" signopt digit_sequence
  |"E" signopt digit_sequence *)
  ;

signeopt:
  |
  |sign
  ;

sign:
  |(* "+"
  |"-" *)
  ;

digit_sequence:
  |digit
  |digit_sequence digit
  ;

hexadecimal_fractional_constant:
  |(* hexadecimal_digit_sequenceopt "." hexadecimal_digit_sequence
  |hexadecimal_digit_sequence "." *)
  ;

binary_exponent_part:
  |(* "p" signopt digit_sequence
  |"P" signopt digit_sequence *)
  ;

hexadecimal_digit_sequence:
  |hexadecimal_digit
  |hexadecimal_digit_sequence hexadecimal_digit
  ;

floating_suffix:
  |(* "f"
  |"l"
  |"F"
  |"L" *)
  ;

enumeration_constant:
  |identifier
  ;

character_constant:
  |(* "'" c_char_sequence "'"
  |"L'" c_char_sequence "'" *)
  ;

c_char_sequence:
  |c_char
  |c_char_sequence c_char
  ;

c_char:
  |(* [!'\'''\\''\n']{1} *)
  |escape_sequence
  ;

escape_sequence:
  |simple_escape_sequence
  |octal_escape_sequence
  |hexadecimal_escape_sequence
  |universal_character_name
  ;

simple_escape_sequence:
  |(* "\\'"
  |"\\\""
  |"\\?"
  |"\\\\"
  |"\\a"
  |"\\b"
  |"\\f"
  |"\\n"
  |"\\r"
  |"\\t"
  |"\\v" *)
  ;

octal_escape_sequence:
  |(* "\\" *) octal_digit
  |(* "\\" *) octal_digit octal_digit
  |(* "\\" *) octal_digit octal_digit octal_digit
  ;

hexadecimal_escape_sequence:
  |(* "\\x" hexadecimal_digit *)
  |hexadecimal_escape_sequence hexadecimal_digit
  ;

string_literal:
  |(* "\"" s_char_sequenceopt "\""
  |"L\"" s_char_sequenceopt "\"" *)
  ;

s_char_sequenceopt:
  |
  |s_char_sequence
  ;

s_char_sequence:
  |s_char
  |s_char_sequence s_char;

s_char:
 |(* [!'"''\\''\n']{1} *)
 |escape_sequence
 ;

punctuator:
  |(* "["
  |"]"
  |"("
  |")"
  |"{"
  |"}"
  |"."
  |"->"
  |"++"
  |"--"
  |"&"
  |"*"
  |"+"
  |"-"
  |"~"
  |"!"
  |"/"
  |"%"
  |"<<"
  |">>"
  |"<"
  |">"
  |"<="
  |">="
  |"=="
  |"!="
  |"^"
  |"|"
  |"&&"
  |"||"
  |"?"
  |":"
  |";"
  |"..."
  |"="
  |"*="
  |"/="
  |"%="
  |"+="
  |"-="
  |"<<="
  |">>="
  |"&="
  |"^="
  |"|="
  |","
  |"#"
  |"##"
  |"<:"
  |":>"
  |"<%"
  |"%>"
  |"%:"
  |"%:%:" *)
  ;

header_name:
  |(* "<" h_char_sequence ">"
  |"\"" q_char_sequence "\"" *)
  ;

h_char_sequence:
  |h_char
  |h_char_sequence h_char
  ;

h_char:
  |(* [!'\n''>']{1} *)
  ;

q_char_sequence:
  |q_char
  |q_char_sequence q_char
  ;

q_char:
  |(* [!'\n''"']{1} *)
  ;

pp_number:
  |digit
  |(* '.' digit *)
  |pp_number digit
  |pp_number identifier_nondigit
  |(* pp_number 'e' sign
  |pp_number 'E' sign
  |pp_number 'p' sign
  |pp_number 'P' sign
  |pp_number '.' *)
  ;

primary_expression:
  |identifier
  |constant string_literal
  |(* '(' expression ')' *)
  ;

postfix_expression:
  |primary_expression
  |(* postfix_expression '[' expression ']'
  |postfix_expression '(' argument_expression_listopt ')'
  |postfix_expression '.' identifier
  |postfix_expression "->" identifier
  |postfix_expression "++"
  |postfix_expression "--"
  |'(' type_name ')' '{' initializer_list '}'
  |'(' type_name ')' '{' initializer_list ',' '}' *)
  ;

argument_expression_list:
  |assignment_expression
  |(* argument_expression_list ',' assignment_expression *)
  ;

unary_expression:
  |postfix_expression
  |(* "++" unary_expression
  |"--" unary_expression
  |unary-operator cast_expression
  |"sizeof" unary_expression
  |"sizeof" '(' type_name ')' *)
  ;

unary-operator:
  |(* '&'
  |'*'
  |'+'
  |'-'
  |'~'
  |'!' *)
  ;

cast_expression:
  |unary_expression
  |(* '(' type_name ')' cast_expression *)
  ;

multiplicative_expression:
  |cast_expression
  |(* multiplicative_expression '*' cast_expression
  |multiplicative_expression '/' cast_expression
  |multiplicative_expression '%' cast_expression *)
  ;

additive_expression:
  |multiplicative_expression
  |(* additive_expression '+' multiplicative_expression
  |additive_expression '-' multiplicative_expression *)
  ;

shift_expression:
  |additive_expression
  |(* shift_expression "<<" additive_expression
  |shift_expression ">>" additive_expression *)
  ;

relational_expression:
  |shift_expression
  |(* relational_expression '<' shift_expression
  |relational_expression '>' shift_expression
  |relational_expression "<=" shift_expression
  |relational_expression ">=" shift_expression *)
  ;

equality_expression:
  |relational_expression
  |(* equality_expression "==" relational_expression
  |equality_expression "!=" relational_expression *)
  ;

AND_expression:
  |equality_expression
  |(* AND_expression '&' equality_expression *)
  ;

exclusive_OR_expression:
  |equality_expression
  |(* exclusive_OR_expression '^' AND_expression *)
  ;

inclusive_OR_expression:
  |exclusive_OR_expression
  |(* inclusive_OR_expression '|' exclusive_OR_expression *)
  ;

logical_AND_expression:
  |inclusive_OR_expression
  |(* logical_AND_expression "&&" inclusive_OR_expression *)
  ;

logical_OR_expression:
  |logical_AND_expression
  |(* logical_OR_expression "||" logical_AND_expression *)
  ;

conditional_expression:
  |logical_OR_expression
  |(* logical_OR_expression '?' expression ':' conditional_expression *)
  ;

assignment_expression:
  |conditional_expression
  |unary_expression assignment_operator assignment_expression
  ;

assignment_operator:
  |(* '='
  |"*="
  |"/="
  |"%="
  |"+="
  |"-="
  |"<<="
  |">>="
  |"&="
  |"^="
  |"|=" *)
  ;

expression:
  |assignment_expression
  |(* expression ',' assignment_expression *)
  ;

constant_expression:
  |conditional_expression
  ;
