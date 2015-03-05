(* tokens to add *)
%token <string> CONSTANT (* for hardcoded values *)
%token <string> ID (* self explanatory *)
%token <string> KEYWORD (* for not token defined keyword *)
%token <string> STRING (* for hardcoded strings *)
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

identifier:
  |ID
  ;

constant:
  |CONSTANT
  ;

sign:
  |PLUS
  |MINUS
  ;

string_literal:
  | STRING
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
  |LPAREN expression RPAREN
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
