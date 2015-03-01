(* tokens to add *)
%token COLON
%token COMMA
%token DOT
%token EOF
%token IF
%token LEFT_BRACE
%token LEFT_BRACK
%token LEFT_PAREN
%token FALSE
%token NULL
%token RIGHT_BRACE
%token RIGHT_BRACK
%token RIGHT_PAREN
%token SEMI_COLON
%token TRUE
(*determine the start point as in the example:
 %start <Jsone.value option> prog*)

%%

token:
  |keyword
  |identifier
  |constant
  |string-literal
  |punctuator
  ;
preprocessing-token:
  |header-name
  |identifier
  |pp-number
  |character-constant
  |string-literal
  |punctuator
  ;

keyword:
  |"auto"
  |"enum"
  |"restrict"
  |"break"
  |"extern"
  |"return"
  |"case"
  |"float"
  |"short"
  |"char"
  |"for"
  |"signed"
  |"const"
  |"goto"
  |"sizeof"
  |"continue"
  |"if"
  |"static"
  |"default"
  |"inline"
  |"struct"
  |"do"
  |"int"
  |"switch"
  |"double"
  |"long"
  |"typedef"
  |"else"
  |"register"
  |"union"
  |"unsigned"
  |"void"
  |"volatile"
  |"while"
  |"_Bool"
  |"_Complex"
  |"_Imaginary"
  ;

identifier:
  |identifier-nondigit
  |identifier identifier-nondigit
  |identifier digit
  ;

identifier-nondigit:
  |nondigit
  |universal-character-name
  ;

nondigit: 
  |['_''a'-'z''A'-'Z']
  ;

digit:
  |['0'-'9']
  ;

universal-character-name: 
  |"\\u" hex-quad
  |"\\U" hex-quad hex-quad
  ;

hex-quad:
  |hexadecimal-digit hexadecimal-digit
  |hexadecimal-digit hexadecimal-digit
  ;

constant:
  |integer-constant
  |floating-constant
  |enumeration-constant
  |character-constant
  ;
integersuffixopt:
|
|integer-suffix
;

integer-constant:
  |decimal-constant integer-suffixopt
  |octal-constant integer-suffixopt
  |hexadecimal-constant integer-suffixopt
  ;

decimal-constant:
  |nonzero-digit
  |decimal-constant digit
  ;

octal-constant:
  |"0"
  |octal-constant octal-digit
  ;

hexadecimal-constant:
  |hexadecimal-prefix hexadecimal-digit
  |hexadecimal-constant hexadecimal-digit
  ;

hexadecimal-prefix:
  |"0x"
  |"0X"
  ;

nonzero-digit:
  |['1'-'9']
  ;

octal-digit:
  |['0'-'7']
  ;

hexadecimal-digit:
  |['0'-'9''a'-'f''A'-'F']
  ;

integer-suffix:
  |unsigned-suffix long-suffixopt
  |unsigned-suffix long-long-suffix
  |long-suffix unsigned-suffixopt
  |long-long-suffix unsigned-suffixopt
  ;

long-suffixopt:
  |
  |long-suffix
  ;

unsigned-suffixopt:
  |
  |unsigned-suffix
  ;

unsigned-suffix:
  |"u"
  |"U"
  ;

long-suffix:
  |"l"
  |"L"
  ;

long-long-suffix:
  |"ll"
  |"LL"
  ;

floating-constant:
  |decimal-floating-constant
  |hexadecimal-floating-constant
  ;

decimal-floating-constant:
  |fractional-constant exponent-partopt floating-suffixopt
  |digit-sequence exponent-part floating-suffixopt
  ;

exponent-partopt:
  |
  |exponent-part
  ;

floating-suffixopt:
  |
  |floating-suffix
  ;

hexadecimal-floating-constant:
  |hexadecimal-prefix hexadecimal-fractional-constant binary-exponent-part floating-suffixopt
  |hexadecimal-prefix hexadecimal-digit-sequence binary-exponent-part floating-suffixopt
  ;

fractional-constant:
  |digit-sequenceopt "." digit-sequence
  |digit-sequence "."
  ;

digit-sequenceopt:
  |
  |digit-sequence
  ;

exponent-part:
  |"e" signopt digit-sequence
  |"E" signopt digit-sequence
  ;

signeopt:
  |
  |sign
  ;

sign:
  |"+"
  |"-"
  ;

digit-sequence:
  |digit
  |digit-sequence digit
  ;

hexadecimal-fractional-constant:
  |hexadecimal-digit-sequenceopt "." hexadecimal-digit-sequence
  |hexadecimal-digit-sequence "."
  ;

binary-exponent-part:
  |"p" signopt digit-sequence
  |"P" signopt digit-sequence
  ;

hexadecimal-digit-sequence:
  |hexadecimal-digit
  |hexadecimal-digit-sequence hexadecimal-digit
  ;

floating-suffix:
  |"f"
  |"l"
  |"F"
  |"L"
  ;

enumeration-constant:
  |identifier
  ;

character-constant:
  |"'" c-char-sequence "'"
  |"L'" c-char-sequence "'"
  ;

c-char-sequence:
  |c-char
  |c-char-sequence c-char
  ;

c-char:
  |[!'\'''\\''\n']{1}
  |escape-sequence
  ;

escape-sequence:
  |simple-escape-sequence
  |octal-escape-sequence
  |hexadecimal-escape-sequence
  |universal-character-name
  ;

simple-escape-sequence:
  |"\\'"
  |"\\\""
  |"\\?"
  |"\\\\"
  |"\\a"
  |"\\b"
  |"\\f"
  |"\\n"
  |"\\r"
  |"\\t"
  |"\\v"
  ;

octal-escape-sequence:
  |"\\" octal-digit
  |"\\" octal-digit octal-digit
  |"\\" octal-digit octal-digit octal-digit
  ;

hexadecimal-escape-sequence:
  |"\\x" hexadecimal-digit
  |hexadecimal-escape-sequence hexadecimal-digit
  ;

string-literal:
  |"\"" s-char-sequenceopt "\""
  |"L\"" s-char-sequenceopt "\""
  ;

s-char-sequenceopt:
  |
  |s-char-sequence
  ;

s-char-sequence:
  |s-char
  |s-char-sequence s-char;

s-char:
 |[!'"''\\''\n']{1}
 |escape-sequence
 ;

punctuator:
  |"["
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
  |"%:%:"
  ;

header-name:
  |"<" h-char-sequence ">"
  |"\"" q-char-sequence "\""
  ;

h-char-sequence:
  |h-char
  |h-char-sequence h-char
  ;

h-char:
  |[!'\n''>']{1}
  ;

q-char-sequence:
  |q-char
  |q-char-sequence q-char
  ;

q-char:
  |[!'\n''"']{1}
  ;
