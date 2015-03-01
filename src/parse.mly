(* tokens to add *)

%start <Jsone.value option> prog
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
  |[_a-zA-Z]
  ;

digit:
  |[0-9]
  ;

universal-character-name: 
  |\\u hex-quad
  |\\U hex-quad hex-quad
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
  |[1-9]
  ;

octal-digit:
  |[0-7]
  ;

hexadecimal-digit:
  |[0-9a-fA-F]
  ;


