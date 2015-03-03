(* tokens to add *)
%token <(string, string, string)> VAR (* (type, name, val) *)
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
%left OR AND
%nonassoc GE GT LE LT EQ NE ASSIGN
%left PLUS MINUS
%left TIMES DIVIDE


(*determine the start point as in the example:
 %start <Jsone.value option> prog*)

%start <None> program

%%

program:
  |EOF { None }
  ;
