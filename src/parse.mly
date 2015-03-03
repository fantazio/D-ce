(* tokens to add *)
%token <(string, string, string)> VAR (* (type, name, val) *)
%token AND (* && *)
%token ARRAY
%token ASSIGN (* = *)
%token BIAND (* & *)
%token BIOR (* | *)
%token BREAK
%token STRUCT
%token COLON (* : *)
%token COMMA (* , *)
%token DEQ  (* -= *)
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
%token LPAREN (* ( *)
%token LT (* < *)
%token MEQ (* -= *)
%token MINUS (* - *)
%token NE (* != *)
%token NULL
%token OF
%token OR (* || *)
%token PEQ (* += *)
%token PLUS (* + *)
%token RBRACE (* } *)
%token RBRACK (* ] *)
%token RPAREN (* ) *)
%token SEMI (* ; *)
%token TEQ (* *= *)
%token THEN
%token TIMES (* * *)
%token TO
%token WHILE
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

%start (* program *)

%%
