%token EMPTYTOKEN
%token<bool> BOOLLITERAL
%token<float> FLOATLITERAL
%token<int> INTLITERAL
%token<string> LID UID STRLITERAL
%token<string list> ATTR

%token<Ast.flat_type> PTYPE
%token<Ast.unop> UNOP
%token<Ast.binop> BINOP
%token LOCAL SHARED 

%token<Core.Label.label> LABEL_LITERAL

%token LANGLEBRACKET RANGLEBRACKET LBRACKET RBRACKET SEMICOLON COMMA LCURLYBRACKET RCURLYBRACKET MID COLON DOUBLE_COLON
%token BANG RECV SELECT BRANCH RECST SESSION TIMEOUT
%token TYPE OF EVENT PROTOCOL VPLACEDEF
%token TIMER
%token IMPLICIT
%token DERIVE
%token EQ IN DOT LPAREN RPAREN 
%token COMPONENT EXPECTING SIGNATURE METHOD OUTPORT INPORT ENSURES INVARIANT RETURNS WITH CONTRACT SIG ON AND METADATA BBRANCH
%token INCLUDE MUTATION
%token SPAWN THIS ONSTARTUP ONDESTROY 
%token AT
%token SIMPLE_QUOTE
%token SIMPLE_RARROW DOUBLE_RARROW
%token BREAK CASE CONTINUE ELSE EXIT FOR IF MATCH RETURN
%token FUNCTION
%token EOF
%token GHOST 
%token USE INLINE DUAL
%token<Ast._comments> COMMENTS 
%token ERR NONE OK SOME

%token IMPL HEADERS TARGET WHERE
%token<string> BLACKBOX_BODY

%%