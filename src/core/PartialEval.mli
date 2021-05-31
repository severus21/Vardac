(*  This module partially evalute [IR] into [IR]. It evaluates 
        - all const expr
        - all const component expr
    Moreover it binds contract and methods 
*)

val peval_program: IR.program -> IR.program