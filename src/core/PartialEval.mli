(*  This module partially evalute [IR] into [IR]. It evaluates 
        - all const expr
        - all const component expr
*)

val peval_program: IR.program -> IR.program