(*  This module partially evalute [IR] into [IR]. It evaluates 
        - all const expr
        - all const component expr
    Moreover it binds contract and methods 

    - transforms
      - 'bridge()' -> bridge literal
    - inline all session types (i.e get ride of session types aliasing + resolve STInline)
*)

val peval_program: IR.program -> IR.program