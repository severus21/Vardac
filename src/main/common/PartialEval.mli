(**  This module partially evalute [IR] into [IR]. It evaluates 
        - all const expr
        - all const component expr

    - transforms
        - 'bridge()' -> bridge literal
    - inline all session types (i.e get ride of session types aliasing + resolve STInline)

    - session type
        - dual elimination
*)

open Core

val peval_program: IR.program -> IR.program
include IRCompilationPass.Pass