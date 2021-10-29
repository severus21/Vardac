(****************************** Binding strategy ******************************)

(* Ex1: creating a PIPE between two activations
        [A:a] spawn [B:b] and [C:c] with a dedicated pipe in between
        such that they play one round of ping-pong
*)    
type protocol;

(* Exercice 1 - one pipe between two activations *)
component A () {
    result<void, error> toto2 () {
        bridge<B, C, protocol> b0 = bridge();

        spawn B(this.b0)?;
        spawn C(this.b0)?;
    }

    (* B and C such that they do ping-pong together *)
    component B () { } 

    component C () { }
}


(* Exercie 1.2 - one pipe between all the activation created by one activation of A *)
component A () {
    local bridge<B, C, protocol> b0;

    oncreate result<void, error> toto () {
        this.b0 = bridge();

        return ok(());
    }

    result<void, error> toto2 () {
        spawn B(this.b0)?;
        spawn C(this.b0)?;
    }

    (* B and C such that they do ping-pong together *)
    component B () { } 

    component C () { }
}

(*  Exercice 5.3 - pipe between B and C 
    - use 5.2 and guarantee there is exactly one A activation
    - introduce the naming of channel: 
        - bridge() -> create a fresh bridge
        - bridge(gid) -> create a new bridge or get an existing one 
        main limitation it will be a dynamic property, we will not be able to statically distinguish channel identity. Except if we constraint gid to be statically none (partial evaluation should resolve it to a literal).
*)

(*  Example 6 - socket like communication
    receiving:  just a combination of bridge + port
    sending:    a combination of bridge + initiate_session_with 
*)