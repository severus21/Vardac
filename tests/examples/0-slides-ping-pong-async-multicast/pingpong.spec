type recprotocol = µ x. ?pong - x;
type protocol = !ping µ x. ?pong - x;

bridge<A, B | C, protocol> b0 = bridge();

component A () {
    oncreate result<void, error> toto () {
        (* initiate_sessions : void -> Sequence/List lazy<Result<'st>> one per target activation that are registered on b0 *)
        for(res0 in initiate_sessions(b0)?){
            protocol s0 = res0?;
            if(s0.endpoint().gid() != this.gid()) { (* No point to send msg to itself*)
                protocol s0 = initiate_session_with(b0, b);

                recprotocol s1 = fire(s0, ping())?;
                (* wait for answer before contacting the next one *)
                (*tuple<pong, .> res = receive(s1)?;*)
            }
        }

       return ok(());
    }

    (* async version *)
    port truc2 on b0 expecting recprotocol = handle_pong 
    
    result<void, error> handle_pong (recprotocol s0) {
        tuple<pong, recprotocol> res = receive(s0)?;
        pong msg = fst(res);

        print("pong from %s (low level details : %s)" s0.endpoint().gid() s0.endpoint.ip_addr());

        return ok(());
    }
}

(*  Question: which activations of B1/B2 are concerned 
    Solution: register activations on a channel

    N.B:
    - no need to register [this] before doing an [b0.initiate_session]
    - no need to register [b] before doing an [b0.initiate_session_with(b)], it will be done automatically
*)
component B () { (*same for B2*)
    oncreate result<void, error> toto () {
        b0.register(this);

        return ok(());
    }

    (* same code as B *)
}

component C () { (*same for B2*)
    oncreate result<void, error> toto () {
        b0.register(this);

        return ok(());
    }

    (* same code as B *)
}