(* Syntax convention 

    An identifier starting by an uppercase character denotes a component
    Otherwise it starts with a lowercase character
*)
vplacedef vpa of "placeA";
vplacedef vpcloud of "Cloud";
(************************** Point to point communication **************************)
(* Ex0 - fire and forget
    A:a --- ping ----> B:b
*)

(*  Ex1 - ping-pong
    A:a ---- ping ----> B:b
        <--- pong ----- 

*)
type error of ;
event ping of; (* type constructor ping() *)
event pong of; (* type constructor pong() *)
(*protocol p_pingpong = !ping?pong. ;*)
protocol p_pingpong = !ping?pong!ping!ping.;
(*protocol p_pingpong = !ping{timer x|}?pong!ping{|(5<x) && (x<100)}!ping{|x<150}.;*)

(*  a bridge is a dynamic object, it replace the channel concept
    - with a unique ID
    - interconnects a set of endoints
        design choice we constraint the endpoints at the creation of the bridge bridge<A , B1 | B2, !ping?pong>
        only activations of type A can initiate interactions with activations of type B1 or B2.
        we use the notation A == b0 ==> B1 | B2
    - it is directionnal (who can initiate activations)
    
    <!ping?pong> means that 
    - messages that can transfered by the bridge have the type
    [ping, pong]
    - protocol of an interaction between two endpoint (activation) is !ping?point
    - sessions that can be transfered have the following types [!ping?pong, ?ping!pong, ?pong, !ping, .] and an init session has the type !ping?point
*)

(* b0 can de defined globaly or passed as a spawn argument *)
(* TODO fixme do i need inline, seems to be correctly handle by partial eval *)
bridge<A, B, inline p_pingpong> b0 = bridge(p_pingpong);
(*bridge<A, B, !ping?pong, None> b0 = bridge();
bridge<A, B, !ping?pong, TLS> b1 = tlsbridge('xxx.cert');*)

(*  Implementation of a bridge will be provided has an external libraries (according to target definition)

    How to extend the bridge: TODO
    bridge<A,B, !ping?pong> b0 = ...; default
    statically type tlsbridge = bridge<TLS, A, B ...>;
    adding argument bridge('tls', id=10, ...)
*)

(*  Contraints: 
    - a procotol is binded to a bridge, the full protocol will take place on the same bridge. How to circumvent this limitation ?
        two protocols + two bridges + one method that does the linlk between both protocol.

    Advantages:
    - check that types of endpoints matches
    - check that the protocols hosted by the logical bridge also match
*)
component A () {
    string _test = "Implicit is working";
    string _testb = "Implicit is working";
    bridge<A, B, inline p_pingpong> _b;
    (*port truc on b0 expecting ?pong. = this.handle_pong;*)

    onstartup void toto (bridge<A, B, inline p_pingpong> b0, activation_info<B> b) {
        this._b = b0;
        print("> Starting A");
        print(string_of_bridge(b0));
        session<p_pingpong> s0 = initiate_session_with(this._b, b); (* initiate_session_with : bridge<_,'A, 'st> -> ActivationInfo<'A> -> 'st *)
        ?pong. s1 = fire(s0, ping()); (* fire : !'a 'st -> 'a -> Result<'st, error> *)
        int i = 1;
        print("> Ping fired");
        tuple<pong, !ping!ping.> res = receive(s1, this._b);  (*Tuple2<e, s>*)
        print("pong_or_timeout");
        int j = i+1;
        !ping. s2 = fire(second(res), ping());
        print("st_ping_fired");
        . s3 = fire(s2, ping());
        print("th_ping_fire");

        print("Spawn_C");
        (*activation_info<C> c = spawn C() ;*)  
        activation_info<C> c = spawn C() @ current_place(); 
        print("Spawned_C");
        place p = placeof(c);
        
    }

    (*result<void, error> handle_pong (pong msg, . s1) {
        print("pong");
        //fire(s1, pong()); 

        return Ok(());
    }*)
    component C () {
        onstartup void toto () {
            (* Adding an implicit in C - comming from A *)            
            print(implicit::_test);
            print(implicit::_testb);


            list<place> ps1 = places();
            print("psone_done");
            print(place_to_string(listget(ps1, 0)));
            place currenta = current_place();
            print("current_done");
            list<place> ps2 = select_places(vpcloud, x  : place -> true);
            print("pstwo_done");

            (*set<activation_info<??>> activations = activationsat(current); TODO general do not exists for generics ....*)
            activationsat(currenta);
            print("activations_done");

        }
    }
}

component B () {
    bridge<A, B, inline p_pingpong> _b;

    onstartup void toto (bridge<A, B, inline p_pingpong> b0){
        print("> Starting B");
        print(string_of_bridge(b0));
        this._b = b0;
    }

    (*
        port name - not used, only to ease the debugging by providing meaningfull name 
        port    1. listen [on] a logical bridge (i.e. get the guarantee of the bridge)
                2. a bridge can transmit various type of messages (any substet of the protocol) 
                so we specify which point of the protocol is handled by this port using [expecting]
        then the handling of an incomming interaction is delegated to a callback : 'st -> Result<void, error>
    *)
    port truc on this._b :: bridge<A, B, inline p_pingpong> expecting ?ping!pong?ping?ping. = this.handle_ping;
    port truc2 on this._b :: bridge<A, B, inline p_pingpong>  expecting ?ping?ping. = this.handle_ping2;
    port truc3 on this._b :: bridge<A, B, inline p_pingpong>  expecting ?ping. = this.handle_ping3;

    result<void, error> handle_ping (ping msg, !pong?ping?ping. s1) {
        print("ping");
        sleep(50);
        fire(s1, pong()); 

        return Ok(());
    }
    result<void, error> handle_ping2 (ping msg, ?ping?ping. s1) {
        print("ping");

        return Ok(());
    }
    result<void, error> handle_ping3 (ping msg, ?ping. s1) {
        print("ping");

        return Ok(());
    }

    contract handle_ping3 
    invariant 0 == 0 
    ensures "a" == "a"
    returns (res : result<void, error> -> res? == () )
}

component Orchestrator () {
    onstartup void toto () {
        bridge<A, B, inline p_pingpong> b0 = bridge(p_pingpong);
        activation_info<B> c = (spawn B(b0));
        activation_info<A> a2 = (spawn A(b0, c));  
    }
}

component PassivePlayer() {
    onstartup void toto () {
        print("Start passive player"); 
    }
}

component MultiJVMOrchestrator (){
    component Inner (){ (* FIXME needed since @ place can not be used directly in the guardian *)

        onstartup void toto () {
            bridge<A, B, inline p_pingpong> b0 = bridge(p_pingpong);

            vplace<vpcloud> vp1 = vpcloud;
            vplace<vpa> vp2 = vpa;

            print("Start active player"); 
            for( place x in places()) {
                print(place_to_string(x));
            }
            list<place> ps1 = select_places(vpcloud, x  : place -> true);
            list<place> ps2 = select_places(vpa, x  : place -> true);
            place p1 = listget(ps1, 0);
            place p2 = listget(ps2, 0);
            activation_info<B> c = spawn B(b0) @ p1;
            print(">>> b 0");
            print(placeof(c));
            print("<<<");
            (* FIXME A|B should be Top *)
            (*for( activation_info<A|B> x in activationsat(p1)){
                print(x);
            }*)

            (* FIXME TODO  Should be @p2*)
            activation_info<A> a2 = spawn A(b0, c);
            print(">>> a");
            print(placeof(a2));
            print("<<<");
            (* TODO assert placeof(a2) = p2 *)

            (*
            FIXME A|B should be Top
            for( activation_info<A|B> x in activationsat(p2)){
                print("activationsat p2");
                print(x);
            }
            for( activation_info<A|B> x in activationsat(p1)){
                print("activationsat p1");
                print(x);
            }
            *)
        }
    }
    onstartup void toto (){
        spawn Inner();
    }
}

void titi (array<string> args){
    print("apossiblemain");
}
void titia (array<string> args){
    print("Multi player main");
}
void titib (array<string> args){
    print("Passive player main");
}

activation_info<B> b = (spawn B(b0)); (* B() -> call the oncreate method of B with the argument whereas B(A) will be a functor application TODO fix the syntax *) 
activation_info<A> a1 = (spawn A(b0, b));  



(*

(* Ex1.2: event carrying values *)
type ping = string; (* alias de type, if we do this ping and pong will have the same type *) 
type ping of string; (* type constructor ping : string -> ping ; type t of 'a has a constructor t: 'a -> t + value_of_t: t-> 'a *)
type pong of string;

(*  Code diff with Ex1.1
    - event creation ping("ping") pong("bl")
    - print("received ping : %s" value_of_ping(msg))
*)


(*  Ex1.3 - ping-pong between activations of the same component 
    A <====== b0 ======> A
    A:a ----- ping ----> A:b 
        <---- pong ----- 
*)

bridge<A, A, !ping?pong> b0 = bridge();

(* Synchronous wait i.e. can not process incomming request *)
component A () {
    oncreate result<void, error> toto (activation_info<A> b) {
       !ping?pong s0 = b0.initiate_session_with(b);

       ?pong s1 = s0.fire(ping())?;
       tuple<pong, .> res = s1.receive_timeout(10)?;

       return ok(());
    }

    port truc on b0 expecting ?ping!pong = handle_ping 
    
    result<void, error> handle_ping (?ping!pong s0) {
        tuple<ping, !pong> res = s0.receive()?;
        ping msg = res.fst();
        !pong s1 = res.snd();

        print("ping");
        s1.fire(pong()); 

        return ok(());
    }
}

(* Asynchronous wait *)
component A () {
    oncreate result<void, error> toto (activation_info<A> a2) {
       !ping?pong s0 = b0.initiate_session_with(a2);

       ?pong s1 = s0.fire(ping())?;
       tuple<pong, .> res = s1.receive_timeout(10)?;

       return ok(());
    }

    port truc on b0 expecting ?ping!pong = handle_ping 
    port truc2 on b0 expecting ?pong = handle_pong 
    
    result<void, error> handle_ping (?ping!pong s0) {
        tuple<ping, !pong> res = s0.receive()?;
        ping msg = res.fst();
        !pong s1 = res.snd();

        print("ping");
        s1.fire(pong()); 

        return ok(());
    }

    result<void, error> handle_pong (?pong s0) {
        tuple<pong, .> res = s0.receive()?;
        ping msg = res.fst();

        print("pong");

        return ok(());
    }
}


(************************** Other discovery method ***************************)
(*  PB 
    Until now we discover the remote enpoint by passing it by argument
    How can we do more ?
*)
(* Ex4: broadcasting/multicasting 
    A ======= b0 =======> B1 + B2
    A:a1 ----- ping ----> A:a2 
         <---- pong ----- 
*)

bridge<A, B1 | B2, !ping?pong> b0 = bridge();

component A () {
    oncreate result<void, error> toto () {
        (* initiate_sessions : void -> Sequence/List lazy<Result<'st>> one per target activation that are registered on b0 *)
        for(res0 in b0.initiate_sessions()?){
            p_pingpong s0 = res0?;
            if(s0.endpoint().gid() != this.gid()) { (* No point to send msg to itself*)
                !ping?pong s0 = b0.initiate_session_with(b);

                ?pong s1 = s0.fire(ping())?;
                (* wait for answer before contacting the next one *)
                tuple<pong, .> res = s1.receive()?;
            }
        }

       return ok(());
    }

    (* async version *)
    port truc2 on b0 expecting ?pong = handle_pong 
    
    result<void, error> handle_pong (?pong s0) {
        tuple<pong, .> res = s0.receive()?;
        ping msg = res.fst();

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
component B1 () { (*same for B2*)
    oncreate result<void, error> toto () {
        b0.register(this);

        return ok(());
    }

    (* same code as B *)
}

(* Ex5: stream like QPUs
    A ======= b0 ======> B
    A:a ----- request ----> A:a2 
    a   <-- stream of item --- 
*)

type request;
type item;
p_pingpongs = !request µ x. ?item . x;

bridge<A, B, p_pingpongs> b0 = bridge();

(* Code like for classical pingpong *)
*)