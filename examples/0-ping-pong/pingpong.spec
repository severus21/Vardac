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

    outport p_out on this._b :: bridge<A, B, inline p_pingpong>;

    onstartup void toto (bridge<A, B, inline p_pingpong> b0, activation_info<B> b) {
        this._b = b0;
        print("> Starting A");
        print(string_of_bridge(b0));
        session<p_pingpong> s0 = initiate_session_with(this.p_out, b);
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