vplacedef vpa of "placeA";
vplacedef vpb of "placeB";

type error of ;
event incr of; 
event value of int;


type testa of int;
type testb of int, bool;

testa t = testa(1);
testb tt = testb(1,true);
(* Pas de pattern matching en java *)
int i = tt._0_;
bool ii = tt._1_;

protocol p_protocol = !incr?value!incr?value.;
bridge<A, Counter, inline p_protocol> b0 = bridge(p_protocol);

component A () {
    bridge<A, Counter, inline p_protocol> _b;

    onstartup void toto (bridge<A, Counter, inline p_protocol> b0, activation_info<Counter> b) {
        this._b = b0;

        print("> Starting A");
        session<p_protocol> s0 = initiate_session_with(this._b, b); 

        ?value!incr?value. s1 = fire(s0, incr()); 
        tuple<value, !incr?value.> resa = receive(s1, this._b);  


        ?value. s2 = fire(second(resa), incr());
        tuple<value, .> resb = receive(s2, this._b);  

        value v_a = first(resa); (* Type reconstruction can not handle first(resa) yet *) 
        value v_b = first(resb);

        print("value1:");
        print(v_a._0_);
        print("value2:");
        print(v_b._0_);
    }
}

component Counter () {
    int counter = 0;
    bridge<A, Counter, inline p_protocol> _b;

    onstartup void toto (bridge<A, Counter, inline p_protocol> b0){
        print("> Starting Counter");
        this._b = b0;
    }


    port truc on this._b :: bridge<A, Counter, inline p_protocol> expecting ?incr!value?incr!value. = this.handle_incr1;
    port truc2 on this._b :: bridge<A, Counter, inline p_protocol>  expecting ?incr!value. = this.handle_incr2;


    result<void, error> handle_incr1 (incr msg, !value?incr!value. s1) {
        this.counter = this.counter + 1;
        fire(s1, value(this.counter)); 

        return Ok(());
    }
    result<void, error> handle_incr2 (incr msg, ?incr!value. s1) {
        this.counter = this.counter + 1;
        fire(s1, value(this.counter)); 
        return Ok(());
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
            bridge<A, Counter, inline p_protocol> b0 = bridge(p_protocol);

            print("Start active player"); 
            list<place> ps1 = select_places(vpb, x  : place -> true);
            list<place> ps2 = select_places(vpa, x  : place -> true);
            place p1 = listget(ps1, 0);
            place p2 = listget(ps2, 0);
            activation_info<Counter> c = spawn Counter(b0) @ p1;
            (* FIXME TODO  Should be @p2*)
            activation_info<A> a2 = spawn A(b0, c) @ p2;
        }
    }
    onstartup void toto (){
        spawn Inner();
    }
}

void titia (array<string> args){
    print("Multi player main");
}
void titib (array<string> args){
    print("Passive player main");
}


@@derive rpc<Counter><>()