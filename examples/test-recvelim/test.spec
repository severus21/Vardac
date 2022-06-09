event value of int;

protocol p = !value!value!value.;

component Mock {
    outport p_out :: bridge<Mock, Dummy, inline p>;

    onstartup (bridge<Mock, Dummy, inline p> _b, activation_ref<Dummy> d){
        debug("> Start Mock");
        bind(this.p_out, _b);

        session<p> s0 = initiate_session_with(this.p_out, d);

        debug("> Send 1");
        ?value?value. s1 = fire(s0, value(1))?;
        debug("> Send 2");
        ?value. s2 = fire(s1, value(2))?;
        debug("> Send 3");
        . s3 = fire(s2, value(3))?;
    }
}

(* TODO test non determinism *)

component Dummy {

    onstartup (bridge<Mock, Dummy, dual p> _b){
        debug("> Start Dummy");
        bind(this.p_in, _b);
    }

    inport p_in :: bridge<Mock, Dummy, dual p> expecting ?value?value?value. = this.callback;

    void callback(?value?value. s, int x){
        (*assert(x._0_ == 1);*)
        debug("> Receive 1");

        (* 1st method *)
        tuple<value, ?value.> resa = receive(s);
        debug("> Receive 2");

        (* 2nd method 
            fvars : s 
            indirect fvars (from 3nd method) : x y z
        *)
        int zz = 1;

        tuple<value, .> resb = receive(second(resa));
        debug("> Receive 3");

        (* 3nd method 
            fvars: x y z
        *)
        value v_a = first(resa); 
        value v_b = first(resb); 
        int c = x + v_a._0_ + v_b._0_;
    }

}
component TopLevel {
    onstartup () {
        print(">> Entering toplevel");
        bridge<Mock, Dummy, inline p> b0 = bridge(p);
        activation_ref<Dummy> a = spawn Dummy(b0);
        activation_ref<Mock> b = spawn Mock(b0, a);  
        print(">> Ending toplevel");
    }
}