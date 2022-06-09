event value of int;

(*** Test 1 ***)
protocol p = !value!value!value.;
component Mock {
    outport p_out :: bridge<Mock, Dummy, inline p>;

    onstartup (bridge<Mock, Dummy, inline p> _b, activation_ref<Dummy> d){
        debug("Test1> Start Mock");
        bind(this.p_out, _b);

        this.run(d);
    }

    result<void, error> run(activation_ref<Dummy> d){
        session<p> s0 = initiate_session_with(this.p_out, d);

        debug("Test1> Send 1");
        ?value?value. s1 = fire(s0, value(1))?;
        debug("Test1> Send 2");
        ?value. s2 = fire(s1, value(2))?;
        debug("Test1> Send 3");
        . s3 = fire(s2, value(3))?;
    }
}

(* TODO test non determinism *)

component Dummy {

    onstartup (bridge<Mock, Dummy, inline p> _b){
        debug("Test1> Start Dummy");
        bind(this.p_in, _b);
    }

    inport p_in :: bridge<Mock, Dummy, inline p> expecting ?value?value?value. = this.callback;

    result<void, error> callback(value x, ?value?value. s){
        (*assert(x._0_ == 1);*)
        debug("Test1> Receive 1");

        (* 1st method *)
        tuple<value, ?value.> resa = receive(s);
        debug("Test1> Receive 2");

        (* 2nd method 
            fvars : s 
            indirect fvars (from 3nd method) : x y z
        *)
        int zz = 1;

        tuple<value, .> resb = receive(second(resa));
        debug("Test1> Receive 3");

        (* 3nd method 
            fvars: x y z
        *)
        value v_a = first(resa); 
        value v_b = first(resb); 
        int c = zz + x._0_ + v_a._0_ + v_b._0_;
        debug("Test1> Result "+int_to_string(c)); (* sould be 7 *)
    }

}

(*** Test 2 ***)
protocol ptest2 = !int?string!int?string.;

component A {
    outport p_out :: bridge<A, B, inline ptest2>;

    onstartup (bridge<A, B, inline ptest2> _b, activation_ref<B> d){
        debug("Test2> Start A");
        bind(this.p_out, _b);

        this.run(d);
    }

    result<void, error> run(activation_ref<B> d){
        session<p> s0 = initiate_session_with(this.p_out, d);

        ?string!int?string. s1 = fire(s0, 1)?;
        debug("Test2> Send int 1");

        tuple<string, !int?string.> res1 = receive(s1);
        debug("Test2> Receive value 1");

        ?string. s3 = fire(second(res1), 2)?;
        debug("Test2> Send int 2");

        tuple<string, .> s3 = receive(s3);
        debug("Test2> Receive value 2");
    }
}

(* TODO test non determinism *)

component B {
    onstartup (bridge<A, B, inline ptest2> _b){
        debug("Test2> Start B");
        bind(this.p_in, _b);
    }

    inport p_in :: bridge<A, B, inline ptest2> expecting (dual ptest2) = this.callback;

    result<void, error> callback(int x, !string?int!string. s){
        debug("Test2> Receive int 1");

        ?int!string. s1 = fire(s, "x")?;
        debug("Test2> Send string 1");

        tuple<int, !string?int!string.> resa = receive(s1);
        debug("Test2> Receive int 2");

        ?int!string. s1 = fire(second(resa), "y")?;
        debug("Test2> Send string 2");

        int v_a = first(resa); 
        int c = x + v_a;
        debug("Test2> Result "+int_to_string(c)); (* sould be 7 *)
    }
}

component TopLevel {
    onstartup () {
        print(">> Entering toplevel");
        bridge<Mock, Dummy, inline p> b0 = bridge(p);
        activation_ref<Dummy> a = spawn Dummy(b0);
        activation_ref<Mock> b = spawn Mock(b0, a);  

        bridge<A, B, inline ptest2> b2 = bridge(p);
        activation_ref<B> a = spawn B(b2);
        activation_ref<A> b = spawn A(b2, a);  
        print(">> Ending toplevel");
    }
}