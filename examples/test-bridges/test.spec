protocol ptest = !int.;

component TestA {
    component A {
        outport p_out expecting (inline ptest);

        onstartup (bridge<A, B, inline ptest> _b){
            debug("TestA> Start A");
            bind(this.p_out, _b);

            this.run();
        }

        result<void, error> run(){
            set<activation_ref<A|B>> tmp = leftactivations(bridgeof(this.p_out))?;
            string n = int_to_string(setlength(tmp));

            debug("TestA> leftactivations "+n);
        }
    }

    component B {
        onstartup (bridge<A, B, inline ptest> _b){
            debug("TestA> Start B");
            bind(this.p_in, _b);

            this.run();
        }

        result<void, error> run(){
            set<activation_ref<A|B>> tmp = rightactivations(bridgeof(this.p_in))?;
        }

        inport p_in :: bridge<A, B, inline ptest> expecting (dual ptest) = this.callback;

        result<void, error> callback(int x, !string?int!string. s){
            debug("TestA> Receive int 1");
        }
    }

    onstartup (){
        bridge<A, B, inline ptest> b2 = bridge(ptest);
        activation_ref<A> a     = spawn A(b2);  
        activation_ref<A> aa    = spawn A(b2);  
        activation_ref<B> aaa   = spawn B(b2);
        activation_ref<B> aaaa  = spawn B(b2);
    }
}

component TopLevel {
    onstartup () {
        print(">> Entering toplevel");

        activation_ref<TestA> test = spawn TestA();

        print(">> Ending toplevel");
    }
}