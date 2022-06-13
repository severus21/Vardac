event ping of;
event pong of;
protocol p_pingpong = !ping?pong.;
bridge<Ping, Pong, inline p_pingpong> b0 = bridge(p_pingpong);

int n = 1000;

(* 

Mono JVM

1) Run N pin pong round 
- only ports + send

2) Run N ping pong ping pong round 
- add receive

3) Select/branch
*)

component Ping {
    outport p_out expecting (inline p_pingpong);
    inport p_in :: bridge<Ping, Pong, inline p_pingpong> expecting ?pong. = this.callback;

    int counter = 0;
    long starttime = 0;
    
    onstartup (bridge<Ping, Pong, inline p_pingpong> b0, activation_ref<Pong> b) {
        debug("> Starting Ping");
        bind(this.p_out, b0);
        bind(this.p_in, b0);

        this.starttime = time();
        for(int i in range(0, n)){
            this.start(b);
        }

    }
    result<void, error> start(activation_ref<Pong> b){
        session<p_pingpong> s0 = initiate_session_with(this.p_out, b);
        ?pong. s1 = fire(s0, ping())?; 
    }

    void sumup(){
        long start = this.starttime;
        long end = time();
        long elapse = end - start;
        string tmp = long_to_string(elapse);
        debug("Time elapse "+tmp);
    }

    result<void, error> callback(pong msg, . s){
        this.counter = this.counter + 1;

        // Last pong
        (* TODO multiple stmts in an if => unable to parse *)
        if( this.counter == n ){
            this.sumup();
        }

        return ok(());
    }
}

component Pong {
    inport p_in :: bridge<Ping, Pong, inline p_pingpong> expecting ?ping!pong. = this.callback;

    onstartup (bridge<Ping, Pong, inline p_pingpong> b0){
        debug("> Starting Pong");
        bind(this.p_in, b0);
    }

    result<void, error> callback (ping msg, ?pong. s0) {
        debug("> Ping received");
        . s1 = fire(s0, pong())?; 
        debug(">> Pong fired");
        return ok(());
    }

    contract callback 
    invariant 0 == 0 
    ensures "a" == "a"
    returns (res : result<void, error> -> is_ok(res) )
}

void main (array<string> args){
    print("apossiblemain");
}

component TopLevel {
    onstartup () {
        print(">> Entering toplevel");
        bridge<Ping, Pong, inline p_pingpong> b0 = bridge(p_pingpong);
        activation_ref<Pong> c = (spawn Pong(b0));
        activation_ref<Ping> a2 = (spawn Ping(b0, c));  
        print(">> Ending toplevel");
    }
}