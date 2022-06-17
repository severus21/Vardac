event ping of int, bool, long; (* i * warmup * init_timestamp*)
event pong of int, bool, long;
protocol p_pingpong = !ping?pong.;
bridge<Ping, Pong, inline p_pingpong> b0 = bridge(p_pingpong);

(* 

Mono JVM

1) Run N pin pong round 
- only ports + send

2) Run N ping pong ping pong round 
- add receive

3) Select/branch
*)

array<long> array(int n);
result<void, error> tosjon(array<long> rtts, string filename);

component Ping {
    outport p_out expecting (inline p_pingpong);
    inport p_in :: bridge<Ping, Pong, inline p_pingpong> expecting ?pong. = this.callback;

    int n = 0;
    int counter = 0;
    long starttime = 0;
    array<long> rtts;
    
    onstartup (int n, bridge<Ping, Pong, inline p_pingpong> b0, activation_ref<Pong> b) {
        debug("> Starting Ping");
        this.n = n;

        bind(this.p_out, b0);
        bind(this.p_in, b0);

        this.starttime = time();
        this.rrts = array(this.n);

        for(int i in range(0, this.n)){
            this.start(b);
        }

        tojson(this.rrts);
    }
    result<void, error> start(int i, activation_ref<Pong> b){
        session<p_pingpong> s0 = initiate_session_with(this.p_out, b);
        ?pong. s1 = fire(s0, ping(i, false, time()))?; 
    }

    void sumup(){
        long start = this.starttime;
        long end = time();
        long elapse = end - start;
        string tmp = long_to_string(elapse);
        info("Time elapse "+tmp+" ms");
        info("Terminated ueyiqu8R");
        info("JamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglog");
    }

    result<void, error> callback(pong msg, . s){
        this.counter = this.counter + 1;
        aput(this.rrts, i, time() - msg._2_);

        // Last pong
        (* TODO multiple stmts in an if => unable to parse *)
        if( this.counter == this.n ){
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
        . s1 = fire(s0, pong(msg._0_, msg._1_, msg._2_))?; 
        debug(">> Pong fired");
        return ok(());
    }

    contract callback 
    invariant 0 == 0 
    ensures "a" == "a"
    returns (res : result<void, error> -> is_ok(res) )
}

tuple<array<string>, int> main (array<string> args){
    print("apossiblemain");
    int n = 100;
    list<string> nargs = [];
    int skip = -1;
    for(int i in range(0, asize(args) - 1)){
        if( ((aget(args, i)) == "-n") && ((i - 1)< asize(args)) ){ (*(aget ..) needed otherwise not parsed as a binop*)
            n = int_of_string(aget(args, i+1));
            skip = i + 1;
        } else { 
            if (i != skip) {
                append(nargs, aget(args, i));
            }
        }
    }
    return (list2array(nargs), n);
}

component TopLevel {
    onstartup (int n) {
        print(">> Entering toplevel");
        bridge<Ping, Pong, inline p_pingpong> b0 = bridge(p_pingpong);
        activation_ref<Pong> c = (spawn Pong(b0));
        activation_ref<Ping> a2 = (spawn Ping(n, b0, c));  
        print(">> Ending toplevel");
    }
}