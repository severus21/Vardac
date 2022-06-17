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

array<long> arrayof(int n);
void aput(array<long> arr, int i, long v);
result<void, error> tojson(array<long> rtts, string filename);

component Ping {
    outport p_out expecting (inline p_pingpong);
    inport p_in :: bridge<Ping, Pong, inline p_pingpong> expecting ?pong. = this.callback;

    int n = 0;
    int warmup = 0;
    int warmup_counter = 0;
    int counter = 0;
    long starttime = 0;
    array<long> rtts = arrayof(0);

    activation_ref<Pong> b = ();
    
    onstartup (int n, int warmup, bridge<Ping, Pong, inline p_pingpong> b0, activation_ref<Pong> b) {
        debug("> Starting Ping");
        this.n = n;
        this.warmup = warmup;
        this.b = b;

        bind(this.p_out, b0);
        bind(this.p_in, b0);

        this.starttime = time();
        this.rtts = arrayof(this.n);
        if(this.warmup > 0 ){
            this.start_warmup();
        } else {
            this.start();
        }
    }

    result<void, error> start_warmup(){
        for(int i in range(0, this.warmup)){
            session<p_pingpong> s0 = initiate_session_with(this.p_out, this.b);
            ?pong. s1 = fire(s0, ping(i, true, time()))?; 
        }
    }

    result<void, error> start(){
        for(int i in range(0, this.n)){
            session<p_pingpong> s0 = initiate_session_with(this.p_out, this.b);
            ?pong. s1 = fire(s0, ping(i, false, time()))?; 
        }
    }

    void sumup(){
        long start = this.starttime;
        long end = time();
        long elapse = end - start;
        string tmp = long_to_string(elapse);
        info("Time elapse "+tmp+" ms");
        tojson(this.rtts, "rtts.json");
        info("Terminated ueyiqu8R");
        info("JamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglog");
    }

    result<void, error> callback(pong msg, . s){
        if(msg._1_){
            this.warmup_counter = this.warmup_counter + 1;
            string x = int_to_string(this.warmup_counter);
            print("warmup_counter "+x);

            // Last pong
            if( this.warmup_counter == this.warmup ){
                this.start();
            }
        } else {
            this.counter = this.counter + 1;
            long endTimestamp = time();
            print(int_to_string(msg._0_));
            aput(this.rtts, msg._0_, endTimestamp - msg._2_);

            // Last pong
            (* TODO multiple stmts in an if => unable to parse *)
            if( this.counter == this.n ){
                this.sumup();
            }
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

tuple<array<string>, tuple<int,int>> main (array<string> args){
    print("apossiblemain");
    int n = 100;
    int warmup = 100;
    list<string> nargs = [];
    int skip = -1;
    for(int i in range(0, asize(args) - 1)){
        if( ((aget(args, i)) == "-n") && ((i - 1)< asize(args)) ){
            n = int_of_string(aget(args, i+1));
            skip = i + 1;
        } else { 
            if( ((aget(args, i)) == "-warmup") && ((i - 1)< asize(args)) ){
                warmup = int_of_string(aget(args, i+1));
                skip = i + 1;
            } else {
                if (i != skip) {
                    append(nargs, aget(args, i));
                }
            }
        }
    }
    return (list2array(nargs), (n, warmup));
}

component TopLevel {
    onstartup (int n, int warmup) {
        print(">> Entering toplevel");
        bridge<Ping, Pong, inline p_pingpong> b0 = bridge(p_pingpong);
        activation_ref<Pong> c = (spawn Pong(b0));
        activation_ref<Ping> a2 = (spawn Ping(n, warmup, b0, c));  
        print(">> Ending toplevel");
    }
}