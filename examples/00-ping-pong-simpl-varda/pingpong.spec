event ping of;
event pong of;
protocol p_pingpong = !ping?pong.;
bridge<Ping, Pong, inline p_pingpong> b0 = bridge(p_pingpong);

component Ping {
    outport p_out :: bridge<Ping, Pong, inline p_pingpong>;
    inport p_in :: bridge<Ping, Pong, inline p_pingpong> expecting ?pong. = this.callback;
    
    int stop_counter = 0;

    onstartup (bridge<Ping, Pong, inline p_pingpong> b0, activation_ref<Pong> b) {
        debug("> Starting Ping");
        bind(this.p_out, b0);
        bind(this.p_in, b0);

        (* TODO handle start error, need to add builtin fcts *)
        this.start(b);
        this.start(b);
    }
    result<void, error> start(activation_ref<Pong> b){
        session<p_pingpong> s0 = initiate_session_with(this.p_out, b);
        ?pong. s1 = fire(s0, ping())?; 
        debug(">> Ping fired");
    }

    result<void, error> callback(pong msg, . s){
        debug(">> Pong received");
        if(this.stop_counter == 1){ 
            exit(());
        }else{
            this.stop_counter = this.stop_counter + 1;
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