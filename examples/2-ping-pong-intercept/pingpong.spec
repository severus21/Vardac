vplacedef vpa of "placeA";
vplacedef vpcloud of "Cloud";

event ping of;
event pong of;

protocol p_pingpong = !ping?pong.;
bridge<A, B, inline p_pingpong> b0 = bridge(p_pingpong);


component CounterInterceptor () {
    int nbr_msg = 0;
    onstartup void toto (){
        print(">CounterInterceptor");
    }

    (* TODO need polymorphism
        option<'a>intercept(activation_info<'b> from, activation_info<'c> ,....')
    *)
    option<ping> intercept(activation_info<A> from, activation_info<B> to, ?pong. continuation, ping msg){
        this.nbr_msg = this.nbr_msg + 1;
        return Some(msg);
    }
}

component A () {
    bridge<A, B, inline p_pingpong> _b;

    onstartup void toto (bridge<A, B, inline p_pingpong> b0, activation_info<B> b) {
        this._b = b0;

        print("> Starting A");
        session<p_pingpong> s0 = initiate_session_with(this._b, b);

        ?pong. s1 = fire(s0, ping());
        print("> Ping fired");
        tuple<pong, !ping!ping.> res = receive(s1, this._b);
        print("pong_or_timeout");
    }
}
@capturable(
    [CounterInterceptor], 
    [port_truc]
)
component B () {
    bridge<A, B, inline p_pingpong> _b;
    
    onstartup void toto (bridge<A, B, inline p_pingpong> b0){
        print("> Starting B");
        this._b = b0;
    }

    port port_truc on this._b :: bridge<A, B, inline p_pingpong> expecting ?ping!pong. = this.handle_ping;

    void handle_ping (ping msg, !pong?ping?ping. s1) {
        print("ping");
        fire(s1, pong()); 
    }
}

component PassivePlayer() {
    onstartup void toto () {
        print("Start passive player"); 
    }
}

(* TODO extends/: CounterInterceptor*)
<Interceptor> activation_info<Interceptor> make_ctx(){
    //compute p from args or ask some interceptor coordinator e.g. to share interceptor for instance
    return spawn Interceptor();(* @ p; *)
}

component MultiJVMOrchestrator (){
    component Inner (){

        onstartup void startup_inner () {
            bridge<A, B, inline p_pingpong> b0 = bridge(p_pingpong);

            vplace<vpcloud> vp1 = vpcloud;
            vplace<vpa> vp2 = vpa;

            print("Start active player"); 
            list<place> ps1 = select_places(vpcloud, x  : place -> true);
            list<place> ps2 = select_places(vpa, x  : place -> true);
            place p1 = listget(ps1, 0);
            place p2 = listget(ps2, 0);
            make_ctx();
            (*with<CounterInterceptor> make_ctx() {
                activation_info<B> b = spawn B(b0) @ p1;
            }*)
            (*activation_info<A> c = spawn A(b0, b);*)
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
