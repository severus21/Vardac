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

    void incr(){
        this.nbr_msg = this.nbr_msg + 1;
    }

    (* TODO need polymorphism
        option<'a>intercept(activation_ref<'b> from, activation_ref<'c> ,....')
    *)
    @intercept
    option<ping> intercept(activation_ref<A> from, activation_ref<B> to, ?pong. continuation_in, !pong. continuation_out, ping msg){
        this.incr();
        return Some(msg);
    }
}

component A () {
    bridge<A, B, inline p_pingpong> _b;
    outport p_out on this._b :: bridge<A, B, inline p_pingpong>;

    onstartup void toto (bridge<A, B, inline p_pingpong> b0, activation_ref<B> b) {
        this._b = b0;

        print("> Starting A");
        session<p_pingpong> s0 = initiate_session_with(this.p_out, b);

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

    port port_truc on this._b :: bridge<A, B, inline p_pingpong> expecting (dual p_pingpong) = this.handle_ping;

    void handle_ping (ping msg, !pong. s1) {
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
<Interceptor> activation_ref<Interceptor> make_ctx(){
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

            (* Interception context - group activations together
                inside group no interception
                interception at group boundaries

                N.B. with can be nested
            *)
            with<CounterInterceptor> make_ctx() {
            (* withanon<CounterInterceptor> make_ctx() { TODO *)
                activation_ref<B> b = spawn B(b0) @ p1;
                print("a");
            }

            (* Low level interception - should be defined as a citem *)
            (* TODO
            component MyInterceptor = MakeInterceptor(CounterInterceptor, [B; A]); 
            activation_ref<MyInterceptor> i = spawn MyInterceptor(i);
            activation_ref<B> b = ispawn (i, B(b0)); (* expose b identity + expose b type + but all proxy everythong trough i *)
            activation_ref<B> b = ispawnanon (i, B(b0)); (* do not expose b identity *)
            *)




            activation_ref<A> c = spawn A(b0, b);
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
