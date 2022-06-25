vplacedef vpa of "placeA";
vplacedef vpcloud of "Cloud";

event ping of;
event pong of;

protocol p_pingpong = !ping?pong.;

(********************* PingPong *********************)
@capturable([MsgCounter])
component A {
    bridge<B, A, inline p_pingpong> _b;
    
    onstartup (bridge<B, A, inline p_pingpong> b0){
        print("> Starting A");
        this._b = b0;
    }

    inport p_in on this._b :: bridge<B, A, inline p_pingpong> expecting (dual p_pingpong) = this.handle_ping;

    void handle_ping (ping msg, !pong. s1) {
        print("ping");
        fire(s1, pong()); 
    }
}

component B {
    bridge<B, A, inline p_pingpong> _b;
    outport p_out on this._b :: bridge<B, A, inline p_pingpong>;

    onstartup (bridge<B, A, inline p_pingpong> b0, activation_ref<A> b) {
        this._b = b0;

        print("> Starting B");
        session<p_pingpong> s0 = initiate_session_with(this.p_out, b);

        ?pong. s1 = fire(s0, ping());
        print("> Ping fired");
        (* TODO add this 
        tuple<pong, !ping!ping.> res = receive(s1, this._b);
        print("pong_or_timeout");
        *)
    }
}


    (* Orchestration logic *)
component PingPong {
    activation_ref<MsgCounter> make_interceptor (
        place -> activation_ref<MsgCounter> factory,
        string intercepted_component_schema,
        place p_of_intercepted
    ){
        place p = current_place();
        return factory(p);
    }

    onstartup () {
        bridge<B, A, inline p_pingpong> bridge0 = bridge(p_pingpong);

        (* First group *) 
        with<MsgCounter> this.make_interceptor{
            activation_ref<A> a = spawn A(bridge0);
        }
        activation_ref<B> b = spawn B(bridge0, a);

        (* Second group *) 
        with<MsgCounter, anonymous> this.make_interceptor{
            activation_ref<A> aa = spawn A(bridge0);
        }
        activation_ref<B> bb = spawn B(bridge0, aa);

        (* Third group - TODO low-level api *)
    }
}
(********************* Interception logic *********************)

(*
    TODO write a msg counter - independent of msg
*)
component MsgCounter {
    onstartup () {
        print(">MsgCounter"); (* count ping *)
    }

    (****************************** Programmer defined state and state handling ******************************)
    int nbr_msg = 0;
    void incr(){
        this.nbr_msg = this.nbr_msg + 1;
    }

    (****************************** Activation onboarding ******************************)
    @onboard([A]) (* List of schemas that can be onboarded by the method *)
    bool onboard_A(activation_ref<A> a, place p_of_a){
        return true;
    }

    (****************************** Session interception  ******************************)
    @sessioninterceptor(true, both)
    option<activation_ref<A>> my_session_interceptor_a(
        dict<activation_id, activation_ref<A>> onboarded_activations, 
        activation_ref<B> from, 
        bridge<B, A, inline p_pingpong> b_inner, 
        string requested_to_schema, 
        ping msg
    ){
        (*  assert onboarded_activations > 1 
            since this do not create activations to process request
        *)
        activation_ref<A> a = pick(onboarded_activations);
        return some(a);
    }

    @sessioninterceptor(false, both)
    option<activation_ref<A>> my_session_interceptor_b(
        dict<activation_id, activation_ref<A>> onboarded_activations, 
        activation_ref<B> from, 
        bridge<B, A, inline p_pingpong> b_inner, 
        activation_ref<A> requested_to, 
        ping msg
    ){
        return some(requested_to);
    }

    (****************************** Msg interception  ******************************)
    (* TODO need polymorphism *)
    @msginterceptor(both)
    option<ping> intercept_ping(
        activation_ref<A> from, 
        activation_ref<B> to, 
        ?pong. continuation_in, 
        !pong. continuation_out, 
        ping msg
    ){
        this.incr();
        
        print("Counted ping");
        print(this.nbr_msg);

        return some(msg);
    }
}


(********************* Guardians and entry points *********************)

component PassivePlayer {
    onstartup () {
        print("Start passive player"); 
    }
}

component MultiJVMOrchestrator {
    onstartup () {
        spawn PingPong();
    }
}

void titia (array<string> args){
    print("Multi player main");
}
void titib (array<string> args){
    print("Passive player main");
}
