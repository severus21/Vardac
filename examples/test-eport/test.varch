type event_a of;
type event_b of;

string toStringEventA(event_a e);

component Dummy {
    onstartup (){
        debug("> init");
    }

    (* Receive XXX notification from Akka runtime *)
    eport p_env expecting event_a = this.callback;

    void callback(event_a e){
        print("> PreStart");
    }
    eport p_b expecting event_b = this.callback_b;

    void callback_b(event_b e){
        print("> PostStop");
    }
}

component TopLevel {
    onstartup (){
        activation_ref<Dummy> d = spawn Dummy();
    }
}