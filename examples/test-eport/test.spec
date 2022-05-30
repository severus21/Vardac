type event_a of;

string toStringEventA(event_a e);

component Dummy {
    onstartup (){
        debug("> init");
    }

    (* Receive XXX notification from Akka runtime *)
    eport p_env expecting event_a = this.callback;

    void callback(event_a e){
        print(e);
    }
}

component TopLevel {
    onstartup (){
        activation_ref<Dummy> d = spawn Dummy();
    }
}