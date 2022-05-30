type event_a of ;

component Dummy {
    onstartup (){
        debug("> init");
    }

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