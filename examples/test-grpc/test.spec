component Dummy {
    string token = "> token";

    onstartup (){
        debug("> init");
        this.token = "> token2";
    }

    (* TODO void api_a triggers an error in protobuf *)
    @expose 
    int api_a(){
        debug("> api_a");
        debug(this.token);
        this.token = "> token_api_a";
        return 1;
    }

    @expose 
    int api_b(string key){
        debug("> api_b");
        debug(this.token);
        this.token = "> token_api_b" + key;
        return 1;
    }
    @expose 
    int api_c(string key, int i){
        debug("> api_c");
        debug(this.token);
        this.token = "> token_api_c" + key;
        return 1;
    }
}

component TopLevel {
    onstartup (){
        activation_ref<Dummy> d = spawn Dummy();
    }
}