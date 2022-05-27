component Dummy {
    onstartup (){
        debug("> init");
    }

    @expose 
    int api_a(){
        debug("> api_a");
        return 1;
    }

    @expose 
    int api_b(string key){
        debug("> api_b");
        return 2;
    }
    @expose 
    int api_c(string key, int i){
        debug("> api_c");
        return 3 + i;
    }
}

component TopLevel {
    onstartup (){
        activation_ref<Dummy> d = spawn Dummy();
    }
}