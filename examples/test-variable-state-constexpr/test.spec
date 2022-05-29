int global = 43; //Error if used in *.impl since it is compiled away before (peval)

component Dummy {
    string token = "static_token";
    onstartup (){
        debug("1> "+this.token);
        this.token = "initialized_token";//seems to be compiled away FIXME
        debug("2> "+this.token);
    }

    @expose 
    int api_a(){
        this.token = "api_a_token";
        debug("3> "+this.token);
        return 1;
    }
}

component TopLevel {
    onstartup (){
        activation_ref<Dummy> d = spawn Dummy();
    }
}