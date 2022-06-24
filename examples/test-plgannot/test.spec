component Dummy {
    onstartup (){
        debug("> init");
        this.methoda();
    }

    void methoda(){
        debug("> methoda");
    }
}

component TopLevel {
    onstartup (){
        activation_ref<Dummy> d = spawn Dummy();
    }
}