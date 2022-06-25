component Dummy {
    onstartup (){
        debug("> init");
        this.methoda();
    }

    void methoda(){
        debug("> methoda");
    }

    component Inner {
        void methodb (){
            debug("> methodb");
        }
    }
}

component TopLevel {
    onstartup (){
        activation_ref<Dummy> d = spawn Dummy();
    }
}