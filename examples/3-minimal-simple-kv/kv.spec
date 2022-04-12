event key of string;
event value of int;

protocol p_kv = &{
    "get": !key?value.;
    "put": !tuple<key,value>?bool.;
};


component KVServer {
    bridge<Client, KVServer, p_kv> b;

    onstartup (bridge<Client, KVServer, p_kv> b){
        this.b = b;
    }

    inport p_in on this.b :: bridge<Client, KVServer, p_kv> expecting (dual p_kv) = this.callback;

    void callback (blabel msg, p_kv s) {
        print("callback");

        if(true){
            print("true");
        }
        print("");

        branch s on msg this.b {
            | "get" => s -> { 
                tuple<key, ?value.> tmp = receive(s, this.b);
                fire(tmp._1, tmp._0);
            }
            | "put" => s -> {
                tuple<tuple<key,value>, ?bool.> tmp = receive(s, this.b);
                tuple<key, value> res = tmp._0; 
                (*TODO put(res._0, res._1); *)
                fire(tmp._1, true);
            }
        }

        return ();
    }

    (*** Impl of get and put out of the scope of the glu, i.e., defined as abstract methods ***)
    value get(key k);
    bool put(key k, value v); 
}

component Client {
    bridge<Client, KVServer, p_kv> b;
    activation_ref<KVServer> kv;

    outport p_out on this.b :: bridge<Client, KVServer, p_kv>;

    onstartup (bridge<Client, KVServer, p_kv> b, activation_ref<KVServer> kv){
        this.b = b;
        this.kv = kv;
    }

    void get(key k){
        session<p_kv> s = initiate_session_with(this.p_out, this.kv);

        !key?value. s = select(s, "get");
        ?value. s = fire(s, k);

        (* We need this intermediate let since there is no unification yet for universal type and receive is universally quantified *)
        (* TODO maybe use polyapp *)
        tuple<int, .> tmp = receive(s, this.b);
        int ret = tmp._0;
        return ();//TODO current recv-elim can not return :')
    }

    void put(key k, value v){
        session<p_kv> s = initiate_session_with(this.p_out, this.kv);

        !tuple<key, value>?bool. s = select(s, "put");
        ?bool. s = fire(s, (k, value()));

        (*assert(receive(s, this.b)._0);*) //TODO assert do not exit
    }
}
component TopLevel {
    onstartup () {
        bridge<Client, KVServer, p_kv> b = bridge(p_kv);

        activation_ref<KVServer> kv_a = spawn KVServer(b);
        activation_ref<KVServer> kv_b = spawn KVServer(b);
        activation_ref<Client> c = spawn Client(b, kv_a);
    }
}