event key of string;
event value of int;

protocol p_kv = &{
    "get": !key?value.;
    "put": !tuple<key,value>?bool.;
};


component KVServer {
    bridge<Client, KVServer, p_kv> b;

    onstartup void toto (bridge<Client, KVServer, p_kv> b){
        this.b = b;
    }

    inport p_in on this.b :: bridge<Client, KVServer, p_kv> expecting (dual p_kv) = this.callback;

    void callback (blabel msg, p_kv s) {
        print("callback");

        if(true){
            print("true");
        }
        print("");

        branch s on msg {
            | "get" => s -> { 
                tuple<key, ?value.> tmp = receive(s);
                fire(nth(tmp, 1), option_get(nth(tmp, 0)));
            }
            | "put" => s -> {
                tuple<tuple<key,value>, ?bool.> tmp= receive(s);
                tuple<key, value> res = nth(tmp, 0); 
                (*TODO put(nth(res, 0), nth(res, 1)); *)
                fire(nth(tmp, 1), true);
            }
        }

        return ();
    }

    (*** Impl of get and put out of the scope of the glu, i.e., defined as abstract methods ***)
    (* TODO
    value get(key k);
    bool put(key k, value v); 
    *)
}

component Client {
    bridge<Client, KVServer, p_kv> b;
    activation_ref<KVServer> kv;

    outport p_out on this.b :: bridge<Client, KVServer, p_kv>;

    onstartup void toto (bridge<Client, KVServer, p_kv> b, activation_ref<KVServer> kv){
        this.b = b;
        this.kv = kv;
    }

    void get(key k){
        session<p_kv> s = initiate_session_with(this.p_out, this.kv);

        !key?value. s = select(s, "get");
        ?value. s = fire(s, k);

        nth(receive(s), 0);//TODO current recv-elim can not return :')
    }

    void put(key k, value v){
        session<p_kv> s = initiate_session_with(this.p_out, this.kv);

        !tuple<key, value>?bool. s = select(s, "put");
        ?bool. s = fire(s, (key, value));

        (*assert(nth(receive(s), 0));*) //TODO assert do not exit
    }
}
component TopLevel {
    onstartup void toto (){
        bridge<Client, KVServer, p_kv> b = bridge(p_kv);

        activation_ref<KVServer> kv_a = spawn KVServer(b);
        activation_ref<KVServer> kv_b = spawn KVServer(b);
        activation_ref<Client> c = spawn Client(b, kv_a);
    }
}