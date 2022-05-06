event key of string;
event value of int;

protocol p_kv = &{
    l_get: !key?value.;
    l_put: !tuple<key,value>?bool.;
};


component KVServer {
    onstartup (bridge<Client, KVServer, p_kv> b){
        print(">>> Starting a KVServer instance");
        bind(this.p_in, b);
    }

    inport p_in :: bridge<Client, KVServer, p_kv> expecting (dual p_kv) = this.callback;

    result<void, error> callback (blabel msg, p_kv s) {
        print("callback");

        if(true){
            print("true");
        }
        print("");

        branch s on msg {
            | l_get => s -> { 
                tuple<key, ?value.> tmp = receive(s);
                fire(tmp._1, tmp._0)?;
            }
            | l_get => s -> {
                tuple<tuple<key,value>, ?bool.> tmp = receive(s);
                tuple<key, value> res = tmp._0; 
                (*TODO put(res._0, res._1); *)
                fire(tmp._1, true)?;
            }
        }

        return ();
    }

    (*** Impl of get and put out of the scope of the glu, i.e., defined as abstract methods ***)
    value get(key k){
        return value(this._get(k));
    }
    int _get(key k);
    bool put(key k, value v); 

    @expose 
    int api_get(string key);

    @expose 
    bool api_put(string key, int value);
}

component Client {
    bridge<Client, KVServer, p_kv> b;
    activation_ref<KVServer> kv;

    outport p_out :: bridge<Client, KVServer, p_kv>;

    onstartup (bridge<Client, KVServer, p_kv> b, activation_ref<KVServer> kv){
        print(">>> Starting a Client instance");
        bind(this.p_out, b);
        this.kv = kv;
        
        this.put(key("Key1"), value(10));
        (*this.get(key("Key1"));*)
    }

    result<void, error> get(key k){
        session<p_kv> s = initiate_session_with(this.p_out, this.kv);

        !key?value. s = select(s, l_get)?;
        ?value. s = fire(s, k)?;

        (* We need this intermediate let since there is no unification yet for universal type and receive is universally quantified *)
        (* TODO maybe use polyapp *)
        tuple<int, .> tmp = receive(s);
        int ret = tmp._0;
        
        (* TODO print(f">> get success {{ret}}");*)
        print(">> get success");
        print(int_to_string(ret));
        print("");
        return ();//TODO current recv-elim can not return :')
    }

    result<void, error> put(key k, value v){
        session<p_kv> s = initiate_session_with(this.p_out, this.kv);

        !tuple<key, value>?bool. s = select(s, l_get)?;
        ?bool. s = fire(s, (k, v))?;

        (*assert(receive(s, this.b)._0);*) //TODO assert do not exit
    }
}
component TopLevel {
    onstartup () {
        print(">> Entering toplevel");
        bridge<Client, KVServer, p_kv> b = bridge(p_kv);

        activation_ref<KVServer> kv_a = spawn KVServer(b);
        activation_ref<KVServer> kv_b = spawn KVServer(b);
        activation_ref<Client> c = spawn Client(b, kv_a);
        print(">> Ending toplevel");
    }
}