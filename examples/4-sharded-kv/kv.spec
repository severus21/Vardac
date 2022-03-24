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

        nth(receive(s), 0);//TODO current recv-elim can not return :')
    }

    void put(key k, value v){
        session<p_kv> s = initiate_session_with(this.p_out, this.kv);

        !tuple<key, value>?bool. s = select(s, "put");
        ?bool. s = fire(s, (k, value));

        (*assert(nth(receive(s), 0));*) //TODO assert do not exit
    }
}

component KVStore {
    (* TODO can use onboardin_activations instead (nb not yet availabe lin msginterceptor) *)
    dict<activation_id, activation_ref<KVServer>> shards = {};

    int user_defined_rooting_policy(key k);

    dict<session_id, label> delayed_sessions;

    activation_ref<KVServer> rooting(key k){
        (* TODO add shards[...] notations *)
        return get2dict(this.shards, this.user_defined_rooting_policy(k)); 
    }

    (*** Interception logic ***)
    @onboard([KVServer])
    void onboard(activation_ref<KVStore> shard){ 
        add2dict(this.shards, shard);
    }

    @sessioninterceptor(false, both)
    option<activation_ref<KVServer>> delaying_selection(
        dict<activation_id, activation_ref<KVServer>> onboarded_activations, 
        activation_ref<Client> from, 
        bridge<Client, KVServer, inline p_kv> b_inner, 
        string requested_to_schema, 
        blabel msg
    ){
        (* TODO this.delayed_sessions[s_id] = flag;*)
        return none; (* TODO maybe an issue *)
    }

    void establishing_session(activation_id s_in_id, key k){
        (* TODO introduce the notation delayed_sessions[x] for dict *)
        blabel l = get2dict(this.delayed_sessions, s_in_id);
        activation_ref<KVServer> to = this.rooting(k);

        (* TODO how to known the outport here ? *)
        (dual p_kv) s_out = initiate_session_with(1, to);

        branch s_out on l {
            | "get" => s -> { (* TODO s_out register s *) }
            | "put" => s -> { (* TODO s_out register s *) }
        }

        return ();
    }

    @msginterceptor(both)
    option<key> intercept_get_request(
        activation_ref<KVServer> from, 
        activation_ref<Client> to,  (* TODO not known *)   
        ?value. s_in,
        ?value. s_out, (* TODO delayed i.e. can not exists yet .... *)
        key msg
    ){
        key k = msg;
        (dual p_kv) s_out_init = this.establishing_session(sessionid(s_in), k);
        (* TODO register s_out_init *)

        return some(msg);
    }

    @msginterceptor(both)
    option<bool> intercept_put_request(
        activation_ref<KVServer> from, 
        activation_ref<Client> to,  (* TODO not known *)   
        ?bool. s_in,
        ?bool. s_out, (* TODO delayed i.e. can not exists yet .... *)
        tuple<key, value> msg
    ){
        key k = nth(msg, 0);

        (dual p_kv) s_out_init = this.establishing_session(sessionid(s_in), k);
        (* TODO register s_out_init *)

        return some(msg);
    }
}

component TopLevel {
    option<activation_ref<KVStore>> singleton_interceptor;

    activation_ref<KVStore> policy(
        place -> activation_ref<KVStore> factory,
        string intercepted_component_schema,
        place p_of_intercepted
    ){
        if(this.singleton_interceptor == none()){
            this.singleton_interceptor = some(factory(current_place()));
        }

        return option_get(this.singleton_interceptor);
    }

    onstartup () {
        bridge<Client, KVServer, p_kv> b = bridge(p_kv);

        with<KVStore, anonymous> this.policy {
            activation_ref<KVServer> kv_a = spawn KVServer(b);
            activation_ref<KVServer> kv_b = spawn KVServer(b);
        }
        activation_ref<Client> c = spawn Client(b, kv_a);
    }
}