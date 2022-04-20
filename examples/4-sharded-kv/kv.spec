include 3-minimal-kv/...;

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