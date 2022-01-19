(* Sharing information between *)

// Shared type object 
sharedcomponent/object PositiveCounter () {
    local int{|x -> x > 0.1} value;

    void decrement ();
    void increment ();
    int read ();

    (* TODO how to express consistency properties
        - conflicts
        - type
        - ?
     *)
}
(* 
    In practice 
        - for each method we will generate an event component_name_method_name of tuple args of method
        - then for the component [PositiveCounter] a protocol :  positive_counter_shared_protocol = +{"shared_method_name1": !component_name_method_name?return_type_of_method; ...} 
*)


(* shared_object extends [activation_ref] with shared method declated inside a shared component, so we can use
        - .decrement() : args -> Result<res_type, error>
        - .increment()
        ...
    we specify a bridge for multiple reasons :
    1. to be built on top the message passing one (since our main target is actor systems)
    2. to be able to constraint the condifentiality and trust 
    3. to be able to constraint who can use it (by just reusing the bridge mechanism)

    Sync:
    counter.sharedmethod(args)
    counter.timeout_sharedmethod(10, args)

    Async: Do we need this ? People can still do this by hand



*)

(* Ex0 counting A and B running activations for a subsect of activations*)
component A () {
    local shared_object<PositiveCounter> counter;

    (* For each component name C there is a related type name C *)
    oncreate result<void, error> toto (shared_object<PositiveCounter> counter) {
        counter.increment()?;
        this.counter = counter;

        return ok(());
    }
    
    ondestroy result<void, error> toto () {
        this.counter.decrement()?;

        return ok(());
    }
}

component B () {
    (* Same as A *)

}

shared_object<PositiveCounter> counter = shared_object(b0, ...);
(* Question: what kind of communication over bridge by adding a second parameter ``bridge<'a, 'b, _> -> sequence<'st>``:
    - talk to a dedicated activation of the object named [x]
        x = ...
        shared_object(b0, b ->  b.initiate_session_with(x))
    - pick a an activation ?
        shared_obbject(b0, b-> 
            (b.initiate_sessions) pick
        )
    - talk n activations ?
        shared_obbject(b0, b-> 
            (b.initiate_sessions) subset 
        )
*)

(spawn A(counter))?;
(spawn B(counter))?;


(* Ex1 counting A and B running activations *)

shared_object<PositiveCounter> counter = shared_object(b0);
component A () {
    (* For each component name C there is a related type name C *)
    oncreate result<void, error> toto () {
        counter.increment()?;

        return ok(());
    }
    
    ondestroy result<void, error> toto () {
        this.counter.decrement()?;

        return ok(());
    }
}


(* Ex2 sharded basic kvs without consistency guarentees *)
component KVSNode () {
    local int min_key;
    local int max_key;
    local dict<..., activation_ref<KVSNode>> shards;
    local store; // abstract local store, could be anything

    oncreate result<void, error> toto (int min_key, int max_key) {
        this.min_key;
        this.max_key;
    }

    shared result<string, error> get (int key) {
        if( min_key <= key && key < max_key) {
            return ok( store.get(key) );
        } else {
            activation_ref<KVSNode> shard = pick_shard(key);
            
            ... s0 = b0.initiate_session_with(shard)?;
            s0.fire()?;
            ... res = s1.receive()?;

            Ok( res.fst() )
        }

    }

    activation_ref<KVSNode> pick_shard (int key);

    shared result<void, error> put (int key, int value) {}
    (* some mechanisme like a bridge with the parent to discover the other nodes *)
}

(*
keys [0;500[
5 noeuds 
*)
(* Create KVSNode activations *)
list<activation_ref<KVSNode>> shards;
for(l in range(5)) {
    shareds.append(spawn KVSNode(((l*100); (l+1)*100))?);
}

(* Make them discover themselves *)
for (s in b0.initiate_sessions()) {
    s.fire(shards)?;
}











(* Ex3 adding visibility *)

BDD
