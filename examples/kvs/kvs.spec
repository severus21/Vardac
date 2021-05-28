// Parameter types
type k;
type v;
type error;

// Custom transaction types
type begintx;
type txid;
type abort;
type ack;
type commit;

type action_protocol = µ x. 
&{ (* non deterministic fire *)
    "get" { | true } : !k ?Result<v, error> . x; (* recursif *)
    "put": !Tuple<k,v> ?Result<void, error> . x { | true }; (* recursif *)
    "abort": !abort ?ack . ; (* non-recursif *)
    "commit": !commit ?Result<void, error> . (* non-recursif *)
};

type tc_protocol = ?txid (inline action_protocol);
type full_tx_protocol = !begintx (inline tc_protocol);

// Shared type object 
component PositiveCounter () {
    local int value;
    (*TODO not yet supported by the Akka plg local int{|x -> x > 0.1} value;*)

    void decrement ();
    void increment ();
    int read ();

    (* TODO how to express consistency properties
        - conflicts
        - type
        - ?
     *)
}

(* Open system - part of the client is outside the scope of this work *)
component Client() {
    local txid tx_id;

    onstartup void init () {
        // Initialize communication with the KVS
        (*FIXME add C() tunnel *)
        full_tx_protocol s = initiate_session().first()?;
        // blocking call
        tc_protocol s1 = fire(s, begintx())?;
        this.tx_id = receive(s1)?; (* TODO specify timeout second args*)
    }

    Result<void, error> put(k k, v v);

    (*contract put
    ensures this.tx_id > 0*)
    (* Pattern matching no supported in cook and in Java
    returns res -> {
        match res {
            //FIXME Ok(e) should be Ok(()) 
            case Err(e) => return false;
            case Ok(e) => return true;
        }
    }
    *)

    Result<v, error> get(k k);
    (*contract get 
    ensures this.tx_id > 0*)
}

int c1 = 1; (*TODO channel C1 (include full_tx_protocol);*)
component TransactionManager () {
    (* Singleton activation specification *)
    (* FIXME cook does support shared object yet 
    ghost shared PositiveCounter __nbr_activations = spawn PositiveCounter();*)
    local txid last_tx_id = 0;

    (* Handling activation initialization *)
    onstartup void init () {
        ghost!{ __nbr_activations.increment(); }
    }

    ondestroy void destroy () {
        ghost!{ __nbr_activations.decrement(); }
    }

    (* Business logic *)
    txid fresh_tx_id ();

    (*contract fresh_tx_id
    with txid current_tx_id = this.last_tx_id
    returns res -> (this.last_tx_id == res) && (res > current_tx_id)*)


    Result<void, ActivationError> start_transaction (full_tx_protocol session) { 
        (*We provide a reference implementation for this method*)
        txid tx_id = this.fresh_tx_id(); (* this denotes the current activation *)

        (* expr? - "?" is used to unpacke a result expr, if it is an error it is returned *)
        (* FIXME it is an activation and not a component *)
        (component TransactionCoordinator) coordinator = (spawn TransactionCoordinator(tx_id, session))?;

        (* A version where we spawn the coordinator near to the client *)
        (* TODO Not yet supported by the compiler 
        place ndc = this.nearest_dc(session.remote_endpoint.ipaddress);
        (component TransactionCoordinator) coordinator = (spawn TransactionCoordinator(tx_id, session) @ ndc)?;
        *)
        return Ok(1);
    }

    (*contract start_transaction
    with txid current_tx_id = this.last_tx_id 
    ensures true*)
    (* Pattern matching no supported in cook and in Java
    returns res -> {
        match res {
            case Err(e) => return false;
            case Ok(e) => return last_tx_id > current_tx_id;
        }
    }
    *)

    Result<place_selector, error> nearest_dc (ipaddress ip) {
        (* TODO place or place information ? ADD builting place/place information *)
        (* TODO place and label are not yet supported by Akka plg List<place> dcs = places(l"DC::*");

        place dc = pick(dcs); //smthing_with(dcs);
        place p = pick(dc.children);
        *)
        int p = 1;

        return p;
    }

    (* Message passing - handling protocol initialization *)
    (* TODO inline full_tx_protocol not yet supported only session type yet -> should partially evaluated first, port port_begintransaction on c1 expecting full_tx_protocol = this.start_transaction;*)
    port port_begintransaction on c1 expecting ?int. = this.start_transaction;
}

component TransactionCoordinator () {
    local txid tx_id;

    onstartup void init(txid tx_id, tc_protocol s) {
        this.tx_id = tx_id;
        action_protocol s1 = fire(s, Ok(tx_id) );


        (* TODO async dispatcher *)
    }

    (*contract init
    ensures this.tx_id > 0*)

    (* Message passing - handling action *)
    (* FIXME c1 tunnel *)
    (* TODO inlue action_porotocl, port port_handleaction on 1 expecting action_protocol = this.dispatcher;*)
    port port_handleaction on 1 expecting ?int. = this.dispatcher;

    (* rev -> Result<...,NetworkError>*)
    Result<void, Error> dispatcher(action_protocol s) {
        (*//https://medium.com/@shannonbarnes_85491/algebraic-data-types-in-scala-701f3227fe91
        *)
        //comment
        (* TODO match is not yet supported by akka plg
        match switch(s) {
            case "abort" => { 
                Tuple<action_protocol, abort> res = receive(s)?; 

                this.abort()?;

                fire(res.first(), ack())?;
            }
            case "commit" => {
                Tuple<action_protocol, commit> res = receive(s)?; 

                fire(res.first(), this.commit())?;
            }
            case "get" => {
                // FIXME qu'implique le wait ici
                // Wait for the msg to process
                Tuple<action_protocol, k> res = receive(s)?; 

                //Do the inner processing 
                Result<v,error> value = this.get(res.last());

                // Send back the value
                fire(res.first(), value)?; 
            }
            case "put" => { 
                Tuple<action_protocol, Tuple<k,v>> res = receive(s)?; 

                Result<void, Error> value = this.put(res.last().first(), res.last().first());

                fire(res.first(), value)?;
            }
        } *)
        return Ok(1);
    }

    (* Business logic *)
    Result<void, Error> abort ();
    Result<void, Error> commit ();
    Result<v, Error> get (k key);
    Result<void, Error> put (k key, v value);
}