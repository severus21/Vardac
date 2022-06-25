event key of string;
event value of int;



component Dummy {
    int docker_port = 6379;

    onstartup (){
        debug("> init with docker at "+ip(current_place())+" "+int_to_string(this.docker_port));
    }

    (*** Impl of get and put out of the scope of the glu, i.e., defined as abstract methods ***)
    result<value, error> get(key k){
        result<int, error> tmp = this._get(k._0_);
        if( is_ok(tmp)){
            return ok(value(get_ok(tmp)));
        } else{
            return err(get_err(tmp));
        }
    }
    result<int, error> _get(string k);

    result<bool, error> put(key k, value v){
        return this._put(k._0_, v._0_);
    }
    result<bool, error> _put(string k, int v);

    @expose 
    int api_get(string k){
        result<value, error> tmp = this.get(key(k));
        if( is_err(tmp) ){
            return -1;
        } else {
            return (get_ok(tmp))._0_;
        }
    }

    @expose 
    bool api_put(string k, int v){
        result<bool, error> tmp = this.put(key(k), value(v));
        if (is_err(tmp)){
            return false;
        } else {
            return get_ok(tmp);
        }
    }
}

component TopLevel {
    onstartup (){
        activation_ref<Dummy> d = spawn Dummy();
    }
}