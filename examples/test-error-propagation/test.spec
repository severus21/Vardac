(* Working example of error propagation *)

component Dummy {
    result<int, error> f1(){
        return err(());
    }

    result<int, error> f2(){
        return ok(1);
    }

    result<int, error> f3(){
        int i = this.f2()? + 3;
        debug("> Seen");
        debug("> i ");
        int j = this.f1()?;
        debug("> Unseen");
        return ok(2);
    }
    
    result<int, error> f4(){
        int k = 3 + this.f3()?;
        return ok(3);
    }

    onstartup (){
        if(is_err(this.f4())){
            debug("> Seen");
        } else {
            debug("> Unseen");
        }
    }
}

component TopLevel {
    onstartup (){
        activation_ref<Dummy> d = spawn Dummy();
    }
}