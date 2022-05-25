event value of int;
component Dummy (){
    (* *)
    bridge<Dummy, Dummy, ?value?value?value.> _b;

    void test_splitrecv(?value?value?value. s, int x, int y, int z){
        (* 1st method *)

        tuple<value, ?value?value.> resa = receive(s, this._b);

        (* 2nd method 
            fvars : s 
            indirect fvars (from 3nd method) : x y z
        *)
        int zz = 1;

        tuple<value, ?value.> resb = receive(second(resa), this._b);

        (* 3nd method 
            fvars: x y z
        *)
        value v_a = first(resa); 
        value v_b = first(resb); 
        int c = x + y + z + zz + v_a._0_ + v_b._0_;
    }

}
int i = 1;