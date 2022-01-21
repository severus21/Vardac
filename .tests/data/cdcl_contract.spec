component A () {
    void m1 ();
    contract m1
    ensures true == true

    void m2 ();
    contract m2 
    returns true == true

    void m3 ();
    contract m3 
    ensures true == true 
    returns true == true

    void m4 ();
    contract m4 
    with int x = 3 and string y = "" 
    ensures true == true 
    returns true == true
}