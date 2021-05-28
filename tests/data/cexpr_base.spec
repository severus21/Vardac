component A () {}
component B (sig_A a){}
component C = A;
component D = B(A);