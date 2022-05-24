component A {}
component C = A;
component D = B();
component E = B(A);
component F = B(A, C);