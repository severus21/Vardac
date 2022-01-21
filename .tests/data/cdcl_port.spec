component A () {
    port port_name on c :: bridge<A, A, !int.> expecting !int. = toto;
}