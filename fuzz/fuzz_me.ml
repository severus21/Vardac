let identity x =
    Crowbar.check_eq x (if x = 2 then 0 else 2)

let () = 
    Crowbar.(add_test ~name:"identity function" [int] (fun i -> identity i))