
open OUnit2
open OUnitLogger
open Testutils
open AstUtils
open IR
open Utils
open AstUtils
open IRMisc

let fplace = (Error.forge_place "Main.Unittests" 0 0) 
let auto_fplace smth = {place = fplace; value=smth}
include AstUtils2.Mtype.Make(struct let fplace = fplace end)

(************************* AST equality/hashing function *********************)

let hash_mt = [%hash: main_type]
let equal_mt = [%equal: main_type]
let heq_suite () = 
    [
    "heq-type" >:: (function ctx ->
        (* check basic structural equality *)
        let mt1 = mtype_of_ft TPlace in
        let mt2 = mtype_of_ft TPlace in

        assert_equal ~cmp:equal_mt mt1 mt2;
        assert_equal (hash_mt mt1) (hash_mt mt2);

        (* check ignore place for structural equality*)
        let mt3 = { 
            place = fplace;
            value = (CType { place = (Error.forge_place "aaa" 0 0); value = (TFlatType TPlace) }) }
        in
        let mt4 = { 
            place = fplace;
            value = (CType { place = (Error.forge_place "bbb" 0 0); value = (TFlatType TPlace) }) }
        in

        assert_equal ~cmp:equal_mt mt3 mt4;
        assert_equal (hash_mt mt3) (hash_mt mt4);

        (* complex structural equality, same fplace *)
        let fplace = (Error.forge_place "fplace5" 0 0) in
        let mt5 = { place = fplace;
            value =
            (CType
                { place = fplace;
                value =
                (TTuple
                    [{ place = fplace;
                        value =
                        (CType
                            { place = fplace;
                            value =
                            (TActivationRef
                                { place = fplace;
                                value =
                                (CompType
                                    { place = fplace;
                                        value =
                                        (CompTUid
                                        (Atom.craft 15 "A" "A" false))
                                        })
                                })
                            })
                        };
                        { place = fplace;
                        value =
                        (CType { place = fplace; value = (TFlatType TPlace) }) }
                        ])
                })
            } in
            let fplace = (Error.forge_place "fplace6" 0 0) in
            let mt6 = { place = fplace;
                value =
                    (CType
                    { place = fplace;
                    value =
                    (TTuple
                    [{ place = fplace;
                    value =
                    (CType
                    { place = fplace;
                        value =
                        (TActivationRef
                            { place = fplace;
                            value =
                            (CompType
                                { place = fplace;
                                value =
                                (CompTUid
                                    (Atom.craft 15 "A" "A" false))
                                })
                            })
                        })
                    };
                    { place = fplace;
                    value =
                    (CType { place = fplace; value = (TFlatType TPlace) }) }
                    ])
                    })
                    } 
            in

            assert_equal ~cmp:equal_mt mt5 mt6;
            assert_equal (hash_mt mt5) (hash_mt mt6);
        ()
    );
]


(************************* Utils - ftvars/fvars ******************************)
let ftvars_of terms = snd (free_tvars_program Atom.Set.empty terms)
let fvars_of terms = List.map snd (snd (free_vars_program Atom.Set.empty terms))
let to_hints atoms = List.map Atom.hint atoms

let a = Atom.fresh "a"
let b = Atom.fresh "b"
let c = Atom.fresh "c"
let d = Atom.fresh "d"

let tterms_1 = 
[{ place = fplace;
   value =
   { plg_annotations = [];
     v =
     (Typedef
        { place = fplace;
          value =
          (EventDef (
             (Atom.craft 49 "spawn_response_B15_" "spawn_response_B15_" false),
             [{ place = fplace;
                value =
                (CType
                   { place = fplace;
                     value =
                     (TActivationRef
                        { place = fplace;
                          value =
                          (CompType
                             { place = fplace;
                               value =
                               (CompTUid
                                  (Atom.craft 15 "B" "B" false))
                               })
                          })
                     })
                }
               ],
             ()))
          })
     }
   };
  { place = fplace;
    value =
    { plg_annotations = [];
      v =
      (Typedef
         { place = fplace;
           value =
           (EventDef (
              (Atom.craft 48 "spawn_request_B15_" "spawn_request_B15_" false),
              [], ()))
           })
      }
    };
]
let tterms_2 = [
  { place = fplace;
    value =
    { plg_annotations = [];
      v =
      (Stmt
         { place = fplace;
           value =
           (LetStmt (
              { place = fplace;
                value =
                (CType
                   { place = fplace;
                     value =
                     (TBridge
                        { in_type =
                          { place = fplace;
                            value =
                            (CompType
                               { place = fplace; value = CompTBottom })
                            };
                          out_type =
                          { place = fplace;
                            value =
                            (CompType
                               { place = fplace;
                                 value =
                                 (CompTUid
                                    (Atom.craft 13 "A" "A" false))
                                 })
                            };
                          protocol =
                          { place = fplace;
                            value =
                            (SType
                               { place = fplace;
                                 value =
                                 (STSend (
                                    { place = fplace;
                                      value =
                                      (CType
                                         { place = fplace;
                                           value =
                                           (TVar
                                              (Atom.craft 48 "spawn_request_B15_" "spawn_request_B15_" false))
                                           })
                                      },
                                    { place = fplace;
                                      value =
                                      (STRecv (
                                         { place = fplace;
                                           value =
                                           (CType
                                              { place = fplace;
                                                value =
                                                (TVar
                                                   (Atom.craft 49 "spawn_response_B15_" "spawn_response_B15_" false))
                                                })
                                           },
                                         { place = fplace; value = STEnd }
                                         ))
                                      }
                                    ))
                                 })
                            }
                          })
                     })
                },
              (Atom.craft 59 "spawn_static_bridge_B15_in_A13_" "spawn_static_bridge_B15_in_A13_" false),
              { place = fplace;
                value =
                ((LitExpr
                    { place = fplace;
                      value =
                      StaticBridge {
                        id =
                        (Atom.craft 62 "spawn_static_bridge_B15_in_A13_" "spawn_static_bridge_B15_in_A13_" false);
                        protocol_name =
                        (Atom.craft 50 "spawn_protocol_B15_" "spawn_protocol_B15_" false)}
                      }),
                 { place = fplace;
                   value =
                   (CType
                      { place = fplace;
                        value =
                        (TBridge
                           { in_type =
                             { place = fplace;
                               value =
                               (CompType
                                  { place = fplace; value = CompTBottom })
                               };
                             out_type =
                             { place = fplace;
                               value =
                               (CompType
                                  { place = fplace;
                                    value =
                                    (CompTUid
                                       (Atom.craft 13 "A" "A" false))
                                    })
                               };
                             protocol =
                             { place = fplace;
                               value =
                               (SType
                                  { place = fplace;
                                    value =
                                    (STSend (
                                       { place = fplace;
                                         value =
                                         (CType
                                            { place = fplace;
                                              value =
                                              (TVar
                                                 (Atom.craft 48 "spawn_request_B15_" "spawn_request_B15_" false))
                                              })
                                         },
                                       { place = fplace;
                                         value =
                                         (STRecv (
                                            { place = fplace;
                                              value =
                                              (CType
                                                 { place = fplace;
                                                   value =
                                                   (TVar
                                                      (Atom.craft 49 "spawn_response_B15_" "spawn_response_B15_" false))
                                                   })
                                              },
                                            { place = fplace; value = STEnd
                                              }
                                            ))
                                         }
                                       ))
                                    })
                               }
                             })
                        })
                   })
                }
              ))
           })
      }
    };]
let tterms_3 = [
  { place = fplace;
    value =
    { plg_annotations = [];
      v =
      (Typedef
         { place = fplace;
           value =
           (ProtocolDef (
              (Atom.craft 50 "spawn_protocol_B15_" "spawn_protocol_B15_" false),
              { place = fplace;
                value =
                (SType
                   { place = fplace;
                     value =
                     (STSend (
                        { place = fplace;
                          value =
                          (CType
                             { place = fplace;
                               value =
                               (TVar
                                  (Atom.craft 48 "spawn_request_B15_" "spawn_request_B15_" false))
                               })
                          },
                        { place = fplace;
                          value =
                          (STRecv (
                             { place = fplace;
                               value =
                               (CType
                                  { place = fplace;
                                    value =
                                    (TVar
                                       (Atom.craft 49 "spawn_response_B15_" "spawn_response_B15_" false))
                                    })
                               },
                             { place = fplace; value = STEnd }))
                          }
                        ))
                     })
                }
              ))
           })
      }
    };
  ]
let tterms = tterms_1 @ tterms_2 @ tterms_3



let ftvars_suite () = [
    "ftvars" >:: (function ctx ->
        let terms = [
            Typedef (auto_fplace(EventDef (a, [], ())));
            Typedef (auto_fplace(EventDef (b, [], ())));
            Typedef (auto_fplace(EventDef (c, [mtype_of_var a; mtype_of_var b], ())));
        ] in    
        let terms = List.map (function x -> auto_fplace (auto_plgannot x)) terms in
        let ftvars = to_hints (ftvars_of terms) in

        assert_equal ftvars [];
    );
    "ftvars2" >:: (function ctx ->
        let terms = [
            Typedef (auto_fplace(EventDef (a, [], ())));
            Typedef (auto_fplace(EventDef (c, [mtype_of_var a; mtype_of_var b], ())));
        ] in    
        let terms = List.map (function x -> auto_fplace (auto_plgannot x)) terms in
        let ftvars = to_hints (ftvars_of terms) in

        assert_equal ftvars ["b"]
    );
    "ftvars_tterms" >:: (function ctx ->
        let ftvars = to_hints (ftvars_of tterms) in
        let fvars = to_hints (fvars_of tterms) in

        let ft_str = 
          Error.show_list "," (fun out x ->Format.fprintf out "%s" x)  ftvars
        in

        let f_str = 
          Error.show_list "," (fun out x ->Format.fprintf out "%s" x) fvars
        in

        logf ctx `Info "ftvars [%d]{%s}" (List.length ftvars) ft_str;
        logf ctx `Info "fvars [%d]{%s}" (List.length fvars) f_str;
        (* check protocol in StaticBridge protocol_name*)
        assert_equal ftvars ["spawn_protocol_B15_"];
        assert_equal fvars []
    );
    "ftvars_tmp" >:: (function ctx ->
        let ftvars = to_hints (ftvars_of (tterms_1@tterms_3@tterms_2)) in
        let fvars = to_hints (fvars_of (tterms_1@tterms_3@tterms_2)) in

        let ft_str = 
          Error.show_list "," (fun out x ->Format.fprintf out "%s" x) ftvars
        in

        let f_str = 
          Error.show_list "," (fun out x ->Format.fprintf out "%s" x) fvars
        in

        logf ctx `Info "ftvars [%d]{%s}" (List.length ftvars) ft_str;
        logf ctx `Info "fvars [%d]{%s}" (List.length fvars) f_str;
        assert_equal ftvars [];
        assert_equal fvars []
    );
    "insert_into_terms" >:: (function ctx ->
        let tmp = insert_in_terms tterms_2 (tterms_1@tterms_3) in
        assert_equal (tterms_1@tterms_3@tterms_2) tmp
    )

]
let ftvars_error_suite () = [

]


(**********************************************************************)

let coverage_suite () = 
    List.flatten (List.map (function f -> f ()) [
        ftvars_suite;
        heq_suite;
    ])

let error_coverage_suite () = 
    List.flatten (List.map (function f -> f ()) [
        ftvars_error_suite;
    ])

let expected_suite () = [
]


let unittests () =
    "Core" >::: (List.flatten [
        coverage_suite ();
        error_coverage_suite ();
        expected_suite ();
    ])