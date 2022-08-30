```make debugbox -- playground```

## Tools

* ``pp_ast2caml.py``
* display the logging tree (+ logging handlers)
  1. run vardac on a project with --debug
  2. then ``cat logging_tree.json |jq``
* ``outline.mli#cartography_program ir_program`` outputs the outline of an AST
  * 
    ```bash
      make run ... --debug --debug-selector pass_name --json /tmp/titi.json
      make debugbox -- outline --ir_file /tmp/titi.json
    ```
