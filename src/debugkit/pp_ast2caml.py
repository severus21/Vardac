#!/usr/bin/python3
# coding: utf-8

import os
from pathlib import Path
import re
import sys

def read_input():
    contents = []
    while True:
        try:
            line = input()
        except EOFError:
            break
        contents.append(line)
    return '\n'.join(contents)

def rewrite_opaque(content):
    return content.replace('<opaque>', 'fplace')

re_line = re.compile(r'{\n*\s*identity\n*\s*=\n*\s*(\d+);\n*\s*hint\n*\s*=\n*\s*"([^"]*)";\n*\s*value\n*\s*=\n*\s*"([^"]*)";\n*\s*builtin\n*\s*=\n*\s*(\w*)\n*\s*}', re.MULTILINE)
def rewrite_atom(content):
    for (atom_id, atom_hint, atom_value, atom_builtin) in re_line.findall(content):
        print(atom_id, atom_hint, atom_value, atom_builtin)
    return re_line.sub(r'(Atom.craft \1 "\2" "\3" \4)', content)

flag_stdout = True
if len(sys.argv) == 3:
    flag_stdout = False 
    with open (sys.argv[1]) as f:
        pp_ast = f.read()
else:
    print("Paste the Varda AST then type Ctrl+D")
    pp_ast = read_input()
    print("\n\n\n")

ocaml_ast = rewrite_atom(rewrite_opaque(pp_ast))

if flag_stdout:
    print("\n\n\n")
    print(ocaml_ast)
else:
    with open(sys.argv[2], 'w') as f:
        f.write(ocaml_ast) 

