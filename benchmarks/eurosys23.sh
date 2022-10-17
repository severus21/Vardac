#!/bin/bash

rm -rf eurosys23-*.stdout eurosys23-*.stderr
python3 manage.py flush

python3 main.py count > eurosys23-count.stdout 2> count.stderr

python3 main.py run --bench-selector mpp* --usre_re > eurosys23-mpp.stdout 2> mpp.stderr
python3 main.py run --bench-selector ms* --usre_re --exclude docker > eurosys23-ms.stdout 2> ms.stderr 
python3 main.py run --bench-selector ycsb --usre_re > eurosys23-ycsb.stdout 2> ycsb.stderr