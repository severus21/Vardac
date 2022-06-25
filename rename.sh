#!/bin/bash

shopt -s globstar

# rename *.varch to *.varch
rename (){
    echo "Rename **/*.$1 -> **/*.$2"
    read -p "Do you want to rename ? [y/n]" flag

    if [[ "$flag" == "y" ]]; then
        for f in **/*.$1; do
            mv "$f" "$(dirname $f)/$(basename -- "$f" .$1).$2"
        done
    fi
}

rename "spec" "varch"
rename "impl" "vimpl"