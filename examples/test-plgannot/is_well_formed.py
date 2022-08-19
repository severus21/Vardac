#!/usr/bin/python3
# coding: utf-8

import os
from pathlib import Path
import re
import sys

if len(sys.argv) == 2:
    build_dir = sys.argv[1]
else:
    build_dir = "compiler-build"
src = (Path(build_dir) / Path("akka/src/main/java/author/project_name")).absolute()


def get_extends_of(content, name):
    re_line = re.compile("^public class "+name +
                         "(\d+) extends (.*(?:\n[^{]*)*){", re.MULTILINE)
    res = re_line.search(content)
    atom_id = res.group(1)
    extends = res.group(2)
    extends = extends.replace('\n', '').replace('\r', '').split(',')
    extends = list(map(lambda x: x.strip(), extends))

    return (atom_id, extends)


def get_decorator_of(content, pattern):
    cl_name, m_name = pattern.split('::')

    # FIXME cl_name is not used

    re_line = re.compile(
        "((?:\s*@.+\n)+)\s*(?:public|private) \S+ "+m_name+"(\d*)", re.MULTILINE)
    res = re_line.search(content)

    atom_id = res.group(2)  # can be empty
    decorators = res.group(1)
    decorators = decorators.replace('\n', '').replace('\r', '').split(',')
    decorators = list(map(lambda x: x.strip(), decorators))

    return (atom_id, decorators)


def get_src_clfile(name):
    for path in src.rglob("*.java"):
        if path.stem.startswith(name):
            return path

    raise Exception(f"Java file for {name} not found!\n {src} {'- '.join(list(src.rglob('*.java')))}")


def check_dummy():
    path = get_src_clfile("Dummy")

    with open(path) as f:
        content = f.read()
        atom_id, extends = get_extends_of(content, "Dummy")
        # FIXME flaky test < Dummy or <Dummy ....
        # Check component: extends
        assert(
            "TOTO" in extends and
            f"com.varda.AbstractComponent<Dummy{atom_id}.Command>" in extends
        )

        # Check method: override
        atom_id, decorators = get_decorator_of(content, "Dummy::methoda")
        assert(
            "@Override" in decorators
        )


def check_inner():
    path = get_src_clfile("Inner")

    with open(path) as f:
        content = f.read()
        atom_id, extends = get_extends_of(content, "Inner")
        # FIXME flaky test < Inner or <Inner ....
        # Check component: extends
        assert(
            "TITI" in extends and
            f"com.varda.AbstractComponent<Inner{atom_id}.Command>" in extends
        )

        # Check method: override
        atom_id, decorators = get_decorator_of(content, "Inner::methodb")
        assert(
            "@Override" in decorators
        )


check_dummy()
check_inner()
