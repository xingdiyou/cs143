#!/bin/bash

set -ex

diff <(lexer bad.cl | parser | semant) <(lexer bad.cl | parser | ./semant)
