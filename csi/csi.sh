#!/bin/sh

DIR=$(dirname "$(readlink -f "$0")")

OCAMLRUNPARAM="${OCAMLRUNPARAM} s=96k" \
  "$DIR"/csi -s AUTO -c "$DIR"/cr.conf -C CR "$@"
