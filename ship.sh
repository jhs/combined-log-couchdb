#!/bin/sh

[ -z "$plugin_name" ] && read -p "plugin_name: " plugin_name

if [ -d "./rebar" ]; then
  make -C rebar
else
  echo "Missing rebar. Try git submodule update --init"
  exit 1
fi

set -e
set -x

./rebar/rebar clean
./rebar/rebar get-deps
./rebar/rebar update-deps
./rebar/rebar compile

zip -r "$plugin_name.zip" ebin priv
