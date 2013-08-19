#!/bin/sh
#
# This whole script should be a rebar plugin, or at least an escript.

[ -z "$plugin_name" ] && read -p "plugin_name: " plugin_name

if [ -d "./rebar" ]; then
  make -C rebar
else
  echo "Missing rebar. Try git submodule update --init"
  exit 1
fi

set -e

[ -d "deps" ] || ./rebar/rebar get-deps
# [ -z "$skip_update" ] && ./rebar/rebar update-deps
./rebar/rebar compile

vsn=`cat "ebin/$plugin_name.app" | perl -ne 'print "$1\n" if(m|vsn,"(.*)"|)'`

dist="$plugin_name-$vsn"
rm -rf "./$dist" "$dist.zip"

mkdir -p "$dist"

cp -r ebin "$dist"
cp -r priv "$dist"

if [ -d "deps" ]; then
  for dep in deps/* ; do
    dep=`basename "$dep"`
    mkdir -p "$dist/deps/$dep"

    cp -r "deps/$dep/ebin" "$dist/deps/$dep/ebin"
    if [ -d "$deps/$dep/priv" ]; then
      cp -r "deps/$dep/priv" "$dist/deps/$dep/priv"
    fi
  done
fi

zip -r "$dist.zip" "$dist"
rm -rf "./$dist"

echo ""
echo "Done: $dist.zip"
