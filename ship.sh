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

[ -z "$skip_clean" ] && ./rebar/rebar clean
./rebar/rebar get-deps
[ -z "$skip_update" ] && ./rebar/rebar update-deps
./rebar/rebar compile

vsn=`cat "ebin/$plugin_name.app" | perl -ne 'print "$1\n" if(m|vsn,"(.*)"|)'`

dist="$plugin_name-$vsn"
rm -rf "./$dist" "$dist.zip"

mkdir -p "$dist"

cp -r ebin "$dist"
cp -r priv "$dist"

zip -r "$dist.zip" "$dist"
rm -rf "./$dist"

echo ""
echo "Done: $dist.zip"
