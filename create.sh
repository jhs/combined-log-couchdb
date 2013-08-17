#!/bin/sh

if [ -d "./rebar" ]; then
  make -C rebar
else
  echo "Missing rebar. Try git submodule update --init"
  exit 1
fi

year=`date +%Y`

[ -z "$plugin_name" ] && read -p "plugin_name: " plugin_name
[ -z "$author_name" ] && read -p "author_name: " author_name
[ -z "$author_email" ] && read -p "author_email: " author_email

# Rebar might say to add -f or --force or force=1, implying that this script wants those options. So support them.
force=""
if [ "$1" = "-f" -o "$1" = "--force" -o "$1" = "force=1" ]; then
  force="force=1"
fi

set -x

./rebar/rebar create \
  template="couch_plugin" name="$plugin_name" year="$year" author_name="$author_name" author_email="$author_email" $force
