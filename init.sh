#!/bin/sh

year=`date +%Y`

[ -z "$plugin_name" ] && read -p "plugin_name: " plugin_name
[ -z "$author_name" ] && read -p "author_name: " author_name
[ -z "$author_email" ] && read -p "author_email: " author_email

# Rebar might say to add -f or --force or force=1, implying that this script wants those options. So support them.
force=""
if [ "$1" = "-f" -o "$1" = "--force" -o "$1" = "force=1" ]; then
  force="force=1"
fi

set -e

./rebar create -C template/rebar/null \
  template="couch_plugin" name="$plugin_name" year="$year" author_name="$author_name" author_email="$author_email" $force

echo ""
echo "## Success"
echo ""
echo "To build your plugin: ./rebar compile generate"
echo ""
echo "Your plugin release will be in rel/$plugin_name"
