#!/bin/sh

# demonize : http://stackoverflow.com/questions/6830806/running-erlang-shell-as-a-deamon-service

OPTS=$*

NOSHELL="-noshell"
COMPILE=false
CLEAN=false
APP=""
NAME=""

help() {
  MESSAGE=$1
  if [ "x$MESSAGE" != "x" ] ; then
    echo $MESSAGE
  fi
  echo "Usage : start.sh [options]" 
  echo ""
  echo "Options :"
  echo "  -c --console     : Run in console mode"
  echo "  -C --compile     : Compile code before run"
  echo "  -K --clean       : Clean and compile code before run"
  echo "  -h --help        : Display this message"
}

while (( "$#" )); do
  case $1 in
    -c|--console) 
      NOSHELL="" ;;
    -C|--compile) 
      COMPILE=true ;;
    -K|--clean) 
      CLEAN=true ; COMPILE=true ;;
    -h|--help)
      help ; exit 0 ;;
    *)
      help "Invalid option $1" ; exit 1 ;;  
  esac
  shift
done

if [ $CLEAN = true ] ; then
  ./rebar clean
fi
if [ $COMPILE = true ] ; then
  ./rebar compile
fi

erl +pc unicode -pa $PWD/ebin $PWD/apps/*/ebin $PWD/deps/*/ebin $NOSHELL -config $PWD/apps/miki.config -s miki
