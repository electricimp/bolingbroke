#!/bin/bash

./rebar get-deps
./rebar compile
ERL_LIBS=$(pwd) erl -pa ebin -pa deps/*/ebin -s bolingbroke -bolingbroke port 18360
