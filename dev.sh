#!/bin/bash

./rebar get-deps
./rebar compile
ERL_LIBS=$(pwd) erl -sname bolingbroke \
    -pa ebin -pa deps/*/ebin \
    -s bolingbroke -bolingbroke port 18360 -bolingbroke start_examples true -bolingbroke update_interval_ms 1000
