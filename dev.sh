#!/bin/bash

rebar3 get-deps
rebar3 compile

erl -sname bolingbroke \
    -pa _build/default/lib/*/ebin \
    -s bolingbroke -bolingbroke port 18360 -bolingbroke start_examples true -bolingbroke update_interval_ms 1000

# Browse to http://localhost:18360/?bolingbroke
