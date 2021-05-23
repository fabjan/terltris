#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname terltris -pa ebin -pa deps/esdl2/ebin

main([]) ->
    gui:run(10, 20);
main(["help"]) ->
    usage(),
    halt(0);
main(_) ->
    usage(),
    halt(1).

usage() ->
    io:format("usage: terltris\n").
