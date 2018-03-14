#!/usr/bin/env escript

-module(hello).
-compile(export_all).

main(_) ->
    io:fwrite("Hello, world!\n").

