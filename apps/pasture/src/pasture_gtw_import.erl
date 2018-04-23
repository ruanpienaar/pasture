-module(pasture_gtw_import).
-export([import/1
        ]).

import(Filename) ->
    Dir = "/Users/ruanpienaar/code/pasture/historic_run/data/",
    Files = filelib:fold_files(Dir,
                               "201.*",
                               false,
                               fun(Filename, AccIn) ->  [Filename|AccIn] end,
                               []),
    %% true = register(spawn),
    import_files(Files).

import_files([]) ->
    ok;
import_files([H|T]) ->
    ok.