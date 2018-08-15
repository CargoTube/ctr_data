-module(ctr_data_publication_if).
-include("ctr_data.hrl").

-export([
         initialize/0,
         do_run/2
        ]).

-callback init() -> ok.


-define(DATA_MODULE(),
        application:get_env(ctr_data, data_publication_if, ctr_data_off)).

-type error_reason() :: any().
-type publication() :: #ctr_publication{}.


-callback store_publication(Publication :: publication()) ->
    { ok, Publication :: publication() } | { error, Reason :: error_reason() }.


initialize() ->
    Module = ?DATA_MODULE(),
    lager:debug("data publication interface is ~p",[Module]),
    Module:init().

do_run(Function, Arguments) ->
   apply(?DATA_MODULE(), Function, Arguments).
