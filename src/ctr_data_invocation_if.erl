-module(ctr_data_invocation_if).
-include("ctr_data.hrl").

-export([
         initialize/0,
         do_run/2
        ]).

-callback init() -> ok.


-define(DATA_MODULE(),
        application:get_env(ctr_data, data_invocation_if, ctr_data_off)).

-type uri() :: binary().
-type error_reason() :: any().
-type id() :: non_neg_integer().
-type invocation() :: #ctrd_invocation{}.



-callback add_invocation( Invocation :: invocation() ) ->
    {ok, UpdatedInvocation :: invocation()}.

-callback update_invocation( Invocation :: invocation() ) ->
    {ok, UpdatedInvocation :: invocation()} | {error, not_found}.

-callback get_invocation(InvocationId :: id(), Realm :: uri()) ->
    {ok, Invocation :: invocation()} | {error, Reason :: error_reason()}.

-callback remove_invocation(InvocationId :: id(), Realm :: uri()) ->
    ok | {error, Reason :: error_reason()}.


initialize() ->
    Module = ?DATA_MODULE(),
    lager:debug("data invocation interface is ~p",[Module]),
    Module:init().

do_run(Function, Arguments) ->
   apply(?DATA_MODULE(), Function, Arguments).
