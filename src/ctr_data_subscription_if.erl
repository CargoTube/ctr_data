-module(ctr_data_subscription_if).
-include("ctr_data.hrl").

-export([
         initialize/0,
         do_run/2
        ]).

-callback init() -> ok.


-define(DATA_MODULE(),
        application:get_env(ctr_data, data_subscription_if, ctr_data_off)).

-type uri() :: binary().
-type error_reason() :: any().
-type options() :: map().
-type id() :: non_neg_integer().
-type match() :: exact | prefix | wildcard.
-type subscription() :: #ctr_subscription{}.


%% for broker with subscriptions

-callback list_subscriptions(Realm :: uri()) ->
    {ok, [ subscription() ]} | {error, Reason :: error_reason()}.


-callback lookup_subscription(Procedure :: uri(), Options :: options(),
                               Realm :: uri()) ->
    {ok, subscription()} | {error, Reason :: error_reason()}.


-callback match_subscription(Procedure :: uri(), Realm :: uri()) ->
    {ok, [subscription()] } | {error, Reason :: error_reason()}.


-callback get_subscription(ProcedureId :: id(), Realm :: uri()) ->
    {ok, subscription()} | {error, Reason :: error_reason()}.


-callback add_subscription(Uri :: uri(), Match :: match(), SessionId :: id(),
                           Realm :: uri()) ->
    { created | added, subscription()} | {error, Reason :: error_reason()}.


-callback  remove_subscription(SubscriptionId :: id(), SessionId :: id(),
                               Realm :: uri() ) ->
    { removed | deleted, subscription() } | {error, Reason :: error_reason()}.

initialize() ->
    Module = ?DATA_MODULE(),
    lager:debug("data subscription interface is ~p",[Module]),
    Module:init().

do_run(Function, Arguments) ->
   apply(?DATA_MODULE(), Function, Arguments).
