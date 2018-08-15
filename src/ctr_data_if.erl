-module(ctr_data_if).
-include("ctr_data.hrl").

-export([
         initialize/0,
         do_run/2
        ]).

-callback init() -> ok.


-define(DATA_MODULE(),
        application:get_env(ctr_data, data_if, ctr_data_off)).

-type uri() :: binary().
-type error_reason() :: any().
-type options() :: map().
-type id() :: non_neg_integer().
-type match() :: exact | prefix | wildcard.
-type subscription() :: #ctr_subscription{}.
-type publication() :: #ctr_publication{}.
-type registration() :: #ctr_registration{}.
-type invocation() :: #ctrd_invocation{}.


%% for broker with subscriptions

-callback list_subscriptions(Realm :: uri()) ->
    {ok, [ subscription() ]} | {error, Reason :: error_reason()}.


-callback lookup_subscription(Procedure :: uri(), Options :: options(),
                               Realm :: uri()) ->
    {ok, subscription()} | {error, Reason :: error_reason()}.


-callback match_subscription(Procedure :: uri(), Realm :: uri()) ->
    {ok, subscription()} | {error, Reason :: error_reason()}.


-callback get_subscription(ProcedureId :: id(), Realm :: uri()) ->
    {ok, subscription()} | {error, Reason :: error_reason()}.


-callback add_subscription(Uri :: uri(), Match :: match(), SessionId :: id(),
                           Realm :: uri()) ->
    { created | added, subscription()} | {error, Reason :: error_reason()}.


-callback  remove_subscription(SubscriptionId :: id(), SessionId :: id(),
                               Realm :: uri() ) ->
    { removed | deleted, subscription() } | {error, Reason :: error_reason()}.


-callback store_publication(Publication :: publication()) ->
    { ok, Publication :: publication() } | { error, Reason :: error_reason() }.


%% for dealer with registrations and invocations

-callback list_registrations(Realm :: uri()) ->
    {ok, RegistratiionList :: [registration()]}.


-callback lookup_registration(Procdure :: uri(), Options :: options(),
                              Realm :: uri()) ->
    {ok, Registration :: registration()} | {error, Reason :: error_reason()}.


-callback match_registration(Procdure :: uri(), Realm :: uri()) ->
    {ok, Registration :: registration()} | {error, Reason :: error_reason()}.

-callback get_registration(ProcdureId :: id(), Realm :: uri()) ->
    {ok, Registration :: registration()} | {error, Reason :: error_reason()}.


-callback add_registration(Procedure :: uri(), Match :: match(),
                           SessionId :: id(), Realm :: uri()) ->
    {created | added, Registration :: registration()} |
    {error, Reason :: error_reason()}.

-callback remove_registration(RegistrationId :: id(), SessionId :: id(),
                              Realm :: uri() ) ->
    {removed | deleted, Registration :: registration()} |
    {error, Reason :: error_reason()}.


%% invocation for keeping track of running calls

-callback add_invocation( Invocation :: invocation() ) ->
    {ok, UpdatedInvocation :: invocation()}.

-callback get_invocation(InvocationId :: id(), Realm :: uri()) ->
    {ok, Invocation :: invocation()} | {error, Reason :: error_reason()}.

-callback remove_invocation(InvocationId :: id(), Realm :: uri()) ->
    ok | {error, Reason :: error_reason()}.


initialize() ->
    Module = ?DATA_MODULE(),
    lager:debug("data interface is ~p",[Module]),
    Module:init().

do_run(Function, Arguments) ->
   Module = ?DATA_MODULE(),
   apply(Module, Function, Arguments).
