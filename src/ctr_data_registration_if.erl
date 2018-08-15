
-module(ctr_data_registration_if).
-include("ctr_data.hrl").

-export([
         initialize/0,
         do_run/2
        ]).

-callback init() -> ok.


-define(DATA_MODULE(),
        application:get_env(ctr_data, data_registration_if, ctr_data_off)).

-type uri() :: binary().
-type error_reason() :: any().
-type options() :: map().
-type id() :: non_neg_integer().
-type match() :: exact | prefix | wildcard.
-type registration() :: #ctr_registration{}.


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


initialize() ->
    Module = ?DATA_MODULE(),
    lager:debug("data registration interface is ~p",[Module]),
    Module:init().

do_run(Function, Arguments) ->
   apply(?DATA_MODULE(), Function, Arguments).
