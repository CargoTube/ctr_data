-module(ctr_data).

-behaviour(ctr_data_if).

-export([
         list_subscriptions/1,
         lookup_subscription/3,
         match_subscription/2,
         get_subscription/2,
         add_subscription/4,
         remove_subscription/3,
         store_publication/1,

         list_registrations/1,
         lookup_registration/3,
         match_registration/2,
         get_registration/2,
         add_registration/4,
         remove_registration/3,

         add_invocation/1,
         get_invocation/2,
         remove_invocation/2,

         init/0
        ]).

-define(RUN(Func, Args),
        ctr_data_if:do_run(Func, Args)).

list_subscriptions(Realm) ->
    ?RUN(?FUNCTION_NAME, [Realm]).

lookup_subscription(Procedure, Options, Realm) ->
    ?RUN(?FUNCTION_NAME, [Procedure, Options, Realm]).


match_subscription(Procedure, Realm) ->
    ?RUN(?FUNCTION_NAME, [Procedure, Realm]).


get_subscription(ProcedureId, Realm) ->
    ?RUN(?FUNCTION_NAME, [ProcedureId, Realm]).


add_subscription(Uri, _Match, SessionId, Realm) ->
    ?RUN(?FUNCTION_NAME, [Uri, _Match, SessionId, Realm]).


remove_subscription(SubscriptionId, SessionId, Realm) ->
    ?RUN(?FUNCTION_NAME, [SubscriptionId, SessionId, Realm]).


store_publication(Publication) ->
    ?RUN(?FUNCTION_NAME, [Publication]).


list_registrations(Realm) ->
    ?RUN(?FUNCTION_NAME, [Realm]).


lookup_registration(Procedure, Options, Realm) ->
    ?RUN(?FUNCTION_NAME, [Procedure, Options, Realm]).


match_registration(Procedure, Realm) ->
    ?RUN(?FUNCTION_NAME, [Procedure, Realm]).

get_registration(ProcedureId, Realm) ->
    ?RUN(?FUNCTION_NAME, [ProcedureId, Realm]).


add_registration(Procedure, Match, SessionId, Realm) ->
    ?RUN(?FUNCTION_NAME, [Procedure, Match, SessionId, Realm]).

remove_registration(RegistrationId, SessionId, Realm) ->
    ?RUN(?FUNCTION_NAME, [RegistrationId, SessionId, Realm]).

add_invocation(Invocation) ->
    ?RUN(?FUNCTION_NAME, [Invocation]).

get_invocation(InvocationId, Realm) ->
    ?RUN(?FUNCTION_NAME, [InvocationId, Realm]).

remove_invocation(InvocationId, Realm) ->
    ?RUN(?FUNCTION_NAME, [InvocationId, Realm]).

init() ->
    ctr_data_if:initialize().
