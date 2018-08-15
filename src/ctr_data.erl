-module(ctr_data).

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

-define(SUB_RUN(Func, Args),
        ctr_data_subscription_if:do_run(Func, Args)).
-define(PUB_RUN(Func, Args),
        ctr_data_publication_if:do_run(Func, Args)).
-define(REG_RUN(Func, Args),
        ctr_data_registration_if:do_run(Func, Args)).
-define(INV_RUN(Func, Args),
        ctr_data_registration_if:do_run(Func, Args)).

list_subscriptions(Realm) ->
    ?SUB_RUN(?FUNCTION_NAME, [Realm]).

lookup_subscription(Procedure, Options, Realm) ->
    ?SUB_RUN(?FUNCTION_NAME, [Procedure, Options, Realm]).


match_subscription(Procedure, Realm) ->
    ?SUB_RUN(?FUNCTION_NAME, [Procedure, Realm]).


get_subscription(ProcedureId, Realm) ->
    ?SUB_RUN(?FUNCTION_NAME, [ProcedureId, Realm]).


add_subscription(Uri, _Match, SessionId, Realm) ->
    ?SUB_RUN(?FUNCTION_NAME, [Uri, _Match, SessionId, Realm]).


remove_subscription(SubscriptionId, SessionId, Realm) ->
    ?SUB_RUN(?FUNCTION_NAME, [SubscriptionId, SessionId, Realm]).


store_publication(Publication) ->
    ?PUB_RUN(?FUNCTION_NAME, [Publication]).


list_registrations(Realm) ->
    ?REG_RUN(?FUNCTION_NAME, [Realm]).


lookup_registration(Procedure, Options, Realm) ->
    ?REG_RUN(?FUNCTION_NAME, [Procedure, Options, Realm]).


match_registration(Procedure, Realm) ->
    ?REG_RUN(?FUNCTION_NAME, [Procedure, Realm]).

get_registration(ProcedureId, Realm) ->
    ?REG_RUN(?FUNCTION_NAME, [ProcedureId, Realm]).


add_registration(Procedure, Match, SessionId, Realm) ->
    ?REG_RUN(?FUNCTION_NAME, [Procedure, Match, SessionId, Realm]).

remove_registration(RegistrationId, SessionId, Realm) ->
    ?REG_RUN(?FUNCTION_NAME, [RegistrationId, SessionId, Realm]).

add_invocation(Invocation) ->
    ?INV_RUN(?FUNCTION_NAME, [Invocation]).

get_invocation(InvocationId, Realm) ->
    ?INV_RUN(?FUNCTION_NAME, [InvocationId, Realm]).

remove_invocation(InvocationId, Realm) ->
    ?INV_RUN(?FUNCTION_NAME, [InvocationId, Realm]).

init() ->
    ctr_data_subscription_if:initialize(),
    ctr_data_publication_if:initialize(),
    ctr_data_registration_if:initialize(),
    ctr_data_invocation_if:initialize(),
    ok.
