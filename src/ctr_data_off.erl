-module(ctr_data_off).

-behaviour(ctr_data_subscription_if).
-behaviour(ctr_data_publication_if).
-behaviour(ctr_data_registration_if).
-behaviour(ctr_data_invocation_if).

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
         invocation_add_result/3,
         get_invocation/2,
         remove_invocation/2,

         init/0
        ]).

list_subscriptions(_Realm) ->
    {ok, []}.

lookup_subscription(_Procedure, _Options, _Realm) ->
    {error, not_found}.


match_subscription(_Procedure, _Realm) ->
    {error, not_found}.


get_subscription(_ProcedureId, _Realm) ->
    {error, not_found}.


add_subscription(_Uri, _Match, _SessionId, _Realm) ->
    {error, not_implemented}.


remove_subscription(_SubscriptionId, _SessionId, _Realm) ->
    {error, not_found}.


store_publication(_Publication) ->
    {error, not_implemented}.


list_registrations(_Realm) ->
    {error, not_implemented}.


lookup_registration(_Procdure, _Options, _Realm) ->
    {error, not_found}.


match_registration(_Procdure, _Realm) ->
    {error, not_found}.

get_registration(_ProcdureId, _Realm) ->
    {error, not_found}.


add_registration(_Procedure, _Match, _SessionId, _Realm) ->
    {error, not_implemented}.

remove_registration(_RegistrationId, _SessionId, _Realm) ->
    {error, not_found}.


%% invocation for keeping track of running calls

add_invocation(_Invocation) ->
    {error, not_implemented}.

invocation_add_result(_Result, _InvocationId, _Realm) ->
    {error, not_found}.

get_invocation(_InvocationId, _Realm) ->
    {error, not_found}.

remove_invocation(_InvocationId, _Realm) ->
    {error, not_found}.

init() ->
     ok.
