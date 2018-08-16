-module(ctr_data_ram_subscription).
-behaviour(ctr_data_subscription_if).
-include("ctr_data.hrl").

-export([
         list_subscriptions/1,
         lookup_subscription/3,
         match_subscription/2,
         get_subscription/2,

         add_subscription/4,
         remove_subscription/3,

         init/0,

         dump_all/0
         ]).

init() ->
    create_table().

get_subscription(Id, Realm) ->
    MatchHead = #ctr_subscription{id=Id, realm=Realm, _='_'},
    Guard = [],
    GiveObject = ['$_'],
    MatchSpec = [{MatchHead, Guard, GiveObject}],
    Lookup =
        fun() ->
                case mnesia:select(ctr_subscription, MatchSpec, write) of
                    [Subscription] ->
                        {ok, Subscription};
                    _ ->
                        {error, not_found}
                end
        end,
    Result = mnesia:transaction(Lookup),
    handle_subscription_get_result(Result).


list_subscriptions(Realm) ->
    MatchHead = #ctr_subscription{realm=Realm, _='_'},
    Guard = [],
    GiveObject = ['$_'],
    MatchSpec = [{MatchHead, Guard, GiveObject}],
    Lookup =
        fun() ->
                case mnesia:select(ctr_subscription, MatchSpec, write) of
                    List when is_list(List) ->
                        {ok, List};
                    Other ->
                        {error, Other}
                end
        end,
    Result = mnesia:transaction(Lookup),
    handle_subscription_list_result(Result).

handle_subscription_list_result({atomic, {ok, List}}) ->
    {ok, List};
handle_subscription_list_result(Other) ->
    lager:error("subscription get list error: ~p", Other),
    {ok, []}.

match_subscription(Topic, Realm) ->
    MatchHead = #ctr_subscription{realm=Realm, uri=Topic, _='_'},
    Guard = [],
    GiveObject = ['$_'],
    MatchSpec = [{MatchHead, Guard, GiveObject}],
    Lookup =
        fun() ->
                case mnesia:select(ctr_subscription, MatchSpec, write) of
                    List when is_list(List) ->
                        {ok, List};
                    Other ->
                        {error, Other}
                end
        end,
    Result = mnesia:transaction(Lookup),
    handle_subscription_match_result(Result).

handle_subscription_match_result({atomic, {ok, List}}) ->
    {ok, List};
handle_subscription_match_result(Other) ->
    lager:error("subscription match error: ~p", Other),
    {ok, []}.

lookup_subscription(Topic, Options , Realm) ->
    Match = maps:get(match, Options, exact),
    MatchHead = #ctr_subscription{realm=Realm, match=Match, uri=Topic, _='_'},
    Guard = [],
    GiveObject = ['$_'],
    MatchSpec = [{MatchHead, Guard, GiveObject}],
    Lookup =
        fun() ->
                case mnesia:select(ctr_subscription, MatchSpec, write) of
                    [Subscription] ->
                        {ok, Subscription};
                    _ ->
                        {error, not_found}
                end
        end,
    Result = mnesia:transaction(Lookup),
    handle_subscription_get_result(Result).


handle_subscription_get_result({atomic, Result}) ->
    Result;
handle_subscription_get_result(Other) ->
    lager:error("subscription get/lookup error: ~p", Other),
    {error, not_found}.



add_subscription(Uri, _Match, SessionId, Realm) ->
    NewId = ctr_utils:gen_global_id(),
    NewSub = #ctr_subscription{id = NewId,
                               uri = Uri,
                               realm = Realm,
                               match = exact,
                               subscribers = [SessionId],
                               created = calendar:universal_time()
                              },

    MatchHead = #ctr_subscription{uri=Uri, realm=Realm, _='_'},
    Guard = [],
    GiveObject = ['$_'],
    MatchSpec = [{MatchHead, Guard, GiveObject}],

    WriteIfNew =
        fun(Id, Sub) ->
                case mnesia:wread({ctr_subscription, Id}) of
                    [] ->
                        ok = mnesia:write(Sub),
                        {created, Sub};
                    _ ->
                        {error, id_exists}
                end
        end,

    Store =
        fun() ->
                case mnesia:select(ctr_subscription, MatchSpec, write) of
                    [#ctr_subscription{subscribers = Subs } = Subscription] ->
                        NewSubs = [ SessionId |
                                    lists:delete(SessionId, Subs)],
                        NewSubscription = Subscription#ctr_subscription{
                                            subscribers = NewSubs
                                           },
                        ok = mnesia:write(NewSubscription),
                        {added, NewSubscription};
                    [] ->
                        WriteIfNew(NewId, NewSub)
                end
        end,
    Result = mnesia:transaction(Store),
    handle_store_result(Result, NewSub).

handle_store_result({atomic, {created, Subscription}}, _) ->
    {created, Subscription};
handle_store_result({atomic, {added, Subscription}}, _) ->
    {added, Subscription};
handle_store_result({atomic, {error, id_exists}}, Subscription) ->
    #ctr_subscription{ realm = Realm, subscribers = [SessionId],
                       uri = Uri, match = Match } = Subscription,
    add_subscription(Uri, Match, Realm, SessionId).

remove_subscription(SubId, SessionId, _Realm) ->
    DeleteOrUpdateSubscription =
        fun([], Subscription) ->
                mnesia:delete({ctr_subscription, SubId}),
                {deleted, Subscription};
           (_, Subscription) ->
                ok = mnesia:write(Subscription),
                {removed, Subscription}
        end,

    Delete =
        fun() ->
                case mnesia:wread({ctr_subscription, SubId}) of
                    [#ctr_subscription{subscribers = Subs } = Subscription] ->
                        NewSubscriber = lists:delete(SessionId, Subs),
                        UpdatedSubscription = Subscription#ctr_subscription{
                                            subscribers = NewSubscriber
                                           },
                        DeleteOrUpdateSubscription(NewSubscriber,
                                                   UpdatedSubscription);
                    [] ->
                        {error, not_found}
                end
        end,
    Result = mnesia:transaction(Delete),
    handle_delete_result(Result).


handle_delete_result({atomic, {deleted, Subscription}}) ->
    {deleted, Subscription};
handle_delete_result({atomic, {removed, Subscription}}) ->
    {removed, Subscription};
handle_delete_result({atomic, {error, not_found}}) ->
    {error, not_found}.



dump_all() ->
    Print = fun(Entry, _) ->
                    lager:debug("~p", [Entry]),
                    ok
            end,
    Transaction = fun() ->
                          lager:debug("*** subscriptions ***"),
                          mnesia:foldl(Print, ok, ctr_subscription),
                          lager:debug("*** end ***"),
                          ok
                  end,
    mnesia:transaction(Transaction).



create_table() ->
    mnesia:delete_table(ctr_subscription),
    SubDef = [{attributes, record_info(fields, ctr_subscription)},
              {ram_copies, [node()]},
              {index, [realm, uri, match]}
             ],
    {atomic, ok} = mnesia:create_table(ctr_subscription, SubDef),
    ok.
