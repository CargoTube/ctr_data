-module(ctr_data_sqlite_publication).

-behaviour(ctr_data_publication_if).
-include("ctr_data.hrl").

-define(TABLEVERSION,0.1).

-export([
         store_publication/1,
         init/0
         ]).

init() ->
    create_table().


store_publication(Pub) ->
    #ctr_publication{
       realm = Realm,
       topic = Topic
      } = Pub,
    %% NewPubId = ctr_utils:gen_global_id(),
    NewPubId = 123,
    CollectSubs =
        fun(#ctr_subscription{id = SubId, match=Match,
                              subscribers = Subs}, AllSubs) ->
                [ {SubId, Match,  Subs}  | AllSubs ]
        end,

    {ok, Found} = ctr_data:match_subscription(Topic, Realm),
    AllSubs = lists:foldl(CollectSubs, [], Found),
    NewPub = Pub#ctr_publication{id = NewPubId, subs=AllSubs},
    Result = do_store(NewPub),
    handle_publication_store_result(Result, NewPub).


handle_publication_store_result(ok, Publication) ->
    {ok, Publication};
handle_publication_store_result({atomic, {error, pub_id_exists}}, Pub) ->
    NewPubId = ctr_utils:gen_global_id(),
    NewPub = Pub#ctr_publication{id = NewPubId},
    Result = do_store(NewPub),
    handle_publication_store_result(Result, NewPub);
handle_publication_store_result(Other, _NewPub) ->
    {error, Other}.




do_store(#ctr_publication{id = Id, pub_sess_id = PubSessId, options = Options,
                          details = Details, subs = Subs, realm = Realm,
                          topic = Topic, ts = TS, arguments = Arguments,
                          argumentskw = ArgumentsKw }) ->
    Sql = "INSERT INTO ctrpublication (id, pub_sess_id, options, details, subs,"
        " realm, topic, ts, arguments, argumentskw) "
        " VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?);",
    Params = [Id, PubSessId, Options, Details, Subs, Realm, Topic, TS,
              Arguments, ArgumentsKw],
    {ok, Con} = ct_data_util:get_sqlite_connection(),
    esqlite3:q(Sql, Params, Con).

create_table() ->
    ok = ct_data_util:setup_sqlite_if_needed(),
    Version = ct_data_util:get_table_version("ctrpublication"),
    maybe_create_table(Version).


maybe_create_table({ok, Version}) when Version == ?TABLEVERSION ->
    ok;
maybe_create_table(_) ->
    {ok, Con} = ct_data_util:get_sqlite_connection(),
    Sql = "DROP TABLE IF EXISTS ctrpublication;"
        "CREATE TABLE ctrpublication ("
        " id INTEGER PRIMARY KEY, "
        " pub_sess_id  INTEGER NOT NULL, "
        " options TEXT, "
        " details TEXT, "
        " subs TEXT, "
        " realm TEXT, "
        " topic TEXT, "
        " ts TEXT, "
        " arguments TEXT, "
        " argumentskw TEXT "
        ");",
    Params = [],
    [] = esqlite:q(Sql, Params, Con),
    ok = ct_data_util:set_table_version("ctrpublication",?TABLEVERSION),
    ok.
