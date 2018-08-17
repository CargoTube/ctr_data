-module(ctr_data_sqlite_publication).

-behaviour(ctr_data_publication_if).
-include("ctr_data.hrl").

-define(TABLEVERSION,0.1).
-define(TABLENAME,"ctrpublication").

-export([
         store_publication/1,
         init/0,
         clean_table/1
         ]).

init() ->
    create_table().

clean_table(MaxAge) ->
    maybe_clean_table(?MODULE == ctr_data_publication_if:get_module(), MaxAge).


store_publication(Pub) ->
    #ctr_publication{
       realm = Realm,
       topic = Topic
      } = Pub,
    NewPubId = ctr_utils:gen_global_id(),
    CollectSubs =
        fun(#ctr_subscription{id = SubId, match=Match,
                              subscribers = Subs}, AllSubs) ->
                [ #{id=>SubId, match=>Match,  subs=>Subs}  | AllSubs ]
        end,

    {ok, Found} = ctr_data:match_subscription(Topic, Realm),
    AllSubs = lists:foldl(CollectSubs, [], Found),
    NewPub = Pub#ctr_publication{id = NewPubId, subs=AllSubs},
    Result = do_store(NewPub),
    handle_publication_store_result(Result, NewPub).


handle_publication_store_result([], Publication) ->
    {ok, Publication};
handle_publication_store_result(
  {error,{constraint,"UNIQUE constraint failed: ctrpublication.id"}}, Pub) ->
    NewPubId = ctr_utils:gen_global_id(),
    NewPub = Pub#ctr_publication{id = NewPubId},
    Result = do_store(NewPub),
    handle_publication_store_result(Result, NewPub).




do_store(#ctr_publication{id = Id, pub_sess_id = PubSessId, options = Options,
                          details = Details, subs = Subs, realm = Realm,
                          topic = Topic, ts = TS, arguments = Arguments,
                          argumentskw = ArgumentsKw }) ->
    Sql = "INSERT INTO ctrpublication (id, pub_sess_id, options, details, subs,"
        " realm, topic, ts, arguments, argumentskw) "
        " VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?);",
    Params = [Id, PubSessId, to_json(Options), to_json(Details), to_json(Subs),
              Realm, Topic, iso8601:format(TS), to_json(Arguments),
              to_json(ArgumentsKw)],
    {ok, Con} = ct_data_util:get_sqlite_connection(),
    esqlite3:q(Sql, Params, Con).


to_json(Any) ->
    jsone:encode(Any).

%% from_json(Json) ->
%%     jsone:decode(Json).


create_table() ->
    ok = ct_data_util:setup_sqlite_if_needed(),
    Version = ct_data_util:get_table_version(?TABLENAME),
    ok = maybe_drop_table(Version),
    ok = do_create_table(),
    ok = ctr_data_sqlite_clean:start(),
    ok.


maybe_drop_table({ok, Version}) when Version == ?TABLEVERSION ->
    ok;
maybe_drop_table(_) ->
    {ok, Con} = ct_data_util:get_sqlite_connection(),
    Sql = "DROP TABLE IF EXISTS ctrpublication;",
    ok = esqlite3:exec(Sql, Con),
    ok.

do_create_table() ->
    {ok, Con} = ct_data_util:get_sqlite_connection(),
     Sql =  "CREATE TABLE IF NOT EXISTS ctrpublication ("
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
    ok = esqlite3:exec(Sql, Con),
    ok = ct_data_util:set_table_version(?TABLENAME,?TABLEVERSION),
    ok.

maybe_clean_table(true, _MaxAge) ->
    ok;
maybe_clean_table(_, _MaxAge) ->
    ok.
