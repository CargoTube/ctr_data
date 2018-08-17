-module(ctr_data_sqlite_invocation).
-behaviour(ctr_data_invocation_if).

-include("ctr_data.hrl").
-define(TABLEVERSION, 0.2).
-define(TABLENAME, "ctrinvocation").

-export([
         add_invocation/1,
         invocation_add_result/3,
         get_invocation/2,
         remove_invocation/2,

         clean_table/1,

         init/0
        ]).


init() ->
    create_table().

clean_table(MaxAge) ->
    maybe_clean_table(?MODULE == ctr_data_invocation_if:get_module(), MaxAge).


add_invocation(Invoc) ->
    NewId = ctr_utils:gen_global_id(),
    NewInvoc = Invoc#ctrd_invocation{id = NewId},
    Result = do_store(NewInvoc),
    handle_invocation_store_result(Result, NewInvoc).


handle_invocation_store_result([], Invocation) ->
    {ok, Invocation};
handle_invocation_store_result(
  {error,{constraint,"UNIQUE constraint failed: ctrinvocation.id"}}, Invoc) ->
    NewId = ctr_utils:gen_global_id(),
    NewInvoc = Invoc#ctrd_invocation{id = NewId},
    Result = do_store(NewInvoc),
    handle_invocation_store_result(Result, NewInvoc).


do_store(#ctrd_invocation{id = Id, caller_sess_id = CallerSessId,
                          caller_req_id = CallerReqId, reg_id = RegId,
                          options = Options, realm = Realm, callees = Callees,
                          procedure = Procedure, ts = TS, arguments = Arguments,
                          argumentskw = ArgumentsKw }) ->
    Sql = "INSERT INTO ctrinvocation (id, caller_sess_id, caller_req_id, "
        "reg_id, ts, procedure, options, arguments, argumentskw, callees, "
        " realm) "
        " VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);",
    Params = [Id, CallerSessId, CallerReqId, RegId, iso8601:format(TS),
              Procedure, to_json(Options), to_json(Arguments),
              to_json(ArgumentsKw), to_json(Callees), Realm],
    {ok, Con} = ct_data_util:get_sqlite_connection(),
    esqlite3:q(Sql, Params, Con).


get_invocation(InvocationId, Realm) ->
    {ok, Con} = ct_data_util:get_sqlite_connection(),
    Sql = "SELECT id, caller_sess_id, caller_req_id, "
        "reg_id, ts, procedure, options, arguments, argumentskw, callees, "
        " realm "
        "FROM ctrinvocation WHERE id = ? AND realm = ?;",
    Params = [InvocationId, Realm],
    Result = esqlite3:q(Sql, Params, Con),
    handle_invocation_get_result(Result).


handle_invocation_get_result([Tuple]) ->
    {Id, CallerSessId, CallerReqId, RegId, TS_Iso, Procedure, Options_txt,
     Arguments_txt, ArgumentsKw_txt, Callees_txt, Realm} = Tuple,
    TS = iso8601:parse(TS_Iso),
    Options = from_json(Options_txt),
    Arguments = from_json(Arguments_txt),
    ArgumentsKw = from_json(ArgumentsKw_txt),
    Callees = from_json(Callees_txt),
    {ok, #ctrd_invocation{id = Id, caller_sess_id = CallerSessId,
                          caller_req_id = CallerReqId, reg_id = RegId,
                          options = Options, realm = Realm, callees = Callees,
                          procedure = Procedure, ts = TS, arguments = Arguments,
                          argumentskw = ArgumentsKw}};
handle_invocation_get_result(_) ->
    {error, not_found}.


invocation_add_result(Result, InvocationId, Realm) ->
    Sql = "INSERT INTO ctrinvocation_result (invoc_id, realm, result) "
        " VALUES (?, ?, ?);",
    Params = [ InvocationId, Realm, to_text(Result)],
    {ok, Con} = ct_data_util:get_sqlite_connection(),
    AddResult = esqlite3:q(Sql, Params, Con),
    handle_add_result_result(AddResult).


handle_add_result_result([]) ->
    ok;
handle_add_result_result(Reason) ->
    {error, Reason}.


remove_invocation(Id, Realm) ->
    {ok, Con} = ct_data_util:get_sqlite_connection(),
    Sql = "DELETE FROM ctrinvocation WHERE  id = ? AND realm = ? ;",
    Params = [Id, Realm],
    [] = esqlite3:q(Sql, Params, Con),
    ok = remove_invocation_results(Id, Realm, Con),
    ok.

remove_invocation_results(Id, Realm, Con) ->
    Sql= "DELETE FROM ctrinvocation_result WHERE  invoc_id = ? AND realm = ? ;",
    Params = [Id, Realm],
    [] = esqlite3:q(Sql, Params, Con),
    ok.

to_json(Any) ->
    jsone:encode(Any).

to_text(Any) ->
    io_lib:format("~p", [Any]).

from_json(Json) ->
    jsone:decode(Json).

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
    Sql = "DROP TABLE IF EXISTS ctrinvocation;"
        "DROP INDEX IF EXISTS idx_ctrinvocation_realm;"
        "DROP TABLE IF EXISTS ctrinvocation_result;"
        "DROP TABLE IF EXISTS ctrinvocation_ts;"
        "DROP INDEX IF EXISTS idx_ctrinvocation_result_invoc_id;"
        "DROP INDEX IF EXISTS idx_ctrinvocation_result_realm;",
    ok = esqlite3:exec(Sql, Con),
    ok.

do_create_table() ->
    {ok, Con} = ct_data_util:get_sqlite_connection(),
     Sql =  "CREATE TABLE IF NOT EXISTS ctrinvocation ("
        " id INTEGER PRIMARY KEY, "
        " caller_sess_id  INTEGER NOT NULL, "
        " caller_req_id  INTEGER NOT NULL, "
        " reg_id  INTEGER NOT NULL, "
        " ts TEXT NOT NULL, "
        " procedure TEXT NOT NULL, "
        " options TEXT, "
        " arguments TEXT, "
        " argumentskw TEXT, "
        " callees TEXT, "
        " realm TEXT NOT NULL "
        "); "
        "CREATE INDEX IF NOT EXISTS idx_ctrinvocation_realm "
        "ON ctrinvocation (realm);"
        "CREATE INDEX IF NOT EXISTS idx_ctrinvocation_ts "
        "ON ctrinvocation (ts);"
        "CREATE TABLE IF NOT EXISTS ctrinvocation_result ("
        " invoc_id INTEGER NOT NULL, "
        " realm TEXT NOT NULL, "
        " result TEXT NOT NULL"
        "); "
        "CREATE INDEX IF NOT EXISTS idx_ctrinvocation_result_invoc_id "
        "ON ctrinvocation_result (invoc_id);"
        "CREATE INDEX IF NOT EXISTS idx_ctrinvocation_result_realm "
        "ON ctrinvocation_result (realm);",
    ok = esqlite3:exec(Sql, Con),
    ok = ct_data_util:set_table_version(?TABLENAME,?TABLEVERSION),
    ok.

maybe_clean_table(true, MaxAge) ->
    {ok, Con} = ct_data_util:get_sqlite_connection(),
    Now = iso8601:format(calendar:universal_time()),
    ok = delete_results(MaxAge, Now, Con),
    ok = delete_invocations(MaxAge, Now, Con);
maybe_clean_table(_, _MaxAge) ->
    ok.


delete_invocations(MaxAge, Now, Con) ->
    SqlTemplate = "DELETE FROM ctrinvocation "
        "WHERE datetime(ts) < datetime('~s','-~s') ",
    Sql = io_lib:format(SqlTemplate, [Now, MaxAge]),
    ok = esqlite3:exec(Sql, Con),
    ok.

delete_results(MaxAge, Now, Con) ->
    SqlTemplate = "DELETE FROM ctrinvocation_result "
        "WHERE id IN (SELECT id FROM ctrinvocation WHERE "
        "datetime(ts) < datetime('~s','-~s') )",
    Sql = io_lib:format(SqlTemplate, [Now, MaxAge]),
    ok = esqlite3:exec(Sql, Con),
    ok.
