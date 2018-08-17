-module(ctr_data_sqlite_invocation).
-behaviour(ctr_data_invocation_if).

-include("ctr_data.hrl").
-define(TABLEVERSION, 0.1).

-export([
         add_invocation/1,
         invocation_add_result/3,
         get_invocation/2,
         remove_invocation/2,

         init/0
        ]).


init() ->
    create_table().


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
    Sql = "DELETE FROM ctrinvocation WHERE  id = ? AND realm = ? ;"
        " DELETE FROM ctrinvocation_result WHERE  invoc_id = ? AND realm = ? ;",
    Params = [Id, Realm, Id, Realm],
    Result = esqlite3:q(Sql, Params, Con),
    handle_invocation_remove_result(Result).

handle_invocation_remove_result([]) ->
    ok;
handle_invocation_remove_result(Error) ->
    {error, Error}.


to_json(Any) ->
    jsone:encode(Any).

to_text(Any) ->
    io_lib:format("~p", [Any]).

from_json(Json) ->
    jsone:decode(Json).

create_table() ->
    ok = ct_data_util:setup_sqlite_if_needed(),
    Version = ct_data_util:get_table_version("ctrpublication"),
    ok = maybe_drop_table(Version),
    ok = do_create_table(),
    ok.


maybe_drop_table({ok, Version}) when Version == ?TABLEVERSION ->
    ok;
maybe_drop_table(_) ->
    {ok, Con} = ct_data_util:get_sqlite_connection(),
    Sql = "DROP TABLE IF EXISTS ctrinvocation;"
        "DROPT TABLE IF EXISTS ctrinvocation_result;",
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
        "CREATE TABLE IF NOT EXISTS ctrinvocation_result ("
        " invoc_id INTEGER NOT NULL, "
        " realm TEXT NOT NULL, "
        " result TEXT NOT NULL"
        "); " ,
    ok = esqlite3:exec(Sql, Con),
    ok = ct_data_util:set_table_version("ctrpublication",?TABLEVERSION),
    ok.
