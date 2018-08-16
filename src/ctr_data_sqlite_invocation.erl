-module(ctr_data_sqlite_invocation).
-behaviour(ctr_data_invocation_if).

-include("ctr_data.hrl").
-define(TABLEVERSION, 0.1).

-export([
         add_invocation/1,
         get_invocation/2,
         delete_invocation/1,

         init/0
        ]).


init() ->
    create_table().


add_invocation(Invoc) ->
    NewId = ctr_utils:gen_global_id(),
    NewInvoc = Invoc#ctrd_invocation{id = NewId},
    StoreInvocation =
        fun() ->
                case mnesia:wread({ctrd_invocation, NewId}) of
                    [] ->
                        ok = mnesia:write(NewInvoc),
                        {ok, NewInvoc};
                    _ ->
                        {error, id_exists}
                end
        end,
    Result = mnesia:transaction(StoreInvocation),
    handle_invocation_store_result(Result, NewInvoc).


handle_invocation_store_result({atomic, {ok, Invoc}}, _) ->
    {ok, Invoc};
handle_invocation_store_result({atomic, {error, id_exists}}, Invoc) ->
    add_invocation(Invoc).


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
              Procedure, to_text(Options), to_text(Arguments),
              to_text(ArgumentsKw), to_text(Callees), Realm],
    {ok, Con} = ct_data_util:get_sqlite_connection(),
    esqlite3:q(Sql, Params, Con).


get_invocation(InvocationId, _Realm) ->
    FindInvocation =
        fun() ->
                case mnesia:read({ctrd_invocation, InvocationId}) of
                    [Invoc] -> {ok, Invoc};
                    _ -> {error, not_found}
                end
        end,
    Result = mnesia:transaction(FindInvocation),
    handle_invocation_find_result(Result).


handle_invocation_find_result({atomic, {ok, Invocation}}) ->
    {ok, Invocation};
handle_invocation_find_result(_) ->
    {error, not_found}.



delete_invocation(#ctrd_invocation{id=Id}) ->
    DeleteInvocation =
        fun() ->
                mnesia:delete({ctrd_invocation, Id})
        end,
    Result = mnesia:transaction(DeleteInvocation),
    handle_invocation_delete_result(Result).

handle_invocation_delete_result({atomic, ok}) ->
    ok;
handle_invocation_delete_result(Error) ->
    {error, Error}.


to_text(Any) ->
    io_lib:format("~p", [Any]).

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
