-module(ctr_data_ram_invocation).
-behaviour(ctr_data_invocation_if).

-include("ctr_data.hrl").

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

invocation_add_result(Result, InvocationId, _Realm) ->
    FindInvocation =
        fun() ->
                case mnesia:wread({ctrd_invocation, InvocationId}) of
                    [#ctrd_invocation{results = Results } = Invoc] ->
                        NewResults = Results ++ [Result],
                        NewInvoc = Invoc#ctrd_invocation{results = NewResults},
                        ok = mnesia:write(NewInvoc),
                        ok;
                    _ -> {error, not_found}
                end
        end,
    AddResult = mnesia:transaction(FindInvocation),
    handle_add_result_result(AddResult).


handle_add_result_result({atomic, ok}) ->
    ok;
handle_add_result_result({atomic, {error, Reason}}) ->
    {error, Reason}.

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



remove_invocation(Id, _Realm) ->
    DeleteInvocation =
        fun() ->
                mnesia:delete({ctrd_invocation, Id})
        end,
    Result = mnesia:transaction(DeleteInvocation),
    handle_invocation_remove_result(Result).

handle_invocation_remove_result({atomic, ok}) ->
    ok;
handle_invocation_remove_result(Error) ->
    {error, Error}.


create_table() ->
    ct_data_util:create_mnesia_schema_if_needed(),
    mnesia:delete_table(ctrd_invocation),
    InvDef = [{attributes, record_info(fields, ctrd_invocation)},
              {ram_copies, [node()]},
              {index, [realm]}
             ],
    {atomic, ok} = mnesia:create_table(ctrd_invocation, InvDef),
    ok.
