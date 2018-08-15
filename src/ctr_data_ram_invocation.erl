-module(ctr_data_ram_invocation).
-behaviour(ctr_data_invocation_if).

-include("ctr_data.hrl").

-export([
         store_invocation/1,
         get_invocation/2,
         delete_invocation/1,

         init/0
        ]).


init() ->
    create_table().


store_invocation(Invoc) ->
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
    store_invocation(Invoc).

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


create_table() ->
    mnesia:delete_table(ctrd_invocation),
    InvDef = [{attributes, record_info(fields, ctrd_invocation)},
              {ram_copies, [node()]},
              {index, [realm]}
             ],
    {atomic, ok} = mnesia:create_table(ctrd_invocation, InvDef),
    ok.
