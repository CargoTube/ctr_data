-module(ctr_data_ram_publication).

-behaviour(ctr_data_publication_if).
-include("ctr_data.hrl").

-export([
         store_publication/1,
         init/0,

         dump_all/0
         ]).

init() ->
    create_table().


store_publication(Pub) ->
    #ctr_publication{
       realm = Realm,
       topic = Topic
      } = Pub,
    NewPubId = ctr_utils:gen_global_id(),
    NewPub = Pub#ctr_publication{id = NewPubId},
    MatchHead = #ctr_subscription{uri=Topic, realm=Realm, _='_'},
    Guard = [],
    GiveObject = ['$_'],
    MatSpec = [{MatchHead, Guard, GiveObject}],


    UpdateOrWriteNew =
        fun([#ctr_subscription{id = SubId, subscribers = Subs}]) ->
                UpdatedPub = NewPub#ctr_publication{sub_id=SubId, subs = Subs},
                ok = mnesia:write(UpdatedPub),
                {ok, UpdatedPub};
           ([]) ->
                ok = mnesia:write(NewPub),
                {ok, NewPub}
        end,

    LookupAndStore =
        fun() ->
                case mnesia:wread({ctr_publication, NewPubId}) of
                    [] ->
                        Found = mnesia:select(ctr_subscription, MatSpec, write),
                        UpdateOrWriteNew(Found);
                    _ ->
                        {error, pub_id_exists}
                end
        end,
    Result = mnesia:transaction(LookupAndStore),
    handle_publication_store_result(Result, Pub).


handle_publication_store_result({atomic, {ok, Publication}}, _Pub0) ->
    {ok, Publication};
handle_publication_store_result({atomic, {error, pub_id_exists}}, Pub0) ->
    store_publication(Pub0).



dump_all() ->
    Print = fun(Entry, _) ->
                    lager:debug("~p", [Entry]),
                    ok
            end,
    Transaction = fun() ->
                          lager:debug("*** publications ***"),
                          mnesia:foldl(Print, ok, ctr_publication),
                          lager:debug("*** end ***"),
                          ok
                  end,
    mnesia:transaction(Transaction).



create_table() ->
    mnesia:delete_table(ctr_publication),
    PubDef = [{attributes, record_info(fields, ctr_publication)},
              {ram_copies, [node()]},
              {index, [realm, topic]}
             ],
    {atomic, ok} = mnesia:create_table(ctr_publication, PubDef),
    ok.
