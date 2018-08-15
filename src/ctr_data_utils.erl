-module(ctr_data_utils).

-define(DBDIR, "db").

-export([
         stop_mnesia/0,
         start_mnesia/0,
         create_mnesia_schema_if_needed/0
        ]).

%% init() ->
%%     Dir = application:get_env(ct_router, mnesia_dir, ?DBDIR),
%%     DirExists = filelib:is_dir(Dir),
%%     ok = stop_mnesia(),
%%     ok = create_schema_if_needed(DirExists),
%%     ok = start_mnesia(),
%%     ok = init_tables(),
%%     ok.

%% init_tables() ->
%%     cta_session:init(),
%%     cta_realm:init(),
%%     ctr_gen_data:initialize(),
%%     ok.


stop_mnesia() ->
    application:stop(mnesia),
    MnesiaDir = mnesia_dir(),
    application:set_env(mnesia, dir, MnesiaDir),
    ok.

start_mnesia() ->
    {ok, _} = application:ensure_all_started(mnesia),
    ok.

create_mnesia_schema_if_needed() ->
    Dir = mnesia_dir(),
    DirExists = filelib:is_dir(Dir),
    create_mnesia_schema_if_needed(DirExists).


create_mnesia_schema_if_needed(true) ->
    ok;
create_mnesia_schema_if_needed(false) ->
    ok = stop_mnesia(),
    ok = mnesia:create_schema([node()]),
    ok = start_mnesia(),
    ok.

mnesia_dir() ->
    application:get_env(ctr_data, mnesia_dir, ?DBDIR).
