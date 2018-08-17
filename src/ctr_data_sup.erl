-module(ctr_data_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, noparams).

init(noparams) ->
    Procs = [
             sqlite_clean_worker()
            ],
    Flags = #{},
    {ok, {Flags, Procs}}.



sqlite_clean_worker() ->
    #{ id => plugin,
       start => {ctr_data_sqlite_clean, start_link, []}
     }.
