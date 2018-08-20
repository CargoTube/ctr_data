-module(ctr_data_sqlite_clean).

-behaviour(gen_server).


%%
-export([start_link/0]).
-export([start/0]).
-export([stop/0]).


%% gen_statem.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/3]).
-export([code_change/4]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_param, []).

start() ->
    gen_server:call(?MODULE, start).

stop() ->
    gen_server:call(?MODULE, stop).

-record(state, {
          enabled = false,
          max_age = "14 days",
          interval = 300
         }).

init(no_param) ->
    {ok, #state{}}.

handle_call(start, _From, #state{max_age=MaxAge0, interval=Intv} = State) ->
    MaxAge = application:get_env(ctr_data, sqlite_clean_max_age, MaxAge0),
    Interval = application:get_env(ctr_data, sqlite_clean_interval, Intv)*1000,
    {reply, ok, State#state{enabled = true, max_age= MaxAge,
                            interval=Interval}, 15000};
handle_call(stop, _From, State) ->
    {reply, ok, State#state{enabled = false}}.

handle_cast(Msg, State) ->
    lager:debug("unexpected message: ~p", [Msg]),
    {noreply, State, 15000}.

handle_info(timeout, #state{enabled = true, interval=Interval} = State) ->
    ok = clean_sqlite_db(State),
    {noreply, State, Interval}.


clean_sqlite_db(#state{max_age=MaxAge}) ->
    lager:debug("cleaning sqlite tables"),
    ctr_data_sqlite_invocation:clean_table(MaxAge),
    ctr_data_sqlite_publication:clean_table(MaxAge),
    ok.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.
