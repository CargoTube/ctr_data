-module(ctr_data_sqlite_clean).

-behaviour(gen_statem).


%%
-export([start_link/0]).
-export([start/0]).
-export([stop/0]).


%% gen_statem.
-export([init/1]).
-export([callback_mode/0]).
-export([handle_event/4]).
-export([terminate/3]).
-export([code_change/4]).


start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, no_param, []).

start() ->
    gen_statem:cast(?MODULE, start).

stop() ->
    gen_statem:cast(?MODULE, stop).


-record(data, {
          max_age = "31 days",
          interval = 300
         }).

%% use the handle event function
callback_mode() -> handle_event_function.

init(no_param) ->
    {ok, waiting, #data{}}.


handle_event(cast, start, waiting, _Data) ->
    MaxAge = application:get_env(ctr_data, sqlite_clean_max_age, "31 days"),
    Interval = application:get_env(ctr_data, sqlite_clean_interval, 15)*1000,
    {next_state, running, #data{max_age=MaxAge, interval=Interval},
     [{timeout, Interval, clean}] };
handle_event(cast, start, State, #data{interval=Interval} = Data) ->
    {next_state, State, Data, [ {timeout, Interval, clean}] };
handle_event(cast, stop, _State, Data) ->
    {next_state, waiting, Data};
handle_event(timeout, clean, running, #data{interval = Interval} = Data) ->
    ok = clean_sqlite_db(Data),
    {next_state, running, Data, [{timeout, Interval, clean}] };
handle_event(timeout, clean, State, Data) ->
    {next_state, State, Data};
handle_event(Event, Content, State, Data) ->
    lager:debug("[~p] ignore event [~p] ~p ~p",[self(), State, Event, Content]),
    {next_state, State, Data}.


clean_sqlite_db(#data{max_age=MaxAge}) ->
    lager:debug("sqlite cleaning"),
    ctr_data_sqlite_invocation:clean_table(MaxAge),
    ctr_data_sqlite_publication:clean_table(MaxAge),
    ok.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.
