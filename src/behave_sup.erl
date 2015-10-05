-module(behave_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [{
                   mm_game,
                   {mm_game, start_link, [mm_llfm_game]},
                   permanent,
                   5000,
                   worker,
                   [mm_game]
                 }],
	{ok, {{one_for_one, 1, 5}, Procs}}.
