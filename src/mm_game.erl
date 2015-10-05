-module(mm_game).

-behaviour(gen_server).

%% API
-export([
         start_link/1,
         msg/1,
         state/0
        ]).

%% gen_server callbacks
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-define(SERVER, ?MODULE).

-callback handle_unconfirmed(Key :: atom(), Id :: integer(), GameState :: term()) -> {GameMod :: atom(), GameState :: term()}.
-callback handle_confirmed(Key :: atom(), Id :: integer(), GameState :: term()) -> {GameMod :: atom(), GameState :: term()}.
-callback handle_cancellation(Key :: atom(), Id :: integer(), GameState :: term()) -> {GameMod :: atom(), GameState :: term()}.

msg(Msg) ->
    gen_server:cast(?SERVER, Msg).

state() ->
    gen_server:call(?SERVER, state).

start_link(GameMod) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [GameMod], []).

init([GameMod]) ->
    {ok, {GameMod, #{}}}.

handle_call(state, _, State) ->
    {reply, State, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(#{
               confirmation := Confirmation,
               key := Key,
               id := Id},
            {GameMod, GameState}
           ) ->
    GameState2 = handle_message(Confirmation, Key, Id, GameMod, GameState),
    {noreply, {GameMod, GameState2}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_message(0, Key, Id, GameMod, GameState) ->
    GameMod:handle_unconfirmed(Key, Id, GameState);
handle_message(1, Key, Id, GameMod, GameState) ->
    GameMod:handle_confirmed(Key, Id, GameState);
handle_message(-1, Key, Id, GameMod, GameState) ->
    GameMod:handle_cancellation(Key, Id, GameState).
