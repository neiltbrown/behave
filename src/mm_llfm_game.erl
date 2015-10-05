-module(mm_llfm_game).

-behaviour(mm_game).

-export([
         handle_unconfirmed/3,
         handle_confirmed/3,
         handle_cancellation/3
        ]).

handle_unconfirmed(Key, Id, GameState) ->
    GameState#{key => Key, id => Id, status => unconfirmed}.

handle_confirmed(Key, Id, GameState) ->
    GameState#{key => Key, id => Id, status => confirmed}.

handle_cancellation(Key, Id, GameState) ->
    GameState#{key => Key, id => Id, status => cancelled}.
