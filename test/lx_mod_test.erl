%%%-------------------------------------------------------------------
%%% @author zyuyou
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jul 2016 下午4:04
%%%-------------------------------------------------------------------
-module(lx_mod_test).
-author("zyuyou").

-include("lx_gen_server_transform.hrl").

-export([do_init/1]).

do_init(A) ->
    A + 1.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

do_init_test_() ->
    ?_assertEqual(?MODULE:do_init(1), ?MODULE:init(1)).

-endif.