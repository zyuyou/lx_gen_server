%%%-------------------------------------------------------------------
%%% @author zyuyou
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jul 2016 下午7:07
%%%-------------------------------------------------------------------
-module(lx_mod_inner_test).
-author("zyuyou").

-include("lx_gen_server_inner_transform.hrl").

%%-behavior(gen_server).

%% API exports
%%-export([
%%    init/1,
%%    handle_call/3,
%%    handle_cast/2,
%%    handle_info/2,
%%    terminate/2,
%%    code_change/3
%%]).

-export([init/1]).

init(Args) ->
    try
        do_init(Args)
    catch
        _:Reason ->
            {stop, Reason}
    end.
%%
%%handle_call(Request, From, State) ->
%%    try
%%        do_call(Request, From, State)
%%    catch
%%        _:_Reason ->
%%            {reply, ok, State}
%%    end.
%%
%%handle_cast(Msg, State) ->
%%    try
%%        do_cast(Msg, State)
%%    catch
%%        _:_Reason ->
%%            {noreply, State}
%%    end.
%%
%%handle_info(Info, State) ->
%%    try
%%        do_info(Info, State)
%%    catch
%%        _:_Reason ->
%%            {noreply, State}
%%    end.
%%
%%terminate(Reason, State) ->
%%    try
%%        do_terminate(Reason, State)
%%    catch
%%        _:_Reason ->
%%            ok
%%    end.
%%
%%code_change(_OldVsn, State, _Extra) ->
%%    {ok, State}.

do_init(_Args) ->
    _Args.

do_call(_Request, _From, _State) ->
    _Request + 1.

do_cast(_Request, _State) ->
    ok.

do_info(_Request, _State) ->
    ok.

do_terminate(_Reason, _State) ->
    ok.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

do_init_test_() ->
    ?_assertEqual(do_init(1), ?MODULE:init(1)).

do_call_test_() ->
    ?_assertEqual(do_call(1,2,3), ?MODULE:handle_call(1,2,3)).

do_module_name_test_() ->
    ?_assertEqual(?MODULE, '$module'()).

-endif.