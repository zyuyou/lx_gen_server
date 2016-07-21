%%%-------------------------------------------------------------------
%%% @author zyuyou
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jul 2016 下午7:07
%%%-------------------------------------------------------------------
-module(lx_gen_server_inner).
-author("zyuyou").

-behavior(gen_server).

%% API exports
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-callback init(Args :: term()) ->
    {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
-callback handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} |
    {reply, Reply :: term(), NewState :: term(), timeout() | hibernate} |
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.
-callback handle_cast(Request :: term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.
-callback handle_info(Info :: timeout | term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.
-callback terminate(Reason :: (normal | shutdown | {shutdown, term()} |
term()),
    State :: term()) ->
    term().
-callback code_change(OldVsn :: (term() | {down, term()}), State :: term(),
    Extra :: term()) ->
    {ok, NewState :: term()} | {error, Reason :: term()}.

init(Args) ->
    try
        do_init(Args)
    catch
        _:Reason ->
            {stop, Reason}
    end.

handle_call(Request, From, State) ->
    try
        do_call(Request, From, State)
    catch
        _:_Reason ->
            {reply, ok, State}
    end.

handle_cast(Msg, State) ->
    try
        do_cast(Msg, State)
    catch
        _:_Reason ->
            {noreply, State}
    end.

handle_info(Info, State) ->
    try
        do_info(Info, State)
    catch
        _:_Reason ->
            {noreply, State}
    end.

terminate(Reason, State) ->
    try
        do_terminate(Reason, State)
    catch
        _:_Reason ->
            ok
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


do_init(_Args) ->
    ok.

do_call(_Request, _From, _State) ->
    ok.

do_cast(_Request, _State) ->
    ok.

do_info(_Request, _State) ->
    ok.

do_terminate(_Reason, _State) ->
    ok.