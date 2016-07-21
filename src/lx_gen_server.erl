-module(lx_gen_server).

%% API exports
-export([
    init/2,
    handle_call/4,
    handle_cast/3,
    handle_info/3,
    terminate/3,
    code_change/4
]).

%%====================================================================
%% behaviour callback
%%====================================================================
-callback init(HandlerModule :: atom(), Args :: term()) ->
    {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.

-callback handle_call(HandlerModule :: atom(), Request :: term(), From :: {pid(), Tag :: term()},
    State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} |
    {reply, Reply :: term(), NewState :: term(), timeout() | hibernate} |
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.

-callback handle_cast(HandlerModule :: atom(), Request :: term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.

-callback handle_info(HandlerModule :: atom(), Info :: timeout | term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.

-callback terminate(HandlerModule :: atom(), Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: term()) ->
    term().

-callback code_change(HandlerModule :: atom(), OldVsn :: (term() | {down, term()}), State :: term(),
    Extra :: term()) ->
    {ok, NewState :: term()} | {error, Reason :: term()}.


%%====================================================================
%% API functions
%%====================================================================
init(Module, Args) ->
    try
        Module:do_init(Args)
    catch
        _:Reason ->
            {stop, Reason}
    end.

handle_call(Module, Request, From, State) ->
    try
        Module:do_call(Request, From, State)
    catch
        _:_Reason ->
            {reply, ok, State}
    end.

handle_cast(Module, Msg, State) ->
    try
        Module:do_cast(Msg, State)
    catch
        _:_Reason ->
            {noreply, State}
    end.

handle_info(Module, Info, State) ->
    try
        Module:do_info(Info, State)
    catch
        _:_Reason ->
            {noreply, State}
    end.

terminate(Module, Reason, State) ->
    try
        Module:do_terminate(Reason, State)
    catch
        _:_Reason ->
            ok
    end.

code_change(_Module, _OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================
