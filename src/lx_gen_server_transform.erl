%%%-------------------------------------------------------------------
%%% @author zyuyou yuyouchow@gmail.com
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jul 2016 下午3:34
%%%-------------------------------------------------------------------
-module(lx_gen_server_transform).
-author("zyuyou").

%% API
-export([parse_transform/2]).

-define(DEFAULT_CLASS, lx_gen_server).
-define(INHERIT_FUNS, [init, handle_call, handle_cast, handle_info, terminate, code_change]).

-ifndef(LX_TEST).
-define(LX_TEST, false).
parse_transform(Forms0, _Options) ->
    transform(Forms0).
-else.
parse_transform([{attribute, 1, file, _FilePath}, {attribute, _L, module, ModuleName} | _Tail] = Forms0, _Options) ->
    Forms2 = transform(Forms0),
    case ?LX_TEST of
        false ->
            Forms2;
        true ->
            file:write_file(io_lib:format("transforms/~w.txt", [ModuleName]), io_lib:format("~p", [Forms2]));
        Dir ->
            file:write_file(io_lib:format("~s/~w.txt", [Dir, ModuleName]), io_lib:format("~p", [Forms2]))
    end,
    Forms2.
-endif.

transform(Forms) ->
    case get_inherit_funs(Forms) of
        no_need ->
            Forms;
        {BaseModule, Handler, InheritFuns} ->
            {ExportAttrExpr, FunExprs} = gen_funs_expr(BaseModule, Handler, InheritFuns, {[], []}),
            Forms1 = add_attributes(Forms, [{attribute, 0, behaviour, gen_server}, ExportAttrExpr]),
            add_new_funcs(Forms1, FunExprs)
    end.

get_inherit_funs(Forms) ->
    get_inherit_funs(Forms, {?DEFAULT_CLASS, undefined, ?INHERIT_FUNS}).

get_inherit_funs(_Forms, {_BasedClass, _Handler, []}) ->
    no_need;
get_inherit_funs([], {_BasedClass, undefined, _Funs}) ->
    no_need;
get_inherit_funs([], {BasedClass, Handler, InheritFuns}) ->
    {BasedClass, Handler, InheritFuns};
get_inherit_funs([{attribute, _L, module, HandlerModule} | Tail], {_DefaultClass, undefined, InheritFuns}) ->
    get_inherit_funs(Tail, {_DefaultClass, HandlerModule, InheritFuns});
get_inherit_funs([{attribute, _L, lx_gen_server, BasedClassName} | Tail], {_DefaultClass, Handler, InheritFuns}) ->
    get_inherit_funs(Tail, {BasedClassName, Handler, InheritFuns});
get_inherit_funs([{function, _L, init, 1, _ClauseBody} | Tail], {BasedClass, Handler, InheritFuns}) ->
    get_inherit_funs(Tail, {BasedClass, Handler, InheritFuns -- [init]});
get_inherit_funs([{function, _L, handle_call, 3, _ClauseBody} | Tail], {BasedClass, Handler, InheritFuns}) ->
    get_inherit_funs(Tail, {BasedClass, Handler, InheritFuns -- [handle_call]});
get_inherit_funs([{function, _L, handle_cast, 2, _ClauseBody} | Tail], {BasedClass, Handler, InheritFuns}) ->
    get_inherit_funs(Tail, {BasedClass, Handler, InheritFuns -- [handle_cast]});
get_inherit_funs([{function, _L, handle_info, 2, _ClauseBody} | Tail], {BasedClass, Handler, InheritFuns}) ->
    get_inherit_funs(Tail, {BasedClass, Handler, InheritFuns -- [handle_info]});
get_inherit_funs([{function, _L, terminate, 2, _ClauseBody} | Tail], {BasedClass, Handler, InheritFuns}) ->
    get_inherit_funs(Tail, {BasedClass, Handler, InheritFuns -- [terminate]});
get_inherit_funs([{function, _L, code_change, 3, _ClauseBody} | Tail], {BasedClass, Handler, InheritFuns}) ->
    get_inherit_funs(Tail, {BasedClass, Handler, InheritFuns -- [code_change]});
get_inherit_funs([_H | Tail], {BasedClass, Handler, InheritFuns}) ->
    get_inherit_funs(Tail, {BasedClass, Handler, InheritFuns}).

%% gen inherit-funs exprs
gen_funs_expr(_BaseModule, _Handler, [], {ExportAttrs, FunExprs}) ->
    {{attribute, 0, export, ExportAttrs}, FunExprs};
gen_funs_expr(BaseModule, Handler, [FunName | TailFuns], {ExportAttr, FunExprs}) ->
    {ArgsCount, FunExpr} =
        case FunName of
            init ->
                {1,
                    {function, 0, init, 1,
                        [{clause, 0,
                            [{var, 0, 'Args'}],
                            [],
                            [{call, 0,
                                {remote, 0,
                                    {atom, 0, BaseModule}, {atom, 0, init}},
                                [{atom, 0, Handler},
                                    {var, 0, 'Args'}]}]}]}
                };
            handle_call ->
                {3,
                    {function, 0, handle_call, 3,
                        [{clause, 0,
                            [{var, 0, 'Msg'}, {var, 0, 'From'}, {var, 0, 'State'}],
                            [],
                            [{call, 0,
                                {remote, 0,
                                    {atom, 0, BaseModule},
                                    {atom, 0, handle_call}},
                                [{atom, 0, Handler},
                                    {var, 0, 'Msg'},
                                    {var, 0, 'From'},
                                    {var, 0, 'State'}]}]}]}
                };
            handle_cast ->
                {2,
                    {function, 0, handle_cast, 2,
                        [{clause, 0,
                            [{var, 0, 'Msg'}, {var, 0, 'State'}],
                            [],
                            [{call, 0,
                                {remote, 0,
                                    {atom, 0, BaseModule},
                                    {atom, 0, handle_cast}},
                                [{atom, 0, Handler},
                                    {var, 0, 'Msg'},
                                    {var, 0, 'State'}]}]}]}
                };
            handle_info ->
                {2,
                    {function, 0, handle_info, 2,
                        [{clause, 0,
                            [{var, 0, 'Msg'}, {var, 0, 'State'}],
                            [],
                            [{call, 0,
                                {remote, 0,
                                    {atom, 0, BaseModule},
                                    {atom, 0, handle_info}},
                                [{atom, 0, Handler},
                                    {var, 0, 'Msg'},
                                    {var, 0, 'State'}]}]}]}
                };
            terminate ->
                {2,
                    {function, 0, terminate, 2,
                        [{clause, 0,
                            [{var, 0, 'Reason'}, {var, 0, 'State'}],
                            [],
                            [{call, 0,
                                {remote, 0,
                                    {atom, 0, BaseModule},
                                    {atom, 0, terminate}},
                                [{atom, 0, Handler},
                                    {var, 0, 'Reason'},
                                    {var, 0, 'State'}]}]}]}
                };
            code_change ->
                {3,
                    {function, 0, code_change, 3,
                        [{clause, 0,
                            [{var, 0, 'OldVsn'}, {var, 0, 'State'}, {var, 0, 'Extra'}],
                            [],
                            [{call, 0,
                                {remote, 0,
                                    {atom, 0, BaseModule},
                                    {atom, 0, code_change}},
                                [{atom, 0, Handler},
                                    {var, 0, 'OldVsn'},
                                    {var, 0, 'State'},
                                    {var, 0, 'Extra'}]}]}]}
                }
        end,
    gen_funs_expr(BaseModule, Handler, TailFuns, {[{FunName, ArgsCount} | ExportAttr], [FunExpr | FunExprs]}).

add_attributes([{attribute, _, module, _} = F | Fs], Attrs) ->
    [F | Attrs ++ Fs];
add_attributes([F | Fs], Attrs) ->
    [F | add_attributes(Fs, Attrs)].

add_new_funcs([{eof, _} | _] = Fs, NewFs) ->
    NewFs ++ Fs;
add_new_funcs([F | Fs], Es) ->
    [F | add_new_funcs(Fs, Es)];
add_new_funcs([], NewFs) ->
    NewFs.