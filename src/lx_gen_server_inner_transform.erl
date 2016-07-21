%%%-------------------------------------------------------------------
%%% @author zyuyou yuyouchow@gmail.com
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jul 2016 下午3:34
%%%-------------------------------------------------------------------
-module(lx_gen_server_inner_transform).
-author("zyuyou").

%% API
-export([parse_transform/2]).

-define(DEFAULT_CLASS, lx_gen_server_inner).
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

outdir(Module) ->
    CompileInfos = Module:module_info(compile),
    Options = proplists:get_value(options, CompileInfos, []),
    proplists:get_value(outdir, Options, "").

transform(Forms) ->
    case get_inherit_funs(Forms) of
        no_need ->
            Forms;
        {BaseModule, InheritFuns} ->
            FunExprs = collect_abstract_code(outdir(BaseModule), BaseModule, InheritFuns),
            ExportAttrExpr = get_export_attrs(InheritFuns),
            Forms1 = add_attributes(Forms, [{attribute, 0, behaviour, gen_server}, ExportAttrExpr]),
            add_new_funcs(Forms1, FunExprs)
    end.

%% 获取 -export 表达式
get_export_attrs(InheritFuns) ->
    ExportAttrs =
        lists:foldl(
            fun
                (init, Acc) ->
                    [{init, 1} | Acc];
                (handle_call, Acc) ->
                    [{handle_call, 3} | Acc];
                (handle_cast, Acc) ->
                    [{handle_cast, 2} | Acc];
                (handle_info, Acc) ->
                    [{handle_info, 2} | Acc];
                (terminate, Acc) ->
                    [{terminate, 2} | Acc];
                (code_change, Acc) ->
                    [{code_change, 3} | Acc]
            end, [], InheritFuns),
    {attribute, 0, export, ExportAttrs}.

%% 获取需要继承的函数
get_inherit_funs(Forms) ->
    get_inherit_funs(Forms, {?DEFAULT_CLASS, ?INHERIT_FUNS}).

get_inherit_funs(_Forms, {_BasedClass, []}) ->
    no_need;
get_inherit_funs([], {BasedClass, InheritFuns}) ->
    {BasedClass, InheritFuns};
get_inherit_funs([{attribute, _L, lx_gen_server_inner, BasedClassName} | Tail], {_DefaultClass, InheritFuns}) ->
    get_inherit_funs(Tail, {BasedClassName, InheritFuns});
get_inherit_funs([{function, _L, init, 1, _ClauseBody} | Tail], {BasedClass, InheritFuns}) ->
    get_inherit_funs(Tail, {BasedClass, InheritFuns -- [init]});
get_inherit_funs([{function, _L, handle_call, 3, _ClauseBody} | Tail], {BasedClass, InheritFuns}) ->
    get_inherit_funs(Tail, {BasedClass, InheritFuns -- [handle_call]});
get_inherit_funs([{function, _L, handle_cast, 2, _ClauseBody} | Tail], {BasedClass, InheritFuns}) ->
    get_inherit_funs(Tail, {BasedClass, InheritFuns -- [handle_cast]});
get_inherit_funs([{function, _L, handle_info, 2, _ClauseBody} | Tail], {BasedClass, InheritFuns}) ->
    get_inherit_funs(Tail, {BasedClass, InheritFuns -- [handle_info]});
get_inherit_funs([{function, _L, terminate, 2, _ClauseBody} | Tail], {BasedClass, InheritFuns}) ->
    get_inherit_funs(Tail, {BasedClass, InheritFuns -- [terminate]});
get_inherit_funs([{function, _L, code_change, 3, _ClauseBody} | Tail], {BasedClass, InheritFuns}) ->
    get_inherit_funs(Tail, {BasedClass, InheritFuns -- [code_change]});
get_inherit_funs([_H | Tail], {BasedClass, InheritFuns}) ->
    get_inherit_funs(Tail, {BasedClass, InheritFuns}).

collect_abstract_code(OutDir, BaseModule, InheritFuns) ->
    BaseModulePath = filename:join([OutDir, atom_to_list(BaseModule) ++ ".beam"]),
    {ok, {BaseModule, [{abstract_code, {_RawAbstractVersion, BaseModuleForms}}]}} = beam_lib:chunks(BaseModulePath, [abstract_code]),
    collect_fun_abstract_code(BaseModuleForms, InheritFuns).

collect_fun_abstract_code(Forms, InheritFuns) ->
    collect_fun_abstract_code(Forms, InheritFuns, []).

collect_fun_abstract_code([], _InheritFuns, FunAbstractCodes) ->
    FunAbstractCodes;
collect_fun_abstract_code([{function, _L, init, 1, _Clauses} = FunForms | Tail], InheritFuns, FunAbstractCodes) ->
    collect_fun_abstract_code1(FunForms, Tail, InheritFuns, FunAbstractCodes);
collect_fun_abstract_code([{function, _L, handle_call, 3, _Clauses} = FunForms | Tail], InheritFuns, FunAbstractCodes) ->
    collect_fun_abstract_code1(FunForms, Tail, InheritFuns, FunAbstractCodes);
collect_fun_abstract_code([{function, _L, handle_cast, 2, _Clauses} = FunForms | Tail], InheritFuns, FunAbstractCodes) ->
    collect_fun_abstract_code1(FunForms, Tail, InheritFuns, FunAbstractCodes);
collect_fun_abstract_code([{function, _L, handle_info, 2, _Clauses} = FunForms | Tail], InheritFuns, FunAbstractCodes) ->
    collect_fun_abstract_code1(FunForms, Tail, InheritFuns, FunAbstractCodes);
collect_fun_abstract_code([{function, _L, terminate, 2, _Clauses} = FunForms | Tail], InheritFuns, FunAbstractCodes) ->
    collect_fun_abstract_code1(FunForms, Tail, InheritFuns, FunAbstractCodes);
collect_fun_abstract_code([{function, _L, code_change, 3, _Clauses} = FunForms | Tail], InheritFuns, FunAbstractCodes) ->
    collect_fun_abstract_code1(FunForms, Tail, InheritFuns, FunAbstractCodes);
collect_fun_abstract_code([_H | Tail], InheritFuns, FunAbstractCodes) ->
    collect_fun_abstract_code(Tail, InheritFuns, FunAbstractCodes).

collect_fun_abstract_code1({function, _L, FunName, _Arity, _Clauses} = FunForms, TailForms, InheritFuns, FunAbstractCodes) ->
    case lists:member(FunName, InheritFuns) of
        true ->
            collect_fun_abstract_code(TailForms, InheritFuns, [replace_line_num(FunForms) | FunAbstractCodes]);
        false ->
            collect_fun_abstract_code(TailForms, InheritFuns, FunAbstractCodes)
    end.

replace_line_num({function, _L, FunName, FunArgsCount, _Clauses}) ->
    {function, 0, FunName, FunArgsCount, replace_clause_line_num(_Clauses)};
replace_line_num([{clause, _L, ArgsForm, GuardForm, FunBodyForm}]) ->
    [{clause, 0, replace_line_num(ArgsForm), replace_line_num(GuardForm), replace_line_num(FunBodyForm)}].

%% 替换 clause 表达式中行号
replace_clause_line_num(Forms) ->
    replace_clause_line_num(Forms, []).

replace_clause_line_num([], Result) ->
    lists:reverse(Result);
replace_clause_line_num([{clause, _L, ArgsForm, GuardForm, ClauseBody} | Tail], Result) ->
    replace_clause_line_num(Tail, [{clause, 0, replace_args_line_num(ArgsForm), replace_guards_line_num(GuardForm), replace_body_line_num(ClauseBody)} | Result]).

%% 替换 参数 表达式中行号
replace_args_line_num(Forms) ->
    replace_args_line_num(Forms, []).

replace_args_line_num([], Result) ->
    lists:reverse(Result);
replace_args_line_num([H | Tail], Result) ->
    replace_args_line_num(Tail, [replace_arg_line_num(H) | Result]).

replace_arg_line_num({cons, _L, Value, TailValue}) ->
    {cons, 0, replace_arg_line_num(Value), replace_arg_line_num(TailValue)};
replace_arg_line_num({nil, _L}) ->
    {nil, 0};
replace_arg_line_num({call, _L, FunName, Values}) ->
    {call, 0, replace_fun_name_line_num(FunName), replace_args_line_num(Values)};
replace_arg_line_num({tuple, _L, Args}) ->
    {tuple, 0, replace_args_line_num(Args)};
replace_arg_line_num({op, _L, Type, LeftValue, RightValue}) ->
    {op, 0, Type, replace_arg_line_num(LeftValue), replace_arg_line_num(RightValue)};
replace_arg_line_num({'block', _L, Body}) ->
    {'block', 0, replace_body_line_num(Body)};
replace_arg_line_num({match, _L, LeftValue, RightValue}) ->
    {match, 0, replace_arg_line_num(LeftValue), replace_arg_line_num(RightValue)};
replace_arg_line_num({Type, _L, Value}) ->
    {Type, 0, Value}.

%% 替换 保护元 表达式中行号
replace_guards_line_num(Forms) ->
    replace_guards_line_num(Forms, []).

replace_guards_line_num([], Result) ->
    lists:reverse(Result);
replace_guards_line_num([Guard | Tail], Result) ->
    replace_guards_line_num(Tail, [replace_guard_line_num(Guard) | Result]).

replace_guard_line_num(Guard) ->
    replace_guard_line_num(Guard, []).

replace_guard_line_num([], Result) ->
    lists:reverse(Result);
replace_guard_line_num([{call, _L, FunName, Values} | Tail], Result) ->
    replace_guard_line_num(Tail, [{call, 0, replace_fun_name_line_num(FunName), replace_args_line_num(Values)} |Result]);
replace_guard_line_num([{op, _L, Type, LeftValue, RightValue} | Tail], Result) ->
    replace_guard_line_num(Tail, [{op, 0, Type, replace_arg_line_num(LeftValue), replace_arg_line_num(RightValue)} |Result]).

replace_fun_name_line_num(Forms) ->
    replace_arg_line_num(Forms).

%% 替换 函数内容表达式 表达式中行号
replace_body_line_num(Forms) ->
    replace_body_line_num(Forms, []).

replace_body_line_num([], Result) ->
    lists:reverse(Result);
replace_body_line_num([{'try', _L, Exprs, Guards, Catchs, Afters}|Tail], Result) ->
    replace_body_line_num(Tail, [{'try', 0, replace_body_line_num(Exprs), replace_clause_line_num(Guards), replace_clause_line_num(Catchs), replace_body_line_num(Afters)} | Result]);
replace_body_line_num([{'case', _L, Value, Clause}|Tail], Result) ->
    replace_body_line_num(Tail, [{'case', 0, replace_arg_line_num(Value), replace_clause_line_num(Clause)} | Result]);
replace_body_line_num([{'if', _L, Clause}|Tail], Result) ->
    replace_body_line_num(Tail, [{'case', 0, replace_clause_line_num(Clause)} | Result]);
replace_body_line_num([{'block', _L, Body}|Tail], Result) ->
    replace_body_line_num(Tail, [{'case', 0, replace_body_line_num(Body)} | Result]);
replace_body_line_num([{'receive', _L, Clause, AfterGuard, AfterExpr}|Tail], Result) ->
    replace_body_line_num(Tail, [{'receive', 0, replace_clause_line_num(Clause), replace_arg_line_num(AfterGuard), replace_body_line_num(AfterExpr)} | Result]);
replace_body_line_num([{call, _L, FunName, Value}|Tail], Result) ->
    replace_body_line_num(Tail, [{call, 0, replace_fun_name_line_num(FunName), replace_args_line_num(Value)} | Result]);
replace_body_line_num([{Type, _L, Value}|Tail], Result) ->
    replace_body_line_num(Tail, [{Type, 0, Value} | Result]).

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