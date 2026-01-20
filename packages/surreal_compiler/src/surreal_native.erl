-module(surreal_native).

-export([parse/1, generate_core_ast/1, format_result/1]).

-on_load(init/0).

%% NIF loading
init() ->
    SoName = find_nif_lib(),
    erlang:load_nif(SoName, 0).

find_nif_lib() ->
    %% Try priv dir first (installed as app), then development paths
    case code:priv_dir(surreal_compiler) of
        {error, _} ->
            %% Not installed as app, try development paths
            find_dev_path();
        Dir ->
            filename:join(Dir, "libsurreal_native")
    end.

find_dev_path() ->
    %% Development paths relative to package root
    Paths = [
        "priv/libsurreal_native",
        "_build/default/lib/surreal_compiler/priv/libsurreal_native",
        "native/surreal_native/target/release/libsurreal_native",
        "native/surreal_native/target/debug/libsurreal_native"
    ],
    find_first_existing(Paths).

find_first_existing([Path | Rest]) ->
    case filelib:is_file(Path ++ ".so") orelse filelib:is_file(Path ++ ".dylib") of
        true -> Path;
        false -> find_first_existing(Rest)
    end;
find_first_existing([]) ->
    %% Last resort - return a path and let load_nif fail with a clear error
    "libsurreal_native".

%% Parse Surreal source code and return module information.
%% Returns: {ok, [{ModuleName, [{FuncName, Arity}, ...]}]} | {error, Reason}
-spec parse(binary() | string()) -> {ok, term()} | {error, term()}.
parse(_Source) ->
    erlang:nif_error(nif_not_loaded).

%% Generate Core Erlang AST from Surreal source code.
%% Returns: {ok, CoreErlangAST} | {error, Reason}
-spec generate_core_ast(binary() | string()) -> {ok, term()} | {error, term()}.
generate_core_ast(_Source) ->
    erlang:nif_error(nif_not_loaded).

%% Format an Erlang term as Surreal syntax.
%% Converts Erlang runtime values to Surreal-style string representation.
%% Useful for REPL output formatting.
%%
%% Examples:
%%   ok -> ":ok"
%%   {ok, 42} -> "Ok(42)"
%%   {error, reason} -> "Err(:reason)"
%%   [1, 2, 3] -> "[1, 2, 3]"
%%   #{a => 1} -> "{a: 1}"
%%   <<"hello">> -> "\"hello\""
%%   "hello" -> "'hello'" (charlist)
-spec format_result(term()) -> string().
format_result(_Term) ->
    erlang:nif_error(nif_not_loaded).
