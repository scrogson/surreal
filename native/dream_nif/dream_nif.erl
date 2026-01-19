-module(dream_nif).
-export([parse/1, generate_core_ast/1]).
-on_load(init/0).

%% NIF loading
init() ->
    SoName = find_nif_lib(),
    erlang:load_nif(SoName, 0).

find_nif_lib() ->
    %% Try various paths to find the NIF library
    Paths = [
        %% From priv dir if running as app
        priv_path(),
        %% From project root
        "native/dream_nif/target/release/libdream_nif",
        "native/dream_nif/target/debug/libdream_nif",
        %% Absolute path fallback
        filename:join([code:lib_dir(), "..", "..", "native", "dream_nif", "target", "release", "libdream_nif"]),
        filename:join([code:lib_dir(), "..", "..", "native", "dream_nif", "target", "debug", "libdream_nif"])
    ],
    find_first_existing(Paths).

priv_path() ->
    case code:priv_dir(dream_nif) of
        {error, _} -> undefined;
        Dir -> filename:join(Dir, "libdream_nif")
    end.

find_first_existing([undefined | Rest]) ->
    find_first_existing(Rest);
find_first_existing([Path | Rest]) ->
    %% Check for .so or .dylib
    case filelib:is_file(Path ++ ".so") orelse filelib:is_file(Path ++ ".dylib") of
        true -> Path;
        false -> find_first_existing(Rest)
    end;
find_first_existing([]) ->
    %% Last resort - return a path and let load_nif fail with a clear error
    "libdream_nif".

%% Parse Dream source code and return module information.
%% Returns: {ok, [{ModuleName, [{FuncName, Arity}, ...]}]} | {error, Reason}
-spec parse(binary() | string()) -> {ok, term()} | {error, term()}.
parse(_Source) ->
    erlang:nif_error(nif_not_loaded).

%% Generate Core Erlang AST from Dream source code.
%% Returns: {ok, CoreErlangAST} | {error, Reason}
-spec generate_core_ast(binary() | string()) -> {ok, term()} | {error, term()}.
generate_core_ast(_Source) ->
    erlang:nif_error(nif_not_loaded).
