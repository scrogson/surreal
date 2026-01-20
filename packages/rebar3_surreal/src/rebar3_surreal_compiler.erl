%% @doc Rebar3 compiler provider for Surreal files.
%%
%% This module implements a rebar3 provider that compiles .surreal files
%% to BEAM bytecode during the compile phase.
-module(rebar3_surreal_compiler).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, compile).
-define(NAMESPACE, surreal).
-define(DEPS, [{default, app_discovery}]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {namespace, ?NAMESPACE},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 surreal compile"},
        {opts, []},
        {short_desc, "Compile Surreal source files"},
        {desc, "Compile .surreal files to BEAM bytecode"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:info("Compiling Surreal files...", []),

    Apps = case rebar_state:current_app(State) of
        undefined -> rebar_state:project_apps(State);
        App -> [App]
    end,

    lists:foreach(fun(App) -> compile_app(App, State) end, Apps),

    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("Surreal compilation error: ~p", [Reason]).

%% ===================================================================
%% Internal functions
%% ===================================================================

compile_app(App, _State) ->
    AppDir = rebar_app_info:dir(App),
    SrcDir = filename:join(AppDir, "src"),
    EbinDir = rebar_app_info:ebin_dir(App),

    %% Find all .surreal files
    SurrealFiles = filelib:wildcard(filename:join(SrcDir, "*.surreal")) ++
                   filelib:wildcard(filename:join(SrcDir, "**/*.surreal")),

    case SurrealFiles of
        [] ->
            rebar_api:debug("No .surreal files found in ~s", [SrcDir]),
            ok;
        _ ->
            rebar_api:info("Found ~B .surreal file(s)", [length(SurrealFiles)]),
            ok = filelib:ensure_dir(filename:join(EbinDir, "dummy")),
            lists:foreach(
                fun(File) -> compile_file(File, EbinDir) end,
                SurrealFiles
            )
    end.

compile_file(File, EbinDir) ->
    rebar_api:debug("Compiling ~s", [File]),
    case surreal_compiler:compile_file(File, [{outdir, EbinDir}]) of
        ok ->
            rebar_api:info("Compiled ~s", [filename:basename(File)]),
            ok;
        {ok, _Modules} ->
            rebar_api:info("Compiled ~s", [filename:basename(File)]),
            ok;
        {error, Reason} ->
            rebar_api:error("Failed to compile ~s: ~p", [File, Reason]),
            throw({compile_error, File, Reason})
    end.
