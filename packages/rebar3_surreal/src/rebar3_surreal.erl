%% @doc Rebar3 plugin for compiling Surreal source files.
%%
%% Add to your rebar.config:
%% ```
%% {plugins, [rebar3_surreal]}.
%% {provider_hooks, [
%%     {pre, [{compile, {surreal, compile}}]}
%% ]}.
%% '''
-module(rebar3_surreal).

-export([init/1]).

%% @doc Initialize the plugin and register the compiler provider.
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_surreal_compiler:init(State),
    {ok, State1}.
