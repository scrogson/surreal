-module(surreal_user).

%% This module is used with `erl -user surreal_user` to replace the
%% default Erlang shell with the Surreal REPL while keeping readline support.
%%
%% Based on how Elixir's IEx.CLI works.

-export([start/0]).

%% Called by Erlang when started with -user surreal_user
start() ->
    %% Start user_drv with our shell as the initial shell
    %% The MFA tuple {surreal_shell, start, [init]} follows the same
    %% pattern as the default {shell, start, [init]}
    %% Provide a no-op expand function to prevent tab from hanging
    NoExpand = fun(_) -> {no, [], []} end,
    user_drv:start(#{
        initial_shell => {surreal_shell, start, [init]},
        expand_fun => NoExpand
    }).
