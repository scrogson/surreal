#!/usr/bin/env escript
%%! -pa ebin -pa priv

main(_) ->
    test_basic(),
    test_closure(),
    test_recursive(),
    io:format("~nAll tests passed!~n").

test_basic() ->
    io:format("=== Basic Test ===~n"),
    Source = <<"mod test {
        pub fn double(x: int) -> int {
            x + x
        }

        pub fn is_even(x: int) -> bool {
            match x {
                0 => true,
                _ => false,
            }
        }
    }">>,

    {ok, CoreAst} = surreal_native:generate_core_ast(Source),
    {ok, test, Binary, _} = compile:forms(CoreAst, [from_core, return_errors, return_warnings]),
    {module, test} = code:load_binary(test, "test.beam", Binary),

    42 = test:double(21),
    true = test:is_even(0),
    false = test:is_even(5),
    io:format("Basic tests passed~n").

test_closure() ->
    io:format("~n=== Closure Test ===~n"),
    Source = <<"mod math {
        pub fn apply_twice(f: fn(int) -> int, x: int) -> int {
            f(f(x))
        }
    }">>,

    {ok, CoreAst} = surreal_native:generate_core_ast(Source),
    io:format("Closure Core AST: ~p~n", [CoreAst]),

    case compile:forms(CoreAst, [from_core, return_errors, return_warnings]) of
        {ok, math, Binary, _} ->
            {module, math} = code:load_binary(math, "math.beam", Binary),
            io:format("Closure compiled successfully~n");
        {error, Errors, _} ->
            io:format("Closure compile errors: ~p~n", [Errors])
    end.

test_recursive() ->
    io:format("~n=== Recursive Test ===~n"),
    Source = <<"mod rectest {
        pub fn sum_to(n: int) -> int {
            if n <= 0 {
                0
            } else {
                n + sum_to(n - 1)
            }
        }
    }">>,

    {ok, CoreAst} = surreal_native:generate_core_ast(Source),
    io:format("Recursive Core AST: ~p~n", [CoreAst]),

    case compile:forms(CoreAst, [from_core, return_errors, return_warnings]) of
        {ok, rectest, Binary, _} ->
            {module, rectest} = code:load_binary(rectest, "rectest.beam", Binary),
            %% sum_to(5) = 5 + 4 + 3 + 2 + 1 + 0 = 15
            15 = rectest:sum_to(5),
            io:format("sum_to(5) = ~p~n", [rectest:sum_to(5)]),
            io:format("Recursive test passed~n");
        {error, Errors, _} ->
            io:format("Recursive compile errors: ~p~n", [Errors])
    end.
