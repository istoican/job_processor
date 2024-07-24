-module(job_processor_tests).

-include_lib("eunit/include/eunit.hrl").

-include("job_processor.hrl").
-import(job_processor, [sort/1, sort2/1]).

sort_test_() ->
    {
        setup,
        fun setup/0,
        fun cleanup/1,
        [
            fun test_sort_empty_list/0,
            fun test_sort_no_deps/0,
            fun test_sort_with_deps/0,
            fun test_sort_with_circular_deps/0,
            fun test_sort_with_unknown_deps/0
        ]
    }.

setup() ->
    ok.

cleanup(_State) ->
    ok.

test_sort_empty_list() ->
    ?assertEqual({ok, []}, sort([])).

test_sort_no_deps() ->
    T1 = #task{name = "task-1", requires = []},
    T2 = #task{name = "task-2", requires = []},
    T3 = #task{name = "task-3", requires = []},

    ?assertEqual({ok, [T3, T2, T1]}, sort([T1, T2, T3])).

test_sort_with_deps() ->
    T1 = #task{name = "task-1", requires = []},
    T2 = #task{name = "task-2", requires = ["task-3"]},
    T3 = #task{name = "task-3", requires = ["task-1"]},
    T4 = #task{name = "task-4", requires = ["task-2", "task-3"]},
    
    ?assertEqual({ok, [T1, T3, T2, T4]}, sort([T1, T2, T3, T4])).

test_sort_with_circular_deps() ->
    Tasks = [
        #task{name = "task-1", requires = ["task-2"]},
        #task{name = "task-2", requires = ["task-3"]},
        #task{name = "task-3", requires = ["task-1"]}
    ],

    ?assertEqual({false}, sort(Tasks)).

test_sort_with_unknown_deps() ->
    Tasks = [
        #task{name = "task-1"},
        #task{name = "task-2", requires = ["task-3"]}
    ],

    ?assertEqual({false}, sort(Tasks)).
