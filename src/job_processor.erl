-module(job_processor).

-include("job_processor.hrl").

-export([sort/1]).

sort(Tasks) ->
    sort(Tasks, [], []).

%% 1. We start by adding into the sorted task list (Acc) all  
%% the tasks which have no dependency or all dependencies are already executed (Visited).
%% 2. If there was no task added in the above step that means there is an circular dependency 
%% or an invalid (undefined) dependency and the task list can not be sorted.
%% 3. Otherwise, the algorithm continue till there is no tasks left to be sorted. 
sort([], _Visited, Acc) -> 
    {ok, lists:reverse(Acc)};
sort(Tasks, Visited0, Acc) ->
    %% return a tuple of {[task_with_all_depencies_resolved], [task_with_at_least_one_unresolved_dependencis]}
    case lists:partition(fun(T) -> all_requires_are_visited(T, Visited0) end, Tasks) of 
        {[], _} ->
            {false};
        {Resolved, Unresolved} ->
            Visited = [T#task.name || T<-Resolved] ++ Visited0,
            sort(Unresolved, Visited, Resolved ++ Acc)
    end.

all_requires_are_visited(Task, Visited) ->
    lists:all(fun(Dep) -> lists:member(Dep, Visited) end, Task#task.requires).

