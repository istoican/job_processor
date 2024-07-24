## A Few Words About the Algorithm Used

We observe that the problem can be illustrated using a graph where each node is connected to its direct dependencies. The problem can be approached in two ways:

### 1. The First Solution Involves:

   * Iterating through the list of tasks and moving those without dependencies to a new list (the ordered tasks list).
   * If all tasks have dependencies, it means the tasks cannot be ordered (we are either dealing with cyclic dependencies or dependencies that are not in the task list).
   * Repeating the above steps for the remaining tasks in the list (only tasks whose dependencies have already been resolved will be moved).
   * Continuing until there are no more tasks to move.

   Analogous to traversing a tree, this algorithm is equivalent to first traversing all leaf nodes, then moving level by level towards the root nodes. 

   The advantage of this algorithm is that it is easy to understand (in a procedural language, recursion is not even needed, a simple loop suffices) and easy to implement.

### 2. The Second Solution Involves:

   1. Starting with the first node and recursively resolving its dependencies.
   2. Repeating the step for all tasks in the list. 
     
   Again, analogous to traversing a tree, this algorithm is equivalent to a post-order traversal. The 'digraph_utils' library, part of the standard library in Erlang, implement this algorithm.
   
   A potential advantage of this algorithm is that it tries to preserve the initial order of tasks as much as possible (depending on the specific implementation).

Given that the problem does not require preserving as much as possible the initial order of tasks, the succinct way it can be implemented in Erlang, and the fact that the second solution is already implemented in the standard library, **I have chosen to implement the first solution**.


## Relevant Files

* `src/job_processor.erl`: Includes the actual implementation of the algorithm. 
* `src/job_processor_htp_handler.erl`: Contains the handler that processes the HTTP request. The same endpoint [POST] "http://127.0.0.1:8080/", is used for both JSON and script responses, with the difference that for a script response, a query parameter "format=script" must be sent (http://127.0.0.1:8080/?format=script). 
* `test/job_processor_tests.erl`: eUnit test file used for testing the implementation.


## Starting the HTTP Server

```shell
rebar3 compile
rebar3 shell
```


## Running eUnit Tests

```shell
rebar3 eunit
```


## Testing the HTTP Endpoint - `json` response

```shell
curl -d @test-data.json http://127.0.0.1:8080
```

## Testing the HTTP Endpoint - `script` response

```shell
curl -d @test-data.json http://127.0.0.1:8080?format=script
```