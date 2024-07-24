%%%-------------------------------------------------------------------
%% @doc job_processor public API
%% @end
%%%-------------------------------------------------------------------

-module(job_processor_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/", job_processor_http_handler, []}]}
    ]), 
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}, {ip, {0,0,0,0}}],
        #{env => #{dispatch => Dispatch}}
    ), 
    job_processor_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
