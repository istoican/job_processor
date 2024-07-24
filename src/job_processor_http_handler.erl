-module(job_processor_http_handler).

-include("job_processor.hrl").

-export([init/2, terminate/3]).


init(Req0 = #{method := <<"POST">>}, State) ->
    Req = case cowboy_req:has_body(Req0) of
        true ->
            post_handler(Req0);
        false ->
            cowboy_req:reply(400, Req0)
    end,
    {ok, Req, State};
init(Req, State) ->
    {ok, cowboy_req:reply(405, Req), State}.

post_handler(Req0) ->
    {ok, Body, Req} = cowboy_req:read_body(Req0),
    Tasks0 = parse_request(Body),

    case job_processor:sort(Tasks0) of
        {ok, Tasks} ->
            QsVals = cowboy_req:parse_qs(Req),
            Format = lists:keyfind(<<"format">>, 1, QsVals),
            case Format of
                {<<"format">>, <<"script">>} 
                    -> 
                        Response = to_script(Tasks),
                        cowboy_req:reply(200, #{<<"content-type">> => "text/plain"}, Response, Req);
                _ 
                    ->
                        Response = to_json(Tasks),
                        cowboy_req:reply(200, #{<<"content-type">> => "application/json"}, [Response, "\n"], Req)
            end;
        {false} ->
            cowboy_req:reply(400, #{<<"content-type">> => "application/json"}, <<"{error:\"unprocessable job\"}">>, Req0)
    end.

parse_request(Body) ->
    Data = jsx:decode(Body),
    Tasks = proplists:get_value(<<"tasks">>, Data),
    [ proplist_to_record(T) || T <- Tasks ].

proplist_to_record(Props) ->  
  #task{name=proplists:get_value(<<"name">>, Props), 
        command=proplists:get_value(<<"command">>, Props), 
        requires=proplists:get_value(<<"requires">>, Props, [])}.  

to_script(Tasks) ->
    to_script(Tasks, ["#!/usr/bin/env bash", "\n"]).
to_script([], Acc) ->
    Acc;
to_script([H|T], Acc) ->
    to_script(T, Acc ++ [H#task.command, "\n"]).

to_json(Tasks) ->
    to_json(Tasks, []).
to_json([], Acc) ->
    jsx:encode(#{<<"tasks">> => Acc});
to_json([H|T], Acc) ->
    to_json(T, Acc ++ [#{<<"name">> => H#task.name, <<"command">> => H#task.command}]).

terminate(_Reason, _Req, _State) ->
    ok.
