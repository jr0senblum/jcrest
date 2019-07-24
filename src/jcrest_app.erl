%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum
%%% @copyright (C) 2020, Jim Rosenblum
%%% @doc Application module for the JCREST application.
%%%
%%% @version {@version}
%%% @end
%%% Created : Jul 2019 by Jim Rosenblum
%%% ----------------------------------------------------------------------------
-module(jcrest_app).

-behaviour(application).


%% Application callbacks.
-export([start/2, stop/1]).


%%% ============================================================================
%%% Application callbacks
%%% ============================================================================

%% -----------------------------------------------------------------------------
%% @private Start the supervisor
%% 
-spec start (normal | {takeover   | failover, atom()}, [{node, atom()}]) -> 
   		      {ok, pid()} | {error, atom()}.

start(_StartType, _StartArgs) ->
    start_REST_service(),
    case jcrest_sup:start_link() of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    {error, Error}
    end.


%% -----------------------------------------------------------------------------
%% @doc Called when the application is stopped.
%% 
-spec stop(term()) -> ok.

stop(_State) ->
    ok.


start_REST_service() ->
    IP = application:get_env(jcrest, server_ip, "127.0.0.1"),
    Port = application:get_env(jcrest, server_port, 8080),
    Root = application:get_env(jcrest, server_root, "/"),

    Dispatch = cowboy_router:compile(
                 [
                  {IP, [
                        {Root ++ "/maps/:map/search/:path", cb_collections_h, [search]},
                        {Root ++ "/maps/:map/:key", cb_map_h, [key]},
                        {Root ++ "/maps/:map", cb_collections_h, [map]},
                        {Root ++ "maps", cb_collections_h, [maps]}
                       ]}
                 ]),

    {ok, _} = cowboy:start_clear(http, [{port, Port}], 
                                 #{env => #{dispatch => Dispatch}}
                                ),

    lager:info("~p: RESTFUL http server is up and listening on host: ~p, port: ~p, path: ~p.",
               [?MODULE, IP, Port, Root]).
