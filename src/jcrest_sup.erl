%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum
%%% @copyright (C) 2020, Jim Rosenblum
%%% @doc Top-level supervisor for the jc application. 
%%% 
%%% @version {@version}
%%% @end
%%% Created : Jul 2020 by Jim Rosenblum
%%% ----------------------------------------------------------------------------
-module(jcrest_sup).

-behaviour(supervisor).


%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


%%% ============================================================================
%%% API functions
%%% ============================================================================

%% -----------------------------------------------------------------------------
%% @doc Start the supervisor.
%%
-spec start_link() -> {ok, pid()} | ignore | {error, {already_started, pid()}}.

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%%% ============================================================================
%%% Supervisor callbacks
%%% ============================================================================

%% -----------------------------------------------------------------------------
%% @private Fire up the supervised processes.
%%
-spec init([]) ->  {'ok', {{'one_for_one', 60, 3600}, 
                           [supervisor:child_spec()]}}.

init([]) ->
    {ok, {{one_for_one, 60, 3600}, []}}.
