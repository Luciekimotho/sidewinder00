%% Author: root
%% Created: Feb 10, 2010
%% Description: TODO: Add description to topology
-module(topology).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/0]).

%%
%% API Functions
%%
start()-> setTopology([{1,0},
							{2,1},{3,1},
							{4,2},{5,3},{6,3}]),
		  attachClients().


%%
%% Local Functions
%%
setTopology([]) ->
	ok;
setTopology([{H1,H2}|T]) ->
	io:write([H1,'=>',H2]),
	setTopology(T).


attachClients()->todo.
	

