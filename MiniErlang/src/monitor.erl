%% Author: root
%% Created: Feb 11, 2010
%% Description: receive text messages or text messages with attached value from distributed processes running at different nodes and print them until it receive a stop message
-module(monitor).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/0, start/2, send/1, send/3, stop/0, stop/2]).

%%
%% API Functions
%%

%% Start the monitor passing it an initial environment containing the process name pname as atom, register the atom

%% start/0 just call start/2
start()->
	start(monitor,node()).

%% start/2
start(MonitorName,MonitorNode)->
	register(MonitorName,self()),
	io:format("From ~w at ~w ~s~n", [MonitorName,MonitorNode,'started!']),
	Env = dict:from_list([{pname,monitor}]),
	%%{s1,node1@ubuntu904desktop}!{monitor,node(),getenv,"Environment"},
	%% Get into loop
	loop(Env),
	%% Exit from loop
	unregister(MonitorName),
	io:format("From ~w at ~w ~s~n", [MonitorName,MonitorNode,'stopped!']).

%% loop/1
loop(Env)->
	receive
		{FromPid,FromNode,message,Message,Value} ->
			io:format("From ~w at ~s : ~s = ~p~n", [FromPid,FromNode,Message,Value]),
			loop(Env);
		{FromPid,FromNode,message,Message} ->
			io:format("From ~w at ~s : ~s~n", [FromPid,FromNode,Message]),
			loop(Env);
		{FromPid,FromNode,stop,Message} -> 
			io:format("From ~w at ~s : ~s~n", [FromPid,FromNode,Message])	
	end.

%%
%% Facilities
%%

%% send/1. Send message to monitor assuming monitor default names are used. Just call send/3
  send(Message) ->
	send(monitor,monitor@ubuntu904desktop,Message).

%% send/3. More general.
send(MonitorName,MonitorNode,Message) -> 
	{MonitorName, MonitorNode}!{self(),node(),message,Message}.

%% stop/0. Stop the monitor assuming monitor default names are used. Just call stop/2
stop()-> 
	stop(monitor,monitor@ubuntu904desktop).

%% stop/2. More general.
stop(MonitorName,MonitorNode) -> 
	{MonitorName, MonitorNode}!{self(),node(),stop,"Stop"}.

%%
%% Local Functions
%%

