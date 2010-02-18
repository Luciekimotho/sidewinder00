%% Author: root
%% Created: Feb 10, 2010
%% Description: it crates initial statically predefined servers topology and attach to them a few clients
-module(topology).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/0,stop/0,startTopology/0,stopTopology/0]).

%%
%% API Functions
%%

%%
%% Set the predefined topology. In order to make some changes you need modify the code directly.
%% Host = host machine (TCP/IP)
%% Node = erlang VM  (erl/beam) running on a host
%% Server = process running on a erlang VM
%% In order to launch process on other host we need to open remotely at least one Erlang VMin a xterm. Then we could use spawn/4.
%% My Topology:
%% {ubuntu904desktop host         								}
%% (node1   )   (node2 )   (node3 )    (node4)  (node5)   (node6)
%% ::::::::::::::::::::::::::::::::
%%                         ( |--s6)
%% (s1-|----)---(--s3--)---(-|--s5)------------(-c2  )
%% (   |-s2-)|--(--s4  )---------------------------------(-c3  )
%% 			 |-------------------------(-c1  )
%% 

start()-> 
		%% Start nodes
		%% Need una-tantum gnome-terminal erlang profile creation from menu Edit->Profiles... of gnome-terminal 
		os:cmd('gnome-terminal --window-with-profile=erlang --title=node1 -e "erl -sname node1 -s topology startTopology" &'),
		os:cmd('gnome-terminal --window-with-profile=erlang --title=node2 -e "erl -sname node2 -s topology startTopology" &'),
		os:cmd('gnome-terminal --window-with-profile=erlang --title=node3 -e "erl -sname node3 -s topology startTopology" &'),
		os:cmd('gnome-terminal --window-with-profile=erlang --title=node4 -e "erl -sname node4 -s topology startTopology" &'),
		os:cmd('gnome-terminal --window-with-profile=erlang --title=node5 -e "erl -sname node5 -s topology startTopology" &'),
		os:cmd('gnome-terminal --window-with-profile=erlang --title=node6 -e "erl -sname node6 -s topology startTopology" &'),
		%% Launch Monitor
		os:cmd('gnome-terminal --window-with-profile=erlang --title=monitor -e "erl -sname monitor -s monitor start" &'),
		%% Test Client is the same gnome-terminanl which launch startnodes.sh or erlang VM (beam) which call topology:start(). 
		ok.

stop() -> 
	stopTopology(),
	os:cmd('killall gnome-terminal').

%%
%% Local Functions
%%

%%
%% Start server and client processes on nodes.
%%
%% My Topology:
%% {ubuntu904desktop host         								}
%% (node1   )   (node2 )   (node3 )    (node4)  (node5)   (node6)
%% ::::::::::::::::::::::::::::::::
%%                         ( |--s6)
%% (s1-|----)---(--s3--)---(-|--s5)------------(-c2  )
%% (   |-s2-)|--(--s4  )---------------------------------(-c3  )
%% 			 |-------------------------(-c1  )

%% We may call startTopology directly if we called before stopTopology and we not closed nodes
startTopology() -> 
	Node=node(),
	%% Startup time is different among nodes so doesn't use messages to set topology (the destination couldn't be start yet even if source run command come before destination one)
	if
		%% Servers
		%% S0 node0@ubuntu904desktop means null node
		Node==node1@ubuntu904desktop -> %% Node1 s1/s2
			spawn(psserver,start,[s1,s0,node0@ubuntu904desktop,[{s2,node1@ubuntu904desktop},{s3,node2@ubuntu904desktop}]]),
			spawn(psserver,start,[s2,s1,node1@ubuntu904desktop,[{s4,node2@ubuntu904desktop}],[{c1,node2@ubuntu904desktop}]]);
		Node==node2@ubuntu904desktop -> %% Node2 s3/s4 
			spawn(psserver,start,[s3,s1,node1@ubuntu904desktop,[{s5,node3@ubuntu904desktop},{s6,node3@ubuntu904desktop}]]),
			spawn(psserver,start,[s4,s2,node1@ubuntu904desktop,[],[{c2,node5@ubuntu904desktop}]]);
		Node==node3@ubuntu904desktop -> %% Node3 s5/s6
			spawn(psserver,start,[s5,s3,node2@ubuntu904desktop,[],[{c3,node6@ubuntu904desktop}]]),
			spawn(psserver,start,[s6,s3,node2@ubuntu904desktop,[]]);
		%% Clients.
		%% In order to start client in automatic mode pass it also RandomTime in milliseconds as 4th argument
		Node==node4@ubuntu904desktop -> %% Node4 c1
			spawn(psclient,start,[c1,s2,node1@ubuntu904desktop]); 
		Node==node5@ubuntu904desktop -> %% Node5 c2
			spawn(psclient,start,[c2,s5,node3@ubuntu904desktop]);
		Node==node6@ubuntu904desktop -> %% Node6 c3
			spawn(psclient,start,[c3,s4,node2@ubuntu904desktop]);
		%% Monitor process already started by start/1
		%% Test Client is the same gnome-terminanl which launch startnodes.sh or erlang VM (beam) which call topology:start().
		true->
			ok
	end.

stopTopology() -> 
	{s1,node1@ubuntu904desktop}!{self(),node(),stop,""},
	{s2,node1@ubuntu904desktop}!{self(),node(),stop,""},
	{s3,node2@ubuntu904desktop}!{self(),node(),stop,""},
	{s4,node2@ubuntu904desktop}!{self(),node(),stop,""},
	{s5,node3@ubuntu904desktop}!{self(),node(),stop,""},
	{s6,node3@ubuntu904desktop}!{self(),node(),stop,""},
	{c1,node4@ubuntu904desktop}!{self(),node(),stop,""},
	{c2,node5@ubuntu904desktop}!{self(),node(),stop,""},
	{c3,node6@ubuntu904desktop}!{self(),node(),stop,""},
	ok.
	