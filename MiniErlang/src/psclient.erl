%% Author: Andrea Matti
%% Created: Feb 10, 2010
%% Description: client. Two running mode: 1 as process, 2 as library called by erl console. In 1 it can publish random messages automatically (ClientMode=random) or manually (ClientMode=manual)
%%				In manual mode you must specify the ParentName and ParentNode of a running server process. In random mode you must send messages to the process in order to trigger a publishing event
%%				Publishing message format: {Attribute,Quantity,Value,ClientName,ClientNode} (String,String,Int,String,String)
%%				For semplicity random generation handles these: Attribute can be [Environmental, Physics, Performances], Quantity can be [Luminance, Temperature, Humidity, Speed, Size, Weight, Workload]
%%				Subscribing message format: {Fun,[]} [] is the Path that for a client is empty (for message compliance with server code)
%%				Fun format: fun(Message)-> true|false (accept publish Message as argument; return true or false)
%%				Subscription rules are handled in OR among them. AND logic is handled by Fun internally.
%%
-module(psclient).

%%
%% Include files
%%
-import(random).

%%
%% Exported Functions
%%
-export([start/1,start/2,start/3, start/4, publish/7, publish/5, subscribe/7, subscribe/5, unsubscribe/7, unsubscribe/5]).

%%
%% API Functions
%%

%% start/1 just call start/3
start(ClientName) -> 
	start(ClientName,null,null).

%% start/2 just call start/4. This is quite useless ecause of client random with parent set to null will not forward messages
start(ClientName,RandomTime) -> 
	start(ClientName,null,null,RandomTime).

%%
%% Start/4: register/unregister clientname, creates initial environment, call loopRandom/1. Set clientmode to random (it can be random/manual. If random it generates random publishing messages automatically each RandomTime milliseconds)
%%
start(ClientName,ParentName,ParentNode,RandomTime) -> 
	register(ClientName,self()),
	io:format("~s ~s~n",[ClientName, "started!"]),
	NewEnv=dict:from_list([{pname,ClientName},{clientmode,random},{randomtime,RandomTime},{parentname,ParentName},{parentnode,ParentNode},{monitorname,monitor},{monitornode,monitor@ubuntu904desktop},{quantities,["luminance", "temperature", "humidity","speed","size","weight","workload"]},{attributes,["environmental", "physics", "performances"]}]),
	%% Entering into the loop
	loopRandom(NewEnv),
	%%Exit from loop. Assuming pname is not changed.
	unregister(dict:fetch(pname,NewEnv)),
	io:format("~s client terminated!~n",[dict:fetch(pname,NewEnv)]).

%%
%% Start/3: register/unregister clientname, creates initial environment, call loop/1. ClientMode can be random/manual (random: it generates random messages automatically each RandomTime milliseconds)
%
start(ClientName,ParentName,ParentNode) -> 
	register(ClientName,self()),
	io:format("~s ~s~n",[ClientName, "started!"]),
	NewEnv=dict:from_list([{pname,ClientName},{clientmode,manual},{randomtime,null},{parentname,ParentName},{parentnode,ParentNode},{monitorname,monitor},{monitornode,monitor@ubuntu904desktop}]),
	%% Entering into the loop
	loop(NewEnv),
	%%Exit from loop. Assuming pname is not changed.
	unregister(dict:fetch(pname,NewEnv)),
	io:format("~s terminated!~n",[dict:fetch(pname,NewEnv)]).

%%
%% Loop: messages handling
%%
loop(Environment) -> 
	PName=dict:fetch(pname, Environment),
	PNode=node(),
	ClientMode=dict:fetch(clientmode, Environment),
	RandomTime=dict:fetch(randomtime, Environment),
	ParentName=dict:fetch(parentname, Environment),
	ParentNode=dict:fetch(parentnode, Environment),
	MonitorName=dict:fetch(monitorname, Environment),
	MonitorNode=dict:fetch(monitornode, Environment),
	receive
		%% GetEnv: cause client to print its environment, so it can be used from any client also without receive message loop
		{FromPid,FromNode,getenv,Message} ->
			%%{FromPid,FromNode}!{PName,PNode,getenv,Message,Environment},
			io:format("~s environment: ~w~n",[PName,Environment]),
			catch {MonitorName,MonitorNode}!{PName,PNode,message,"environment sent!"},
			loop(Environment);
		
		%% SetParent
		{FromPid,FromNode,setParent,PrntName,PrntNode} -> 
			NewEnv=dict:store(parentname,PrntName,Environment),
			NewEnv1=dict:store(parentnode,PrntNode,NewEnv),
			%%{FromPid,FromNode}!{PName,PNode,setparent,lists:concat(["client attached to ", PrntName," ",PrntNode])}, %% Ack
			{PrntName,PrntNode}!{PName,PNode,addchild,PName,PNode}, %% Add itself to parent child list
			io:format("~s client attached to ~w ~s~n",[PName,PrntName,PrntNode]),
			catch {MonitorName,MonitorNode}!{PName,PNode,message,lists:concat(["client attached to ", PrntName," ",PrntNode])},
			loop(NewEnv1);
		
		%% Publish Message format= {Attribute,Quantity,Value}
		{FromPid,FromNode,publish,Message} -> 
			if
				ParentName=/=null -> %% Send messagee if parent is not null
					{ParentName,ParentNode}!{PName,PNode,publish,Message},
					io:format("~s ~s ~w~n",[PName,"message published!",Message]),
					catch {MonitorName,MonitorNode}!{PName,PNode,message,"message published!",Message};
				true -> 
					io:format("~s ~s ~w~n",[PName,"parent=null message not published!",Message]),
					catch {MonitorName,MonitorNode}!{PName,PNode,message,"parent=null message not published!",Message}
			end,
			loop(Environment);
		
		%% Subscribe Message format= {Fun,[]}
		{FromPid,FromNode,subscribe,Message} -> 
			if
				ParentName=/=null -> %% Send messagee if parent is not null
					{ParentName,ParentNode}!{PName,PNode,subscribe,Message},
					io:format("~s ~s ~w~n",[PName,"subscription sent!",Message]),
					catch {MonitorName,MonitorNode}!{PName,PNode,message,"subscription sent!",Message};
				true -> 
					io:format("~s ~s ~w~n",[PName,"parent=null subscription not sent!",Message]),
					catch {MonitorName,MonitorNode}!{PName,PNode,message,"parent=null subscription not sent!",Message}
			end,
			loop(Environment);
		
		%% Unsubscribe Message format= {Fun,[]}
		{FromPid,FromNode,unsubscribe,Message} -> 
			if
				ParentName=/=null -> %% Send messagee if parent is not null
					{ParentName,ParentNode}!{PName,PNode,unsubscribe,Message},
					io:format("~s ~s ~w~n",[PName,"unsubscription sent!",Message]),
					catch {MonitorName,MonitorNode}!{PName,PNode,message,"unsubscription sent!",Message};
				true -> 
					io:format("~s ~s ~w~n",[PName,"parent=null unsubscription not sent!",Message]),
					catch {MonitorName,MonitorNode}!{PName,PNode,message,"parent=null unsubscription not sent!",Message}
			end,
			loop(Environment);
		
		%% unsubscribeclient.
		%% Rimuove tutte le sottoscrizioni del client {ClientName,ClientNode}. Primo elemento dei Path delle rules. Propaga verso l'alto.
		{FromPid,FromNode,unsubscribeclient,ClientName,ClientNode} ->
			if
				ParentName=/=null -> %% Send messagee if parent is not null
					{ParentName,ParentNode}!{PName,PNode,unsubscribeclient,ClientName,ClientNode},
					io:format("~s ~s ~w ~s~n",[PName,"client unsubscription sent! Client=",ClientName,ClientNode]),
					catch {MonitorName,MonitorNode}!{PName,PNode,message,"client unsubscription sent!"};
				true -> 
					io:format("~s ~s~n",[PName,"parent=null client unsubscription not sent!"]),
					catch {MonitorName,MonitorNode}!{PName,PNode,message,"parent=null client unsubscription not sent!"}
			end,
			loop(Environment);
			
		
		%% Delivery: riceived from broker server (parent) during message delivery
		{FromPid,FromNode,delivery,Message,MatchingRules} -> 
			io:format("~s message received! ~p~n",[PName,Message]),
			catch {MonitorName,MonitorNode}!{PName,PNode,message,"message received!", Message},
			loop(Environment);
		
		%% IsServer: true(per i server)/false(per i client)
		{FromPid,FromNode,isserver,Message} -> 
			io:format("~s I'm a client! isserver=~s~n",[PName,"false"]),
			%%{FromPid,FromNode}!{PName,PNode,iserver,"false"},
			loop(Environment);
		
		%% Stop the client	
		{FromPid,FromNode,stop,Message} -> 
			io:format("~s client stopped! ~s~n",[PName,Message])
			%%{FromPid,FromNode}!{PName,PNode,stop,lists:concat(["client stopped! ", Message])}

	
	end.

%%
%% loopRandom: messages handling with automatic publishing message generation
%%
loopRandom(Environment) -> 
	PName=dict:fetch(pname, Environment),
	PNode=node(),
	ClientMode=dict:fetch(clientmode, Environment),
	RandomTime=dict:fetch(randomtime, Environment),
	Attributes=dict:fetch(attributes, Environment),
	Quantities=dict:fetch(quantities, Environment),
	ParentName=dict:fetch(parentname, Environment),
	ParentNode=dict:fetch(parentnode, Environment),
	MonitorName=dict:fetch(monitorname, Environment),
	MonitorNode=dict:fetch(monitornode, Environment),
	receive		
		%% GetEnv
		{FromPid,FromNode,getenv,Message} ->
			{FromPid,FromNode}!{PName,PNode,getenv,Message,Environment},
			catch {MonitorName,MonitorNode}!{PName,PNode,message,"environment sent!"},
			loopRandom(Environment);
		
		%% SetParent
		{FromPid,FromNode,setParent,PrntName,PrntNode} -> 
			NewEnv=dict:store(parentname,PrntName,Environment),
			NewEnv1=dict:store(parentnode,PrntNode,NewEnv),
			{FromPid,FromNode}!{PName,PNode,setparent,lists:concat(["parent changed to ", PrntName," ",PrntNode])}, %% Ack
			{PrntName,PrntNode}!{PName,PNode,addchild,PName,PNode}, %% Add itself to parent child list
			catch {MonitorName,MonitorNode}!{PName,PNode,message,lists:concat(["client attached to ", PrntName," ",PrntNode])},
			loopRandom(NewEnv1);
		
		%% Publish Message format = {Attribute,Quantity,Value}
		{FromPid,FromNode,publish,Message} -> 
			if
				ParentName=/=null -> %% Send messagee if parent is not null
					{ParentName,ParentNode}!{PName,PNode,publish,Message},
					io:format("~s ~s ~w~n",[PName,"message published!",Message]),
					catch {MonitorName,MonitorNode}!{PName,PNode,message,"message published!",Message};
				true -> 
					io:format("~s ~s ~w~n",[PName,"parent=null message not published!",Message]),
					catch {MonitorName,MonitorNode}!{PName,PNode,message,"parent=null message not published!",Message}
			end,
			loopRandom(Environment);
		
		%% Subscribe Message format= {Fun,[]}. Forward original to parent server. It can be sent from any console.
		{FromPid,FromNode,subscribe,Message} -> 
			if
				ParentName=/=null -> %% Send messagee if parent is not null
					{ParentName,ParentNode}!{PName,PNode,subscribe,Message},
					io:format("~s ~s ~w~n",[PName,"subscription sent!",Message]),
					catch {MonitorName,MonitorNode}!{PName,PNode,message,"subscription sent!",Message};
				true -> 
					io:format("~s ~s ~w~n",[PName,"parent=null subscription not sent!",Message]),
					catch {MonitorName,MonitorNode}!{PName,PNode,message,"parent=null subscription not sent!",Message}
			end,
			loopRandom(Environment);
		
		%% Unsubscribe Message format= {Fun,[]}. Forward original to parent server. It can be sent from any console.
		{FromPid,FromNode,unsubscribe,Message} -> 
			if
				ParentName=/=null -> %% Send messagee if parent is not null
					{ParentName,ParentNode}!{PName,PNode,unsubscribe,Message},
					io:format("~s ~s ~w~n",[PName,"unsubscription sent!",Message]),
					catch {MonitorName,MonitorNode}!{PName,PNode,message,"unsubscription sent!",Message};
				true -> 
					io:format("~s ~s ~w~n",[PName,"parent=null unsubscription not sent!",Message]),
					catch {MonitorName,MonitorNode}!{PName,PNode,message,"parent=null unsubscription not sent!",Message}
			end,
			loopRandom(Environment);
		
		%% unsubscribeclient.
		%% Rimuove tutte le sottoscrizioni del client {ClientName,ClientNode}. Primo elemento dei Path delle rules. Propaga verso l'alto.
		{FromPid,FromNode,unsubscribeclient,ClientName,ClientNode} ->
			if
				ParentName=/=null -> %% Send messagee if parent is not null
					{ParentName,ParentNode}!{PName,PNode,unsubscribeclient,ClientName,ClientNode},
					io:format("~s ~s ~w ~s~n",[PName,"client unsubscription sent! Client=",ClientName,ClientNode]),
					catch {MonitorName,MonitorNode}!{PName,PNode,message,"client unsubscription sent!"};
				true -> 
					io:format("~s ~s~n",[PName,"parent=null client unsubscription not sent!"]),
					catch {MonitorName,MonitorNode}!{PName,PNode,message,"parent=null client unsubscription not sent!"}
			end,
			loopRandom(Environment);
		
		%% Delivery: riceived from broker server (parent) during message delivery
		{FromPid,FromNode,delivery,Message,MatchingRules} -> 
			io:format("~s message received! ~p~n",[PName,Message]),
			catch {MonitorName,MonitorNode}!{PName,PNode,message,"message received!", Message},
			loopRandom(Environment);
		
		%% IsServer: true(per i server)/false(per i client)
		{FromPid,FromNode,isserver,Message} ->
			io:format("~s I'm a client! isserver=~s~n",[PName,"false"]),
			%%{FromPid,FromNode}!{PName,PNode,isserver,"false"};
			loopRandom(Environment);

		%% Stop the client	
		{FromPid,FromNode,stop,Message} -> 
			io:format("~s client stopped! ~s~n",[PName,Message])
			%%{FromPid,FromNode}!{PName,PNode,stop,lists:concat(["client stopped! ", Message])}		
	
	after RandomTime ->
		
		%% Randmely generated publish message
		Attribute=random:uniform(3),
		if 
			Attribute==1 -> %% environmental da 1 a 3
				Quantity = random:uniform(3);
			Attribute==2 -> %% physics da 4 a 6
				Quantity = random:uniform(3) + 3;
			true -> 		%% performances 7
				Quantity = 7
		end,
		Value= random:uniform(100),
		Message= {lists:nth(Attribute,Attributes),lists:nth(Quantity,Quantities),Value},
		if
			ParentName=/=null -> 
				{ParentName,ParentNode}!{PName,PNode,publish,Message}, %% Spedisce messaggio al parent se non nullo
				io:format("~s random message published! ~p~n",[PName,Message]),
				catch {MonitorName,MonitorNode}!{PName,PNode,message,"random message published!",Message}; %% Notifica monitor;
			true ->
				io:format("~s parent=null random message not published! ~p~n",[PName,Message]),
				catch {MonitorName,MonitorNode}!{PName,PNode,message,"parent=null random message not published!",Message} %% Notifica monitor
		end,
		loopRandom(Environment)

	end.

%%
%% Subscribe as library's function. Subscribe message format= {Fun,[]}
%%
subscribe(PName,PNode,ParentName,ParentNode,MonitorName,MonitorNode,Message) -> 
		{ParentName,ParentNode}!{PName,PNode,subscribe,Message},
		catch {MonitorName,MonitorNode}!{PName,PNode,message,lists:concat(["subscription message sent!"])}.
subscribe(PName,PNode,ParentName,ParentNode,Message) -> 
		{ParentName,ParentNode}!{PName,PNode,subscribe,Message}.

%%
%% Unsubscribe as library's function. Unsubscribe message format= {Fun,[]}
%%
unsubscribe(PName,PNode,ParentName,ParentNode,MonitorName,MonitorNode,Message) -> 
		{ParentName,ParentNode}!{PName,PNode,unsubscribe,Message},
		catch {MonitorName,MonitorNode}!{PName,PNode,message,lists:concat(["unsubscription message sent!"])}.
unsubscribe(PName,PNode,ParentName,ParentNode,Message) -> 
		{ParentName,ParentNode}!{PName,PNode,unsubscribe,Message}.

%%
%% Publish as library's function. Publish message format = {Attribute,Quantity,Value} 
%%
publish(PName,PNode,ParentName,ParentNode,MonitorName,MonitorNode,Message) -> 
		{ParentName,ParentNode}!{PName,PNode,publish,Message},
		catch {MonitorName,MonitorNode}!{PName,PNode,message,lists:concat(["publish message sent!"," ",element(1,Message)," ",element(2,Message)," ",element(3,Message)," "])}.
publish(PName,PNode,ParentName,ParentNode,Message) -> 
		{ParentName,ParentNode}!{PName,PNode,publish,Message}.


%%
%% Local Functions
%%

