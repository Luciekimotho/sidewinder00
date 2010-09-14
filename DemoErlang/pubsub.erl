-module(pubsub).
-export([startServer/1, startClient/1, serverLoop/5, pingChildren/0, 
		subscribe/1, printSubscriptions/0, publish/2, closeApp/0,
		unsubscribe/1,crash/0]).

startClient([Parent]) ->
	io:fwrite("Starting client...~n"),
	Pid = spawn(fun() -> serverLoop(Parent, [], [], no, []) end),
	register(server, Pid),
	io:fwrite("Client started!~n"),
	setParent().

startServer([Parent]) ->
	io:fwrite("Starting server...~n"),
	Pid = spawn(fun() -> serverLoop(Parent, [], [], yes, []) end),
	register(server, Pid),
	io:fwrite("Server started!~n"),
	setParent(),
	%% Start the error handler
	on_exit(Pid,Parent),
	io:fwrite("Handler started!~n").

restartServer(Parent,Children,Subscription) ->
	io:fwrite("Restarting server...after 20 sec~n"),
	timer:sleep(20000),
	%% Get subscription from the attached server
	if 
	    Parent=/=node() ->
		{server,Parent}!{getSubscriptions,self(),node()},
		receive
		    {subscriptionList,ParentSubscription}->
			io:format("~s ~p~n~s ~p~n",["Mio:",Subscription,"Padre:",ParentSubscription]),			
			%%Aggiunge tutto ciò che c'è nella nuova subscription (1* parametro) a quello che c'è nella mia (2* parametro) partendo dalla situazione iniziale mia (3* parametro). Il 4* parametro viene usato per avvertire i restanti nodi qualora aggiungo una sottoscrizione a me visto che ero giu e non ho potuto inoltrare la notifica
			NewParentSubscription1=mergeSubscription(ParentSubscription,Subscription,Subscription,Parent,Children,Parent),
			io:format("~s ~p~n",["Nuove sottoscrizioni dopo merge subscribe con padre:",NewParentSubscription1]),
			NewParentSubscription2=mergeUnsubscription(ParentSubscription,NewParentSubscription1,[],Parent,Children,Parent),
			io:format("~s ~p~n",["Nuove sottoscrizioni dopo merge unsubscribe con padre:",NewParentSubscription2]),
			NewSubscription=requestChildrenSubscripionList(Children,NewParentSubscription2,Parent,Children),
			io:format("~s ~p~n",["Nuove sottoscrizioni dopo merge:",NewSubscription]),
			Pid = spawn(fun() -> serverLoop(Parent, Children, NewSubscription, yes, []) end),
			register(server, Pid),
			io:fwrite("Server restarted!~n"),
			setParent(),
			%% Start the error handler
			on_exit(Pid,Parent),
			io:fwrite("Handler started!~n")
		end;
	    true->ok
	end.

%% error handler
on_exit(Pid,Parent)->
    PidHandler=spawn(fun()->
			process_flag(trap_exit,true),
			link(Pid),
			handlerLoop(Parent,[],[],Pid)
		     end),
    Pid!{handler,PidHandler}.

handlerLoop(Parent,Children,Subscription,Pid) ->
    receive 
	{'EXIT',Pid,_}->
			io:fwrite("Errore nel server, riavvio!~n~n"),
			restartServer(Parent,Children,Subscription);
	{children,NewChildren} ->
	    io:fwrite("Update childern~n"),
	    handlerLoop(Parent,NewChildren,Subscription,Pid);
	{subscription,NewSubscription} ->
	    io:fwrite("Update subscription~n"),
	    handlerLoop(Parent,Children,NewSubscription,Pid)
    end.

%% Get the subscription list from childred
requestChildrenSubscripionList([],Subscription,_,_) ->
	Subscription;
requestChildrenSubscripionList([Child | OtherChilds],Subscription,Parent,Children) ->
	{server,Child}!{getSubscriptions,self(),node()},
	receive
	    {subscriptionList,ChildSubscription}->
		io:format("~s ~p~n~s ~p~n",["Mio:",Subscription,"Figlio:",ChildSubscription]),			
		%%Aggiunge tutto ciò che c'è nella nuova subscription (1* parametro) a quello che c'è nella mia (2* parametro) partendo dalla situazione iniziale mia (3* parametro). Il 4* parametro viene usato per avvertire i restanti nodi qualora aggiungo una sottoscrizione a me visto che ero giu e non ho potuto inoltrare la notifica
		NewChildSubscription1=mergeSubscription(ChildSubscription,Subscription,Subscription,Parent,Children,Child),
		io:format("~s ~p~n",["Nuove sottoscrizioni dopo merge subscribe con figlo:",NewChildSubscription1]),
		NewChildSubscription2=mergeUnsubscription(ChildSubscription,NewChildSubscription1,[],Parent,Children,Child),
		io:format("~s ~p~n",["Nuove sottoscrizioni dopo merge unsubscribe con figlo:",NewChildSubscription2]),
		requestChildrenSubscripionList(OtherChilds,NewChildSubscription2,Parent,Children);
	    subscriptionList->
		requestChildrenSubscripionList(OtherChilds,Subscription,Parent,Children)
	end.

serverLoop(Parent, Children, Subscriptions, IsServer, PidHandler) ->
	receive
		%% Get the handler pid
		{handler,Handler}->
			    io:format("~s ~p ~n",["Ricevuto PID Handler:",Handler]),
			    serverLoop(Parent, Children, Subscriptions,IsServer,Handler);

		%% Sends a setChildren message to the parent node
		setParent ->
			if 
				Parent =/= node() ->
					{server, Parent} ! {setChildren, node()},
					serverLoop(Parent, Children, Subscriptions, IsServer, PidHandler);
				true ->
					io:fwrite("Root node, no parent added~n"),
					serverLoop(Parent, Children, Subscriptions, IsServer, PidHandler)
			end;
			
		%% Reply to the setChildren message
		setParentAck ->
			io:fwrite("Parent attached!~n"),
			serverLoop(Parent, Children, Subscriptions, IsServer, PidHandler);
			
		setParentAbort ->
			io:fwrite("Cannot attach to a Client node!~n"),
			io:fwrite("Closing application~n");
			
		%% Adds a children to the children list
		{setChildren, Child} ->
			if
				IsServer == yes ->
					io:fwrite("setChildren message received...~n"),
					IsChild = matches(Child, Children),
					if
						IsChild == yes ->
							io:fwrite("Node is already in the children list~n"),
							NewChildren = Children;
						IsChild == no ->
							io:fwrite("Adding new Child~n"),
							NewChildren = [Child | Children]
					end,
					{server, Child} ! setParentAck,
					PidHandler ! {children,NewChildren},
					serverLoop(Parent, NewChildren, Subscriptions, IsServer, PidHandler);
				IsServer == no ->
					io:fwrite("Clients cannot have children!~n"),
					io:fwrite("Sending abort message!~n"),
					{server, Child} ! setParentAbort,
					serverLoop(Parent, Children, Subscriptions, IsServer, PidHandler)
			end;
			
		%% Start publishing a message
		{startPublish, Name, Value} ->
			io:fwrite("Publishing ~w~n", [{Name, Value}]),
			{server, Parent} ! {publish, Name, Value, node()},
			serverLoop(Parent, Children, Subscriptions, IsServer, PidHandler);
			
		%% Publish a new message
		{publish, Name, Value, SourceNode} ->
			if
				IsServer == yes ->
					relayMessage(Name, Value, Subscriptions, SourceNode);
				IsServer == no ->
					io:fwrite("Message received: ~w~n", [{Name, Value}])
			end,
			serverLoop(Parent, Children, Subscriptions, IsServer, PidHandler);
		
		%% Subscribes (client mode)
		{startSubscription, Subscription} ->
			io:fwrite("Subscribing (user initiated)~n"),
			{server, Parent} ! {subscribe, node(), Subscription},
			serverLoop(Parent, Children, Subscriptions, IsServer, PidHandler);
			
		%% Handles a subscription request (server mode)
		{subscribe, Node, Subscription} ->
			if
				IsServer == yes ->
					io:fwrite("Subscription request received...~n"),
					propagateSubscription(Parent, Children, Node, Subscription),
					NewSubscriptions = addSubscription({Subscription, Node}, Subscriptions, []),
					PidHandler ! {subscription,NewSubscriptions},
					serverLoop(Parent, Children, NewSubscriptions, IsServer, PidHandler);
				IsServer == no ->
					serverLoop(Parent, Children, Subscriptions, IsServer, PidHandler)
			end;

		%% Get the subscription list (server mode)
		{getSubscriptions,PidNode,Node} ->
			io:fwrite("Subscription list requested...~n"),
			if 
			    IsServer==yes->
				%Rimuovo le sottoscrizioni relative al nodo che mi ha richiesto l'informazione per evitare che lui aggiunga sottoscrizioni da inviare a se stesso e imposto le restanti al mio nodo				
				SubscriptionsRequested=modifySubscriptions(Subscriptions,Node,[]),
				io:format("~s ~p~n~s ~p~n",["Prima:",Subscriptions,"Dopo:",SubscriptionsRequested]),			
				PidNode ! {subscriptionList,SubscriptionsRequested};
			    true->
				PidNode ! subscriptionList
			end,
			serverLoop(Parent, Children, Subscriptions, IsServer,PidHandler);
			
		%% Unsubscribes (client mode)
		{startUnsubscribe, Subscription} ->
			io:fwrite("Unsubscribe (user initiated)~n"),
			{server, Parent} ! {unsubscribe, node(), Subscription},
			serverLoop(Parent, Children, Subscriptions, IsServer, PidHandler);
			
		%% Handles an unsubscription request (server mode)
		{unsubscribe, Node, Subscription} ->
			if
				IsServer == yes ->
					io:fwrite("Unsubscribe request received...~n"),
					propagateUnsubscribe(Parent, Children, Node, Subscription),
					NewSubscriptions = removeSubscription(Subscription, Node, Subscriptions, []),
					PidHandler ! {subscription,NewSubscriptions},
					serverLoop(Parent, Children, NewSubscriptions, IsServer, PidHandler);
				IsServer == no ->
					serverLoop(Parent, Children, Subscriptions, IsServer, PidHandler)
			end;
		
		%% Prints the current subscription list
		printSubscriptions ->
			io:fwrite("Printing subscription list:~n"),
			printSubscriptionList(Subscriptions),
			serverLoop(Parent, Children, Subscriptions, IsServer, PidHandler);
		
		%% Start pinging the children	
		pingChildren ->
			executePingChildren(Children),
			serverLoop(Parent, Children, Subscriptions, IsServer, PidHandler);
			
		%% Ping message received
		ping ->
			io:fwrite("Ping received!~n"),
			serverLoop(Parent, Children, Subscriptions, IsServer, PidHandler);

		%% Crash message received
		crash ->
			2*f;
			
		%% Removes a children when it exits
		{removeChildren, NodeToRemove} ->
			if
				IsServer == yes ->
					io:fwrite("Children ~w has exited, removing it...~n", [NodeToRemove]),
					NewChildren = lists:delete(NodeToRemove, Children),
					NewSubscriptions = removeNodeFromSubscription(Parent, NewChildren, NodeToRemove, Subscriptions, []),
					serverLoop(Parent, NewChildren, NewSubscriptions, IsServer, PidHandler);
				IsServer == no ->
					serverLoop(Parent, Children, Subscriptions, IsServer, PidHandler)
			end;
		
		%% Terminates the application (if possible)
		exit ->
			if
				Children == [] ->
					{server, Parent} ! {removeChildren, node()},
					io:fwrite("Application closed!~n");
				true ->
					io:fwrite("Unable to exit, there still are children attached!~n"),
					serverLoop(Parent, Children, Subscriptions, IsServer, PidHandler)
			end
	end.
	
closeApp() ->
	io:fwrite("Closing application...~n"),
	server ! exit.

%% Send a bad message to server to test handlers (debug only)
crash() ->
	server ! crash.	
	
%% Triggers the sending of the setChildren message
setParent() ->
	io:fwrite("Requesting to attach as a children...~n"),
	server ! setParent.
	
%% Triggers the publish of a new message
publish(Name, Value) ->
	server ! {startPublish, Name, Value}.
	
%% Relays the message to all the subscribed nodes
relayMessage(_, _, [], _) ->
	ok;
relayMessage(MsgName, MsgValue, [{Name, Nodes} | _], SourceNode) when MsgName == Name ->
	sendMessage(MsgName, MsgValue, Nodes, SourceNode);
relayMessage(MsgName, MsgValue, [_ | OtherSub], SourceNode) ->
	relayMessage(MsgName, MsgValue, OtherSub, SourceNode).
	
%% Sends the message to all the subscribed nodes (except the node where the message came from)
sendMessage(_, _, [], _) ->
	ok;
sendMessage(MsgName, MsgValue, [Node | Nodes], SourceNode) when SourceNode == Node ->
	sendMessage(MsgName, MsgValue, Nodes, SourceNode);
sendMessage(MsgName, MsgValue, [Node | Nodes], SourceNode) ->
	{server, Node} ! {publish, MsgName, MsgValue, node()},
	sendMessage(MsgName, MsgValue, Nodes, SourceNode).
	
%% Triggers an unsubscription
unsubscribe(Subscription) ->
	server ! {startUnsubscribe, Subscription}.
	
%% Triggers a subscription
subscribe(Subscription) ->
	server ! {startSubscription, Subscription}.
	
%% Triggers the printing of the subscription list (debug only)
printSubscriptions() ->
	server ! printSubscriptions.
	
%% Adds a subscription (if necessary) to the subscription list
addSubscription({NewSub, NewNode}, [], SubscriptionList) ->
	[{NewSub, [NewNode]} | SubscriptionList];
addSubscription({NewSub, NewNode}, [{Sub, Nodes} | OtherSub], SubscriptionList) when NewSub == Sub ->
	IsInList = matches(NewNode, Nodes),
	if
		IsInList == yes ->
			[{Sub, Nodes} | OtherSub] ++ SubscriptionList;
		IsInList == no ->
			[{Sub, [NewNode | Nodes]} | OtherSub] ++ SubscriptionList
	end;
addSubscription(NewSub, [Sub | OtherSub], SubscriptionList) ->
	NewSubscriptionList = [Sub | SubscriptionList],
	addSubscription(NewSub, OtherSub, NewSubscriptionList).
	
%% Propagate the subscription to the parent and the children
propagateSubscription(Parent, Children, Node, Subscription) ->
	if 
		Node == Parent ->
			io:fwrite("Not propagating subscribe to the Parent~n");
		Parent == node() ->
			io:fwrite("Root node, no parent to propagate the subscribe to~n");
		true ->
			io:fwrite("Propagating subscribe to the parent~n"),
			{server, Parent} ! {subscribe, node(), Subscription}
	end,
	propagateSubscriptionToChildren(Children, Node, Subscription).
	
%% Propagate the subscription to all children, except the one that sent it
propagateSubscriptionToChildren([], _, _) ->
	ok;
propagateSubscriptionToChildren([Child | Children], Node, Subscription) when Child == Node ->
	propagateSubscriptionToChildren(Children, Node, Subscription);
propagateSubscriptionToChildren([Child | Children], Node, Subscription) ->
	{server, Child} ! {subscribe, node(), Subscription},
	propagateSubscriptionToChildren(Children, Node, Subscription).
	
%% Propagate the unsubscription to the parent and the children
propagateUnsubscribe(Parent, Children, Node, Subscription) ->
	if
		Node == Parent ->
			io:fwrite("Not propagating unsubscribe to the Parent~n");
		Parent == node() ->
			io:fwrite("Root node, no parent to propagate the unsubscribe to~n");
		true ->
			io:fwrite("Propagating unsubscribe to the parent~n"),
			{server, Parent} ! {unsubscribe, node(), Subscription}
	end,
	propagateUnsubscribeToChildren(Children, Node, Subscription).
	
%% Propagate the unsubscribe to al children, except the one that sent it
propagateUnsubscribeToChildren([], _, _) ->
	ok;
propagateUnsubscribeToChildren([Child | Children], Node, Subscription) when Child == Node ->
	%% The unsubscribe is not sent back to the node from which it has been received
	propagateUnsubscribeToChildren(Children, Node, Subscription);
propagateUnsubscribeToChildren([Child | Children], Node, Subscription) ->
	{server, Child} ! {unsubscribe, node(), Subscription},
	propagateUnsubscribeToChildren(Children, Node, Subscription).
	
%% Removes a node from the subscription list
removeNodeFromSubscription(_, _, _, [], NewSubscriptions) ->
	NewSubscriptions;
removeNodeFromSubscription(Parent, Children, Node, [{SubName, Nodes} | OtherSub], NewSubscriptions) ->
	IsInList = matches(Node, Nodes),
	if
		IsInList == yes ->
			NewNodes = lists:delete(Node, Nodes),
			%% if the new list is empty remove the subscription altogether and propagate the
			%% unsubscribe to the parent and children
			if
				NewNodes == [] ->
					propagateUnsubscribe(Parent, Children, Node, SubName),
					removeNodeFromSubscription(Parent, Children, Node, OtherSub, NewSubscriptions);
				true ->
					removeNodeFromSubscription(Parent, Children, Node, OtherSub, [{SubName, NewNodes} | NewSubscriptions])
			end;
		IsInList == no ->
			removeNodeFromSubscription(Parent, Children, Node, OtherSub, [{SubName, Nodes} | NewSubscriptions])
	end.
	
%% Removes a subscription
removeSubscription(_, _, [], NewSubscriptions) ->
	NewSubscriptions;
removeSubscription(SubToRemove, NodeToRemove, [{Sub, Nodes} | OtherSub], NewSubscriptions) when SubToRemove == Sub ->
	IsInList = matches(NodeToRemove, Nodes),
	if
		IsInList == yes ->
			NewNodes = lists:delete(NodeToRemove, Nodes),
			if
				NewNodes == [] ->
					removeSubscription(SubToRemove, NodeToRemove, OtherSub, NewSubscriptions);
				true ->
					removeSubscription(SubToRemove, NodeToRemove, OtherSub, [{Sub, NewNodes} | NewSubscriptions])
			end;
		IsInList == no ->
			removeSubscription(SubToRemove, NodeToRemove, OtherSub, [{Sub, Nodes} | NewSubscriptions])
	end;
removeSubscription(SubToRemove, NodeToRemove, [Sub | OtherSub], NewSubscriptions) ->
	removeSubscription(SubToRemove, NodeToRemove, OtherSub, [Sub | NewSubscriptions]).
	
%% Prints the subscriptions stored (debug only)
printSubscriptionList([]) ->
	ok;
printSubscriptionList([Subscription | Subscriptions]) ->
	io:fwrite("Subscription: ~w~n", [Subscription]),
	printSubscriptionList(Subscriptions).
	
%% Start a children ping (debug only)
pingChildren() ->
	io:fwrite("Pinging all children...~n"),
	server ! pingChildren.
	
%% Execute a children ping (debug only)
executePingChildren([]) ->
	io:fwrite("Ping complete!~n");
executePingChildren([Child | Children]) ->
	io:fwrite("Pinging ~w ~n", [Child]),
	{server, Child} ! ping,
	executePingChildren(Children).
	
%get only the subscription where the destination isn't Node and with my node in the other
modifySubscriptions([],_,Result) ->
	Result;
modifySubscriptions([{_,[To]}|Other],Node,Result) when To == Node ->
	modifySubscriptions(Other,Node, Result);
modifySubscriptions([{Attr,_}|Other],Node,Result) ->
	modifySubscriptions(Other, Node, [{Attr,[node()]}|Result]).
	
%Merge the content of the 2 subscription list
mergeSubscription([],_,Result,_,_,_)->
    Result;
mergeSubscription([{Attr,[To]}|Subscription1Tail],Subscription2,Result,Parent,Children,NodoMittente)->
    Res=matches({Attr,[To]},Subscription2),
    if 
	Res==no->
	    %%Notifico agli altri che ho aggiunto questa sottoscrizione modificando l'unica sottoscrizione per metterla a me
	    propagateSubscription(Parent,Children,NodoMittente,Attr),
	    mergeSubscription(Subscription1Tail,Subscription2,[{Attr,[To]}|Result],Parent,Children,NodoMittente);
	true->
	    mergeSubscription(Subscription1Tail,Subscription2,Result,Parent,Children,NodoMittente)
    end.

mergeUnsubscription(_,[],Result,_,_,_)->
    Result;
mergeUnsubscription(Subscription2,[{Attr,[To]}|Subscription1Tail],Result,Parent,Children,NodoMittente)->
    if
	%% Confronto solo quelli che ho nella subscription con nodo uguale a quello suo (??VERIFICARE??)
	To==NodoMittente->
	    Res=matches({Attr,[To]},Subscription2),
	    if 
		Res==no->
		    %%Notifico agli altri che ho rimosso questa sottoscrizione
		    propagateUnsubscribe(Parent,Children,NodoMittente,Attr),
		    mergeUnsubscription(Subscription2,Subscription1Tail,Result,Parent,Children,NodoMittente);
		true->
		    mergeUnsubscription(Subscription2,Subscription1Tail,[{Attr,[To]}|Result],Parent,Children,NodoMittente)
	    end;
	true->
	    mergeUnsubscription(Subscription2,Subscription1Tail,[{Attr,[To]}|Result],Parent,Children,NodoMittente)
    end.
%% Searches for an item in a list
%% Maybe we could use lists:member
matches(_, []) ->
	no;
matches(Item, [Head | _]) when Item == Head ->
	yes;
matches(Item, [_ | Rest]) ->
	matches(Item, Rest).