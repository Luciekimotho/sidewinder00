-module(pubsub).
-export([startServer/1, startClient/1, serverLoop/8, pingChildren/0, 
		subscribe/1, printSubscriptions/0, publish/2, closeApp/0,
		unsubscribe/1, printVC/0, crash/0]).

startClient([Parent]) ->
	io:fwrite("Starting client...~n"),
	Pid = spawn(fun() -> serverLoop(Parent, [], [], no, [{node(), 0}], [], [], []) end),
	register(server, Pid),
	io:fwrite("Client started!~n"),
	setParent().

startServer([Parent]) ->
	io:fwrite("Starting server...~n"),
	Pid = spawn(fun() -> serverLoop(Parent, [], [], yes, [], [], [], []) end),
	register(server, Pid),
	io:fwrite("Server started!~n"),
	setParent(),
	on_exit(Pid,Parent),
	io:fwrite("Handler started!~n").
	
restartServer(Parent, Children, Subscriptions) ->
	io:fwrite("Restarting server...after 20 sec~n"),
	timer:sleep(20000),
	io:fwrite("Children are: ~w~n", [Children]),
	io:fwrite("Subscriptions are: ~w~n", [Subscriptions]),
	Pid = spawn(fun() -> serverLoop(Parent, Children, Subscriptions, yes, [], [], [], []) end),
	register(server, Pid),
	io:fwrite("Server restarted!~n"),
	setParent(),
	%% Start the error handler
	on_exit(Pid,Parent),
	io:fwrite("Handler started!~n").

%% error handler
on_exit(Pid,Parent)->
    PidHandler=spawn(fun()->
		process_flag(trap_exit,true),
		link(Pid),
		handlerLoop(Parent,[],[],Pid)
		end),
    Pid!{handler,PidHandler}.

handlerLoop(Parent, Children, Subscription, Pid) ->
    receive 
	{'EXIT', Pid, _} ->
			io:fwrite("Errore nel server, riavvio!~n~n"),
			restartServer(Parent,Children,Subscription);
	{children, NewChildren} ->
	    io:fwrite("Update childern~n"),
	    handlerLoop(Parent,NewChildren,Subscription,Pid);
	{subscription, NewSubscription} ->
	    io:fwrite("Update subscription~n"),
	    handlerLoop(Parent,Children,NewSubscription,Pid);
	normalExit ->
	    io:fwrite("Normal exit~n")
    end.

%% Function to resend the message (subscribe,publish,ecc) if the parent is down
resendMessage(Destinazione, Messaggio) ->
	NewMex = setelement(tuple_size(Messaggio), Messaggio, self()),
	Destinazione ! NewMex,
	receive
	    ack -> ok
	    after 1000 -> 
		io:fwrite("Il padre non risponde, riprovo tra 3 secondi...~n"),
		timer:sleep(3000),
		resendMessage(Destinazione, Messaggio)
	end.

serverLoop(Parent, Children, Subscriptions, IsServer, VC, MessagesOnHold, UpdateVCOnHold, PidHandler) ->
	receive
		%% Get the handler pid
		{handler, Handler}->
			    io:format("~s ~p ~n",["Ricevuto PID Handler:", Handler]),
			    serverLoop(Parent, Children, Subscriptions, IsServer, VC, MessagesOnHold, UpdateVCOnHold, Handler);
		
		%% Sends a setChildren message to the parent node
		setParent ->
			if 
				Parent =/= node() ->
					spawn(fun() -> resendMessage({server, Parent}, {setChildren, node(), VC, pidPlaceholder}) end),
					serverLoop(Parent, Children, Subscriptions, IsServer, VC, MessagesOnHold, UpdateVCOnHold, PidHandler);
				true ->
					io:fwrite("Root node, no parent added~n"),
					serverLoop(Parent, Children, Subscriptions, IsServer, VC, MessagesOnHold, UpdateVCOnHold, PidHandler)
			end;
			
		%% Reply to the setChildren message
		{setParentAck, ParentVC} ->
			io:fwrite("Parent attached!~n"),
			io:fwrite("Received VC from parent: ~w~n", [ParentVC]),
			UpdatedVC = computeNewVCServer(ParentVC, VC, []),
			io:fwrite("New VC is ~w~n", [UpdatedVC]),
			serverLoop(Parent, Children, Subscriptions, IsServer, UpdatedVC, MessagesOnHold, UpdateVCOnHold, PidHandler);
			
		setParentAbort ->
			io:fwrite("Cannot attach to a Client node!~n"),
			PidHandler ! normalExit,
			io:fwrite("Closing application~n");
			
		%% Adds a children to the children list
		{setChildren, Child, ChildVC, PidAck} ->
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
					{server, Child} ! {setParentAck, VC},
					PidHandler ! {children, NewChildren},
					PidAck ! ack,
					UpdatedVC = computeNewVCServer(ChildVC, VC, []),
					io:fwrite("New VC after adding children is ~w~n", [UpdatedVC]),
					if
						UpdatedVC == [] ->
							io:fwrite("No need to propagate the VC~n");
						true ->
							io:fwrite("Propagating the new VC~n"),
							propagateUpdateVC(Parent, Children, Child, Child, UpdatedVC)
					end,
					propagateSubscriptions(Child, Subscriptions),
					serverLoop(Parent, NewChildren, Subscriptions, IsServer, UpdatedVC, MessagesOnHold, UpdateVCOnHold, PidHandler);
				IsServer == no ->
					io:fwrite("Clients cannot have children!~n"),
					io:fwrite("Sending abort message!~n"),
					{server, Child} ! setParentAbort,
					PidAck ! ack,
					serverLoop(Parent, Children, Subscriptions, IsServer, VC, MessagesOnHold, UpdateVCOnHold, PidHandler)
			end;
			
		%% Start publishing a message
		{startPublish, Name, Value} ->
			io:fwrite("Publishing ~w~n", [{Name, Value}]),
			NewVC = increaseSelfVC(VC, []),
			spawn(fun() -> resendMessage({server, Parent},{publish, Name, Value, node(), node(), NewVC, pidPlaceholder}) end),
			serverLoop(Parent, Children, Subscriptions, IsServer, NewVC, MessagesOnHold, UpdateVCOnHold, PidHandler);
			
		%% Publish a new message
		{publish, Name, Value, SourceNode, PrevHop, MessageVC, PidAck} ->
			if
				IsServer == yes ->
					io:fwrite("Relaying message...~n"),
					UpdatedVC = computeNewVCServer(MessageVC, VC, []),
					relayMessage(Name, Value, Subscriptions, SourceNode, PrevHop, MessageVC, Parent, Children),
					PidAck ! ack,
					serverLoop(Parent, Children, Subscriptions, IsServer, UpdatedVC, MessagesOnHold, UpdateVCOnHold, PidHandler);
				IsServer == no ->
					UpdatedVC = checkMessage(SourceNode, MessageVC, VC),
					if
						UpdatedVC == false ->
							NewMessagesOnHold = [{SourceNode, Name, Value, MessageVC} | MessagesOnHold],
							io:fwrite("Message added to MessagesOnHold (~w)~n", [NewMessagesOnHold]),
							PidAck ! ack,
							serverLoop(Parent, Children, Subscriptions, IsServer, VC, NewMessagesOnHold, UpdateVCOnHold, PidHandler);
						true ->
							io:fwrite("Message received: ~w with VC ~w from ~w~n", [{Name, Value}, MessageVC, SourceNode]),
							{FinalUpdatedVC, NewMessageOnHold, NewUpdateVCOnHOld} = checkOnHoldLists(UpdatedVC, MessagesOnHold, UpdateVCOnHold),
							io:fwrite("VC after on-hold messages check: ~w~n", [FinalUpdatedVC]),
							PidAck ! ack,
							serverLoop(Parent, Children, Subscriptions, IsServer, FinalUpdatedVC, NewMessageOnHold, NewUpdateVCOnHOld, PidHandler)
					end
			end;
					
		%% Subscribes (client mode)
		{startSubscription, Subscription} ->
			io:fwrite("Subscribing (user initiated)~n"),
			spawn(fun() -> resendMessage({server, Parent},{subscribe, node(), Subscription, pidPlaceholder}) end),
			serverLoop(Parent, Children, Subscriptions, IsServer, VC, MessagesOnHold, UpdateVCOnHold, PidHandler);
			
		%% Handles a subscription request (server mode)
		{subscribe, Node, Subscription, PidAck} ->
			if
				IsServer == yes ->
					io:fwrite("Subscription request received...~n"),
					propagateSubscription(Parent, Children, Node, Subscription),
					NewSubscriptions = addSubscription({Subscription, Node}, Subscriptions, []),
					PidHandler ! {subscription,NewSubscriptions},
					PidAck ! ack,
					serverLoop(Parent, Children, NewSubscriptions, IsServer, VC, MessagesOnHold, UpdateVCOnHold, PidHandler);
				IsServer == no ->
					PidAck ! ack,
					serverLoop(Parent, Children, Subscriptions, IsServer, VC, MessagesOnHold, UpdateVCOnHold, PidHandler)
			end;
			
		%% Unsubscribes (client mode)
		{startUnsubscribe, Subscription} ->
			io:fwrite("Unsubscribe (user initiated)~n"),
			spawn(fun() -> resendMessage({server, Parent},{unsubscribe, node(), Subscription, pidPlaceholder}) end),
			serverLoop(Parent, Children, Subscriptions, IsServer, VC, MessagesOnHold, UpdateVCOnHold, PidHandler);
			
		%% Handles an unsubscription request (server mode)
		{unsubscribe, Node, Subscription, PidAck} ->
			if
				IsServer == yes ->
					io:fwrite("Unsubscribe request received...~n"),
					propagateUnsubscribe(Parent, Children, Node, Subscription),
					NewSubscriptions = removeSubscription(Subscription, Node, Subscriptions, []),
					PidHandler ! {subscription, NewSubscriptions},
					PidAck ! ack,
					serverLoop(Parent, Children, NewSubscriptions, IsServer, VC, MessagesOnHold, UpdateVCOnHold, PidHandler);
				IsServer == no ->
					PidAck ! ack,
					serverLoop(Parent, Children, Subscriptions, IsServer, VC, MessagesOnHold, UpdateVCOnHold, PidHandler)
			end;
		
		%% Prints the current subscription list
		printSubscriptions ->
			io:fwrite("Printing subscription list:~n"),
			printSubscriptionList(Subscriptions),
			serverLoop(Parent, Children, Subscriptions, IsServer, VC, MessagesOnHold, UpdateVCOnHold, PidHandler);
			
		%% Updates the VC
		{updateVC, PrevHop, SourceNode, NewVC, PidAck} ->
			io:fwrite("Updating Vector Clock...~n"),
			if
				IsServer == yes ->
					propagateUpdateVC(Parent, Children, PrevHop, SourceNode, NewVC),
					UpdatedVC = computeNewVCServer(NewVC, VC, []),
					io:fwrite("Server: old VC is ~w, received ~w, new VC is ~w~n", [VC, NewVC, UpdatedVC]),
					PidAck ! ack,
					serverLoop(Parent, Children, Subscriptions, IsServer, UpdatedVC, MessagesOnHold, UpdateVCOnHold, PidHandler);
				IsServer == no ->
					UpdatedVC = updateNewVCEntriesClient(NewVC, VC),
					NewUpdatedVC = checkUpdateVC(SourceNode, NewVC, VC),
					if
						NewUpdatedVC == false ->
							NewUpdateVCOnHold = [{SourceNode, NewVC} | UpdateVCOnHold],
							io:fwrite("UpdateVC added to UpdateVCOnHold (~w)~n", [NewUpdateVCOnHold]),
							PidAck ! ack,
							serverLoop(Parent, Children, Subscriptions, IsServer, UpdatedVC, MessagesOnHold, NewUpdateVCOnHold, PidHandler);
						true ->
							{FinalUpdatedVC, NewMessageOnHold, NewUpdateVCOnHOld} = checkOnHoldLists(NewUpdatedVC, MessagesOnHold, UpdateVCOnHold),
							io:fwrite("VC after on-hold messages check: ~w~n", [FinalUpdatedVC]),
							PidAck ! ack,
							serverLoop(Parent, Children, Subscriptions, IsServer, FinalUpdatedVC, NewMessageOnHold, NewUpdateVCOnHOld, PidHandler)
					end
			end;
			
		%% Prints the current VC
		printVC ->
			io:fwrite("Current VC is ~w~n", [VC]),
			serverLoop(Parent, Children, Subscriptions, IsServer, VC, MessagesOnHold, UpdateVCOnHold, PidHandler);
		
		%% Start pinging the children	
		pingChildren ->
			executePingChildren(Children),
			serverLoop(Parent, Children, Subscriptions, IsServer, VC, MessagesOnHold, UpdateVCOnHold, PidHandler);
			
		%% Ping message received
		ping ->
			io:fwrite("Ping received!~n"),
			serverLoop(Parent, Children, Subscriptions, IsServer, VC, MessagesOnHold, UpdateVCOnHold, PidHandler);
			
		%% Crash message received
		crash ->
			2*f;
			
		%% Removes a children when it exits
		{removeChildren, NodeToRemove, PrevHop, PidAck} ->
			if
				IsServer == yes ->
					io:fwrite("Node ~w has exited, removing it (if necessary)...~n", [NodeToRemove]),
					NewChildren = lists:delete(NodeToRemove, Children),
					PidHandler ! {children, NewChildren},
					NewSubscriptions = removeNodeFromSubscription(Parent, NewChildren, NodeToRemove, Subscriptions, []),
					PidHandler ! {subscription, NewSubscriptions},
					UpdatedVC = removeNodeFromVC(NodeToRemove, VC, []),
					propagateRemoveChildren(Parent, Children, PrevHop, NodeToRemove),
					PidAck ! ack,
					serverLoop(Parent, NewChildren, NewSubscriptions, IsServer, UpdatedVC, MessagesOnHold, UpdateVCOnHold, PidHandler);
				IsServer == no ->
					io:fwrite("Node ~w has exited, removing it (if necessary)...~n", [NodeToRemove]),
					UpdatedVC = removeNodeFromVC(NodeToRemove, VC, []),
					PidAck ! ack,
					serverLoop(Parent, Children, Subscriptions, IsServer, UpdatedVC, MessagesOnHold, UpdateVCOnHold, PidHandler)
			end;
		
		%% Terminates the application (if possible)
		exit ->
			if
				Children == [] ->
					if 
					    Parent =/= node() ->
							spawn(fun() -> resendMessage({server, Parent}, {removeChildren, node(), node(), pidPlaceholder}) end);
					    true ->
							ok
					end,
					if 
					    IsServer == yes ->
							PidHandler ! normalExit;
					    true ->
							ok
					end,
					io:fwrite("Application closed!~n");
				true ->
					io:fwrite("Unable to exit, there still are children attached!~n"),
					serverLoop(Parent, Children, Subscriptions, IsServer, VC, MessagesOnHold, UpdateVCOnHold, PidHandler)
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
	
%% Triggers the printing of the current VC
printVC() ->
	server ! printVC.
	
propagateSubscriptions(_, []) ->
	ok;
propagateSubscriptions(Child, [{SubName, _} | Subscriptions]) ->
	spawn(fun() -> resendMessage({server, Child},{subscribe, node(), SubName, pidPlaceholder}) end),
	propagateSubscriptions(Child, Subscriptions).
	
%% Relays the message to all the subscribed nodes
relayMessage(_, _, [], SourceNode, PrevHop, VC, Parent, Children) ->
	propagateUpdateVC(Parent, Children, PrevHop, SourceNode, VC);
relayMessage(MsgName, MsgValue, [{Name, Nodes} | _], SourceNode, PrevHop, VC, Parent, Children) when MsgName == Name ->
	propagateUpdateVCRelay(Parent, Children, PrevHop, SourceNode, VC, Nodes),
	sendMessage(MsgName, MsgValue, Nodes, SourceNode, PrevHop, VC);
relayMessage(MsgName, MsgValue, [_ | OtherSub], SourceNode, PrevHop, VC, Parent, Children) ->
	relayMessage(MsgName, MsgValue, OtherSub, SourceNode, PrevHop, VC, Parent, Children).
		
%% Propagates the updateVC to the parent and the children	
propagateUpdateVCRelay(Parent, Children, PrevHop, Source, VC, NodesSent) ->
	ChildrenToSendUpdateVC = lists:subtract(Children, NodesSent),
	SentToParent = lists:member(Parent, NodesSent),
	if
		SentToParent == true ->
			io:fwrite("Message already sent to parent, not propagating updateVC~n");
		PrevHop == Parent ->
			io:fwrite("Not propagating updateVC to the Parent~n");
		Parent == node() ->
			io:fwrite("Root node, no parent to propagate the updateVC to~n");
		true ->
			io:fwrite("Propagating the updateVC to the parent~n"),
			spawn(fun() -> resendMessage({server, Parent}, {updateVC, node(), Source, VC, pidPlaceholder}) end)
	end,
	propagateUpdateVCToChildren(ChildrenToSendUpdateVC, PrevHop, Source, VC).
	
%% Sends the message to all the subscribed nodes (except the node where the message came from)
sendMessage(_, _, [], _, _, _) ->
	ok;
sendMessage(MsgName, MsgValue, [Node | Nodes], SourceNode, PrevHop, VC) when PrevHop == Node ->
	sendMessage(MsgName, MsgValue, Nodes, SourceNode, PrevHop, VC);
sendMessage(MsgName, MsgValue, [Node | Nodes], SourceNode, PrevHop, VC) ->
	spawn(fun() -> resendMessage({server, Node}, {publish, MsgName, MsgValue, SourceNode, node(), VC, pidPlaceholder}) end),
	sendMessage(MsgName, MsgValue, Nodes, SourceNode, PrevHop, VC).
	
%% Triggers an unsubscription
unsubscribe(Subscription) ->
	server ! {startUnsubscribe, Subscription}.
	
%% Triggers a subscription
subscribe(Subscription) ->
	server ! {startSubscription, Subscription}.
	
%% Triggers the printing of the subscription list (debug only)
printSubscriptions() ->
	server ! printSubscriptions.
	
%% Removes an entry corresponding to a Node from the VC
removeNodeFromVC(_, [], NewVC) ->
	NewVC;
removeNodeFromVC(Node, [{VCNode, VCValue} | OtherVC], NewVC) when VCNode =/= Node->
	removeNodeFromVC(Node, OtherVC, [{VCNode, VCValue} | NewVC]);
removeNodeFromVC(Node, [_ | OtherVC], NewVC) ->
	removeNodeFromVC(Node, OtherVC, NewVC).

%% Checks if the on-hold lists contain messages that can be released
checkOnHoldLists(VC, MessageOnHoldList, UpdateVCOnHoldList) ->
	ReleasedMessageOnHold = navigateMessageOnHoldList(VC, UpdateVCOnHoldList, MessageOnHoldList, []),
	ReleasedUpdateOnHold = navigateUpdateOnHoldList(VC, MessageOnHoldList, MessageOnHoldList, []),
	if
		ReleasedMessageOnHold =/= false ->
			ReleasedMessageOnHold;
		ReleasedUpdateOnHold =/= false ->
			ReleasedUpdateOnHold;
		true ->
			{VC, MessageOnHoldList, UpdateVCOnHoldList}
	end.
	
%% Checks and releases messages from the MessageOnHoldList
navigateMessageOnHoldList(_, _, [], _) ->
	false;
navigateMessageOnHoldList(VC, UpdateVCOnHoldList, [MessageOnHold | OtherMessageOnHoldList], OutputMessageOnHoldList) ->
	{SourceNode, Name, Value, MessageVC} = MessageOnHold,
	UpdatedVC = checkMessage(SourceNode, VC, MessageVC),
	if
		UpdatedVC == false ->
			navigateMessageOnHoldList(VC, UpdateVCOnHoldList, OtherMessageOnHoldList, [MessageOnHold | OutputMessageOnHoldList]);
		true ->
			io:fwrite("Message received: ~w with VC ~w from ~w~n", [{Name, Value}, MessageVC, SourceNode]),
			NewOutputMessageOnHoldList = OtherMessageOnHoldList ++ OutputMessageOnHoldList,
			checkOnHoldLists(UpdatedVC, NewOutputMessageOnHoldList, UpdateVCOnHoldList)
	end.
	
%% Checks and releases messages from the UpdateOnHoldList
navigateUpdateOnHoldList(_, _, [], _) ->
	false;
navigateUpdateOnHoldList(VC, MessageOnHoldList, [UpdateOnHold | OtherUpdateOnHoldList], OutputUpdateOnHoldList) ->
	{SourceNode, NewVC} = UpdateOnHold,
	UpdateVC = checkUpdateVC(SourceNode, NewVC, VC),
	if
		UpdateVC == false ->
			navigateUpdateOnHoldList(VC, MessageOnHoldList, OtherUpdateOnHoldList, [UpdateOnHold | OutputUpdateOnHoldList]);
		true ->
			io:fwrite("updateVC on hold released ~w~n", NewVC),
			NewOutputUpdateOnHoldList = OtherUpdateOnHoldList ++ OutputUpdateOnHoldList,
			checkOnHoldLists(UpdateVC, MessageOnHoldList, NewOutputUpdateOnHoldList)
	end.

%% Check if we can release the message or if it has to be stored for a
%% future release	
checkMessage(SourceNode, MessageVC, VC) ->
	IsSubsequent = checkIfSubsequentVC(SourceNode, VC, MessageVC, no),
	if
		IsSubsequent == yes ->
			io:fwrite("The 2 VCs are subsequent (publish)~n"),
			UpdatedVC = computeNewVCClient(MessageVC, VC, []);
		true ->
			io:fwrite("The 2 VCs are not subsequent (publish)~n"),
			UpdatedVC = false
	end,
	UpdatedVC.
	
%% Check if we can update the VC or if we must store the updateVC for a
%% future release	
checkUpdateVC(SourceNode, NewVC, VC) ->
	IsSubsequent = checkIfSubsequentVC(SourceNode, VC, NewVC, yes),
	if
		IsSubsequent == yes ->
			io:fwrite("The 2 VCs are subsequent (updateVC)~n"),
			UpdatedVC = computeNewVCClient(NewVC, VC, []);
		true ->
			io:fwrite("The 2 VCs are not subsequent (updateVC)~n"),
			UpdatedVC = false
	end,
	UpdatedVC.
	
%% Increases the node counter entry in the VC
increaseSelfVC([], IncreasedVC) ->
	IncreasedVC;
increaseSelfVC([{VCNode, VCValue} | OtherVC], IncreasedVC) when VCNode == node() ->
	NewIncreasedVC = [{VCNode, VCValue + 1} | IncreasedVC],
	increaseSelfVC(OtherVC, NewIncreasedVC);
increaseSelfVC([VCElement | OtherVC], IncreasedVC) ->
	increaseSelfVC(OtherVC, [VCElement | IncreasedVC]).
	
%% Checks if the two vector clocks are subsequent or not
checkIfSubsequentVC(SourceNode, NodeVC, ReceivedVC, Equal) ->
	NodeVCResult = lists:keysearch(SourceNode, 1, NodeVC),
	ReceivedVCResult = lists:keysearch(SourceNode, 1, ReceivedVC),
	if
		(NodeVCResult == false) and (ReceivedVCResult =/= false) ->
			io:fwrite("NodeVC does not contain an element. Checking if this corresponds with a new node...~n"),
			{_, {_, ReceivedVCValue}} = ReceivedVCResult,
			if
				(ReceivedVCValue == 1) and (Equal == no) ->
					checkIfSubsequentVCSecondary(SourceNode, NodeVC, ReceivedVC);
				(ReceivedVCValue == 0) and (Equal == yes) ->
					checkIfSubsequentVCSecondary(SourceNode, NodeVC, ReceivedVC);
				true ->
					no
			end;
		ReceivedVCResult == false ->
			io:fwrite("Received an updateVC generated by a server addition!~n"),
			yes;
		true ->
			{_, {_, NodeVCValue}} = NodeVCResult,
			{_, {_, ReceivedVCValue}} = ReceivedVCResult,
			if
				(ReceivedVCValue == NodeVCValue + 1) and (Equal == no) ->
					io:fwrite("The SourceNode elements in the VCs are ok~n"),
					checkIfSubsequentVCSecondary(SourceNode, NodeVC, ReceivedVC);
				(ReceivedVCValue >= NodeVCValue) and (Equal == yes) ->
					io:fwrite("The SourceNode elements in the VCs are ok~n"),
					checkIfSubsequentVCSecondary(SourceNode, NodeVC, ReceivedVC);
				true ->
					io:fwrite("The SourceNode elements in the VCs are ok~n"),
					no
			end
	end.
	
%% Helper function for checkIfSubsequentVC
checkIfSubsequentVCSecondary(_, [], _) ->
	yes;
checkIfSubsequentVCSecondary(SourceNode, [{NodeVCElement, NodeVCValue} | OtherNodeVC], ReceivedVC) when NodeVCElement =/= SourceNode ->
	ReceivedVCResult = lists:keysearch(NodeVCElement, 1, ReceivedVC),
	if
		ReceivedVCResult == false ->
			checkIfSubsequentVCSecondary(SourceNode, OtherNodeVC, ReceivedVC);
		true ->
			{_, {_, ReceivedVCValue}} = ReceivedVCResult,
			if
				ReceivedVCValue > NodeVCValue ->
					no;
				true ->
					checkIfSubsequentVCSecondary(SourceNode, OtherNodeVC, ReceivedVC)
			end
	end;
checkIfSubsequentVCSecondary(SourceNode, [_ | OtherNodeVC], ReceivedVC) ->
	checkIfSubsequentVCSecondary(SourceNode, OtherNodeVC, ReceivedVC).
	
%% Adds the new entries (counter == 0) in the Client VC
updateNewVCEntriesClient([], VC) ->
	VC;
updateNewVCEntriesClient([{NewVCNode, NewVCValue} | OtherNewVC], VC) when NewVCValue == 0 ->
	SearchResult = lists:keysearch(NewVCNode, 1, VC),
	if
		SearchResult == false ->
			UpdatedVC = [{NewVCNode, NewVCValue} | VC];
		true ->
			UpdatedVC = VC
	end,
	updateNewVCEntriesClient(OtherNewVC, UpdatedVC);
updateNewVCEntriesClient([_ | OtherNewVC], VC) ->
	updateNewVCEntriesClient(OtherNewVC, VC).
	
%% Computes a new Virtual Clock for the client
computeNewVCClient([], [], UpdatedVC) ->
	UpdatedVC;
computeNewVCClient([], [{VCNode, VCValue} | OtherVC], UpdatedVC) ->
	SearchResult = lists:keysearch(VCNode, 1, UpdatedVC),
	if
		SearchResult == false ->
			NewUpdatedVC = [{VCNode, VCValue} | UpdatedVC];
		true ->
			NewUpdatedVC = UpdatedVC
	end,
	computeNewVCClient([], OtherVC, NewUpdatedVC);
computeNewVCClient([{NewVCNode, NewVCValue} | OtherNewVC], VC, UpdatedVC) when NewVCNode =/= node() ->
	SearchResult = lists:keysearch(NewVCNode, 1, VC),
	if
		SearchResult == false ->
			NewUpdatedVC = [{NewVCNode, NewVCValue} | UpdatedVC];
		true ->
			{_, VCElement} = SearchResult,
			{_, VCValue} = VCElement,
			if
				VCValue > NewVCValue ->
					NewUpdatedVC = [VCElement | UpdatedVC];
				true ->
					NewUpdatedVC = [{NewVCNode, NewVCValue} | UpdatedVC]
			end
	end,
	computeNewVCClient(OtherNewVC, VC, NewUpdatedVC);
computeNewVCClient([_ | OtherNewVC], VC, UpdatedVC) ->
	computeNewVCClient(OtherNewVC, VC, UpdatedVC).
	
%% Computes a new Virtual Clock for the server
computeNewVCServer([], [], UpdatedVC) ->
	UpdatedVC;
computeNewVCServer([], [{VCNode, VCValue} | OtherVC], UpdatedVC) ->
	SearchResult = lists:keysearch(VCNode, 1, UpdatedVC),
	if
		SearchResult == false ->
			NewUpdatedVC = [{VCNode, VCValue} | UpdatedVC];
		true ->
			NewUpdatedVC = UpdatedVC
	end,
	computeNewVCServer([], OtherVC, NewUpdatedVC);
computeNewVCServer([{NewVCNode, NewVCValue} | OtherNewVC], VC, UpdatedVC) ->
	SearchResult = lists:keysearch(NewVCNode, 1, VC),
	if
		SearchResult == false ->
			NewUpdatedVC = [{NewVCNode, NewVCValue} | UpdatedVC];
		true ->
			{_, VCElement} = SearchResult,
			{_, VCValue} = VCElement,
			if
				VCValue > NewVCValue ->
					NewUpdatedVC = [VCElement | UpdatedVC];
				true ->
					NewUpdatedVC = [{NewVCNode, NewVCValue} | UpdatedVC]
			end
	end,
	computeNewVCServer(OtherNewVC, VC, NewUpdatedVC).

%% Propagates the updateVC to the parent and the children	
propagateUpdateVC(Parent, Children, PrevHop, Source, VC) ->
	if
		PrevHop == Parent ->
			io:fwrite("Not propagating updateVC to the Parent~n");
		Parent == node() ->
			io:fwrite("Root node, no parent to propagate the updateVC to~n");
		true ->
			io:fwrite("Propagating the updateVC to the parent~n"),
			spawn(fun() -> resendMessage({server, Parent}, {updateVC, node(), Source, VC, pidPlaceholder}) end)
	end,
	propagateUpdateVCToChildren(Children, PrevHop, Source, VC).
	
%% Propagates the updateVC to all children, except the one that sent it
propagateUpdateVCToChildren([], _, _, _) ->
	ok;
propagateUpdateVCToChildren([Child | Children], PrevHop, Source, VC) when Child == PrevHop ->
	propagateUpdateVCToChildren(Children, PrevHop, Source, VC);
propagateUpdateVCToChildren([Child | Children], PrevHop, Source, VC) ->
	io:fwrite("Propagating the updateVC to the ~w~n", [Child]),
	spawn(fun() -> resendMessage({server, Child}, {updateVC, node(), Source, VC, pidPlaceholder}) end),
	propagateUpdateVCToChildren(Children, PrevHop, Source, VC).
	
%% Propagates the removeChildren to the parent and children
propagateRemoveChildren(Parent, Children, PrevHop, NodeToRemove) ->
	if
		PrevHop == Parent ->
			io:fwrite("Not propagating removeChildren to the Parent~n");
		Parent == node() ->
			io:fwrite("Root node, no parent to propagate the removeChildren to~n");
		true ->
			io:fwrite("Propagating the removeChildren to the parent~n"),
			spawn(fun() -> resendMessage({server, Parent}, {removeChildren, NodeToRemove, node(), pidPlaceholder}) end)
	end,
	propagateRemoveChildrenToChildren(Children, PrevHop, NodeToRemove).
	
%% Propagates the removeChildren to all children, except the one that sent it
propagateRemoveChildrenToChildren([], _, _) ->
	ok;
propagateRemoveChildrenToChildren([Child | Children], PrevHop, NodeToRemove) when Child == PrevHop ->
	propagateRemoveChildrenToChildren(Children, PrevHop, NodeToRemove);
propagateRemoveChildrenToChildren([Child | Children], PrevHop, NodeToRemove) ->
	spawn(fun() -> resendMessage({server, Child}, {removeChildren, NodeToRemove, node(), pidPlaceholder}) end),
	propagateRemoveChildrenToChildren(Children, PrevHop, NodeToRemove).
	
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
			spawn(fun() -> resendMessage({server, Parent},{subscribe, node(), Subscription, pidPlaceholder}) end)
	end,
	propagateSubscriptionToChildren(Children, Node, Subscription).
	
%% Propagate the subscription to all children, except the one that sent it
propagateSubscriptionToChildren([], _, _) ->
	ok;
propagateSubscriptionToChildren([Child | Children], Node, Subscription) when Child == Node ->
	propagateSubscriptionToChildren(Children, Node, Subscription);
propagateSubscriptionToChildren([Child | Children], Node, Subscription) ->
	spawn(fun() -> resendMessage({server, Child},{subscribe, node(), Subscription, pidPlaceholder}) end),
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
			spawn(fun() -> resendMessage({server, Parent},{unsubscribe, node(), Subscription, pidPlaceholder}) end)
	end,
	propagateUnsubscribeToChildren(Children, Node, Subscription).
	
%% Propagate the unsubscribe to al children, except the one that sent it
propagateUnsubscribeToChildren([], _, _) ->
	ok;
propagateUnsubscribeToChildren([Child | Children], Node, Subscription) when Child == Node ->
	%% The unsubscribe is not sent back to the node from which it has been received
	propagateUnsubscribeToChildren(Children, Node, Subscription);
propagateUnsubscribeToChildren([Child | Children], Node, Subscription) ->
	spawn(fun() -> resendMessage({server, Child},{unsubscribe, node(), Subscription, pidPlaceholder}) end),
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
		
%% Searches for an item in a list
%% Maybe we could use lists:member
matches(_, []) ->
	no;
matches(Item, [Head | _]) when Item == Head ->
	yes;
matches(Item, [_ | Rest]) ->
	matches(Item, Rest).