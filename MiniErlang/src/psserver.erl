%% Author: Andrea Matti
%% Created: Feb 10, 2010
%% Description: server.
%%				Publishing message format: {Attribute,Quantity,Value,ClientName,ClientNode} (String,String,Int,String,String)
%%				For semplicity client random generation handles these: Attribute can be [Environmental, Physics, Performances], Quantity can be [Luminance, Temperature, Humidity, Speed, Size, Weight, Workload]
%%				Subscribing message format: {Fun,[{Name1,Node1},.....,{Namen,Noden}]} the Path that for a client is empty (for message compliance with server code)
%%				Fun format: fun(Message)-> true|false (accept publish Message as argument; return true or false)
%%				Subscription rules are handled in OR among them. AND logic is handled by Fun internally.
%% 
-module(psserver).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/1,start/4, start/5,compactList/1,removeLastItem/1,filterEmptyPathRules/1,fetchChildrenList/4,match/2]).

%%
%% API Functions
%%

%%
%% Start/1, Start/3, Start/4 just call Start/5
%%
start(ServerName) ->
	start(ServerName,null,null,[],[]).

start(ServerName,ParentName,ParentNode) ->
	start(ServerName,ParentName,ParentNode,[],[]).

start(ServerName,ParentName,ParentNode,Children) ->
	start(ServerName,ParentName,ParentNode,Children,[]).

%%
%% Start/5
%% Start a server. Register the atom ServerName and set its parent server. Prepare a dict Environment e pass it to loop
%%
%% Data structures
%% Environment: mantains several server state info (Process Name,Parent Info,Monitor,ecc...)
%% RuleList (tuple list [{Fun,Path}, ... ]) : matching/routing table wher Fun is a fun(PublishMessage)->true|false object and Path is a list of tuples [{PName,PNode}, ... ]
%% ChildList (tuple list [{PName,PNode}, ... ]): child nodes, only needed in order to check if a server is a leaf
%% ClientList (tuple list [{PName,PNode}, ... ]): attached client nodes, only needed in order to notify clients when leaf server goes down
%%
start(ServerName,ParentName,ParentNode,Children,Clients)->
    register(ServerName,self()),
	io:format("~s ~s~n",[ServerName, "started!"]),
	NewEnv=dict:from_list([{pname,ServerName},{parentname,ParentName},{parentnode,ParentNode},{monitorname,monitor},{monitornode,monitor@ubuntu904desktop}]),
	%% Entering into the loop
	loop(NewEnv,Children,[],Clients),
	%%Exit from loop. Assume pname is not changed.
	unregister(dict:fetch(pname,NewEnv)),
	io:format("~s terminated!~n",[dict:fetch(pname,NewEnv)]).

%%
%% Wait for messages.
%% Status changes: stop,setparent,addchild,delchild,addrule,delrule,
%% Subscriptions or publishing)
%%
loop(Environment,ChildList,RuleList,ClientList)->
	PName=dict:fetch(pname, Environment),
	PNode=node(),
	ParentName=dict:fetch(parentname, Environment),
	ParentNode=dict:fetch(parentnode, Environment),
	MonitorName=dict:fetch(monitorname, Environment),
	MonitorNode=dict:fetch(monitornode, Environment),
	receive
		%% Get server printing some status info to screen (Environment, ChildList,RuleList,ClientList) so can be used from any client console without receive message loop
		{FromPid,FromNode,getenv,Message} ->
			%%{FromPid,FromNode}!{PName,PNode,getenv,Message,Environment},
			io:format("~s environment: ~w~n",[PName,Environment]),
			catch {MonitorName,MonitorNode}!{PName,PNode,message,"environment sent!"},
			loop(Environment,ChildList,RuleList,ClientList);
		{FromPid,FromNode,getchildlist,Message} ->
			%%{FromPid,FromNode}!{PName,PNode,getchildlist,Message,ChildList},
			io:format("~s childlist: ~w~n",[PName,ChildList]),
			catch {MonitorName,MonitorNode}!{PName,PNode,message,"children list sent!"},
			loop(Environment,ChildList,RuleList,ClientList);
		{FromPid,FromNode,getrulelist,Message} ->
			%%{FromPid,FromNode}!{PName,PNode,getrulelist,Message,RuleList},
			io:format("~s rulelist: ~w~n",[PName,RuleList]),
			catch {MonitorName,MonitorNode}!{PName,PNode,message,"rules list sent!"},
			loop(Environment,ChildList,RuleList,ClientList);
		{FromPid,FromNode,getclientlist,Message} ->
			%%{FromPid,FromNode}!{PName,PNode,getclientlist,Message,ClientList},
			io:format("~s clientlist ~w~n",[PName,ClientList]),
			catch {MonitorName,MonitorNode}!{PName,PNode,message,"clients list sent!"},
			loop(Environment,ChildList,RuleList,ClientList);
		{FromPid,FromNode,isleaf,Message} ->
			%%{FromPid,FromNode}!{PName,PNode,isleaf,Message,length(ChildList)}, %%!0=false;0=true
			io:format("~s isleaf = ~w~n",[PName,length(ChildList)]),
			catch {MonitorName,MonitorNode}!{PName,PNode,message,"isleaf sent!"},
			loop(Environment,ChildList,RuleList,ClientList);
		%% SetParent. It's up to programmers remove child from old Parent if not null
		{FromPid,FromNode,setParent,PrntName,PrntNode} -> 
			if %% Per poter settare il parent di un server: da null non occorre controllo, deve essere un nodo foglia se ha un old parent 
				ParentName==null or (length(ChildList)==0 and length(ClientList==0)) ->
						NewEnv=dict:store(parentname,PrntName,Environment),
						NewEnv1=dict:store(parentnode,PrntNode,NewEnv),
						%%{FromPid,FromNode}!{PName,PNode,setparent,lists:concat(["parent changed to ", PrntName," ",PrntNode])},
						io:format("~s parent changed to ~w ~s~n",[PName,PrntName,PrntNode]),
						catch {MonitorName,MonitorNode}!{PName,PNode,message,lists:concat(["parent changed to ", PrntName," ",PrntNode])};
				true -> 
					NewEnv1=Environment,
					io:format("~s access denied! Old parent is not null and this server is not a leaf. Old parent ~w ~s~n",[PName,PrntName,PrntNode]),
					catch {MonitorName,MonitorNode}!{PName,PNode,message,lists:concat(["access denied! Old parent is not null and this server is not a leaf. Old parent ", PrntName," ",PrntNode])}
			end,
			loop(NewEnv1,ChildList,RuleList,ClientList);
		%% AddChild. It's up to programmers set the Parent of the child too.
		{FromPid,FromNode,addchild,ChildName,ChildNode} -> 
			NewChildList=lists:append(ChildList,[{ChildName,ChildNode}]),
			%%{FromPid,FromNode}!{PName,PNode,addchild,lists:concat(["child added! ", ChildName," ",ChildNode])},
			io:format("~s child added ~w ~s~n",[PName,ChildName,ChildNode]),
			catch {MonitorName,MonitorNode}!{PName,PNode,message,lists:concat(["child added ", ChildName," ",ChildNode])},
			loop(Environment,NewChildList,RuleList,ClientList);
		%% DelChild. It's up to programmers set the Parent of the child to null or to another parent.
		{FromPid,FromNode,delchild,ChildName,ChildNode} -> 
			NewChildList=lists:delete({ChildName,ChildNode},ChildList),
			%%{FromPid,FromNode}!{PName,PNode,delchild,lists:concat(["Child deleted! ", ChildName," ",ChildNode])},
			io:format("~s child deleted! ~w ~s~n",[PName,ChildName,ChildNode]),
			catch {MonitorName,MonitorNode}!{PName,PNode,message,lists:concat(["child deleted! ", ChildName," ",ChildNode])},
			loop(Environment,NewChildList,RuleList,ClientList);
		%% AddClient. It's up to programmers set the Parent of the client too.
		{FromPid,FromNode,addclient,ClientName,ClientNode} -> 
			NewClientList=lists:append(ClientList,[{ClientName,ClientNode}]),
			%%{FromPid,FromNode}!{PName,PNode,addchild,lists:concat(["client added! ", ClientName," ",ClientNode])},
			io:format("~s client added ~w ~s~n",[PName,ClientName,ClientNode]),
			catch {MonitorName,MonitorNode}!{PName,PNode,message,lists:concat(["child added ", ClientName," ",ClientNode])},
			loop(Environment,ChildList,RuleList,NewClientList);
		%% DelClient. It's up to programmers set the Parent of the client to null or to another parent server.
		{FromPid,FromNode,delclient,ClientName,ClientNode} -> 
			NewClientList=lists:delete({ClientName,ClientNode},ClientList),
			%%{FromPid,FromNode}!{PName,PNode,delchild,lists:concat(["client deleted! ", ClientName," ",ClientNode])},
			io:format("~s client deleted! ~w ~s~n",[PName,ClientName,ClientNode]),
			catch {MonitorName,MonitorNode}!{PName,PNode,message,lists:concat(["child deleted! ", ClientName," ",ClientNode])},
			loop(Environment,ChildList,RuleList,NewClientList);
		%% subscribe
		%% Usata dai client per sottoscriversi. Il client passa una Fun(Message) che i server valuteranno per instradare i messaggi
		%% Il server broker aggiunge la sottoscrizione alla propria RuleList e poi si sottoscrive presso il proprio parent. La sottoscrizione procede quindi lungo il patha fino alla radice dell'albero.
		%% Ad ogni subscribe alla lista Path della sottoscrizione viene aggiunto il server che ha effettuato la sottoscrizione. In ogni RuleList quindi la sottoscrizione conterrà la lista sei server necesssari per raggiungere il sottoscrittore iniziale da quel nodo server.
		%% La RuleList è quindi una lista di entry {Fun , [{Pid0,Node0},{Pid1,Node1}, ..... ,{Pidn,Noden}]} dove {Pid0,Node0} è il client e {Pidn,Noden} il server figlio del corrente a cui occorrerà indirizzare l messaggio che matcha questa Fun(message)
		{FromPid,FromNode,subscribe,{Fun,Path}} ->
			NewPath = lists:append(Path,[{FromPid,FromNode}]),
			NewRuleList=lists:append(RuleList,[{Fun,NewPath}]), %% Aggiunge la rule {Fun,NewPath} alla propria RuleList
			io:format("~s subscription received and appended to ~s~n",[PName,"RuleList!"]),
			catch {MonitorName,MonitorNode}!{PName,PNode,message,"subscription received and appended to RuleList!"},
			if  %% Se non sono il nodo radice inoltro il messaggio di subscribe al mio parent cambiando il sottoscrittore in me stesso.
				ParentName =/= s0 -> %% se parent=null me ne accorgo dal messaggio inviato al monitor
					{ParentName,ParentNode}!{PName,PNode,subscribe,{Fun,NewPath}},
					io:format("~s subscription forwarded to parent ~w ~s~n",[PName,ParentName, ParentNode]),
					catch {MonitorName,MonitorNode}!{PName,PNode,message,lists:concat(["subscription forwarded to parent ", ParentName, " ", ParentNode])};
				true->
					s0
			end,
			loop(Environment,ChildList,NewRuleList,ClientList);
		%% unsubscribe
		%% Il messaggio di unsubscribe deve ripercorrere il path fino alla radice eliminando dalle RuleList la entry relativa
		%% Se si desidera mandare messaggio di unsubscribe da un client diverso da colui che ha fatto la subscribe occorre passare al messaggio il PName e il PNode del processo client che ha fatto la subcribe (impersonarlo)
		%% Se il path cambiasse ad esempio perchè il broker del client sottoscrittore lasciasse l'albero allora non si potrebbe più cancellare la sottoscrizione e occorre allora gestire questa situazione con il server che lascia che cancella tutte le prorie sottoscrizioni oppure un garbage collector che le elimina se non usate da x tempo
		{FromPid,FromNode,unsubscribe,{Fun,Path}} -> %% Attenzione: non necessario controllare di quale client infatti la Fun sarà diversa come oggetto (anche se codice uguale) da client a client
			NewPath = lists:append(Path,[{FromPid,FromNode}]), %% serve per l'inoltro al parent il cui path sarà così
			NewRuleList = lists:delete({Fun,NewPath},RuleList), %% Elimina la rule dalla propria RuleList
			io:format("~s subscription deleted from ~s~n",[PName,"RuleList!"]),
			catch {MonitorName,MonitorNode}!{PName,PNode,message,"subscription deleted from RuleList!"},
			if  %% Se non sono il nodo radice inoltro il messaggio di unsubscribe al mio parent cambiando il sottoscrittore in me stesso.
				ParentName =/= 's0' -> %% se parent=null me ne accorgo dal messaggio inviato al monitor
					{ParentName,ParentNode}!{PName,PNode,unsubscribe,{Fun,NewPath}},
					io:format("~s unsubscription forwarded to parent ~w ~s~n",[PName,ParentName,ParentNode]),
					catch {MonitorName,MonitorNode}!{PName,PNode,message,lists:concat(["unsubscription forwarded to parent ", ParentName, " ", ParentNode])};
				true->
					s0
			end,
			loop(Environment,ChildList,NewRuleList,ClientList);
		%% unsubscribeclient.
		%% Rimuove tutte le sottoscrizioni del client {ClientName,ClientNode}. Primo elemento dei Path delle rules. Propaga verso l'alto.
		{FromPid,FromNode,unsubscribeclient,ClientName,ClientNode} ->
			NewRuleList = delClientRules(ClientName,ClientNode,RuleList), %% Elimina tutte le rules del client dalla propria RuleList
			io:format("~s client subscriptions deleted from ~s~n",[PName,"RuleList!"]),
			catch {MonitorName,MonitorNode}!{PName,PNode,message,"client subscriptions deleted from RuleList!"},
			if  %% Se non sono il nodo radice inoltro il messaggio di unsubscribeclient al mio parent cambiando il sottoscrittore in me stesso.
				ParentName =/= 's0' -> %% se parent=null me ne accorgo dal messaggio inviato al monitor
					{ParentName,ParentNode}!{PName,PNode,unsubscribeclient,ClientName,ClientNode},
					io:format("~s client unsubscription forwarded to parent ~w ~s~n",[PName,ParentName,ParentNode]),
					catch {MonitorName,MonitorNode}!{PName,PNode,message,lists:concat(["client unsubscription forwarded to parent ", ParentName, " ", ParentNode])};
				true->
					s0
			end,
			loop(Environment,ChildList,NewRuleList,ClientList);
		%% publish
		%% Il messaggio di publish fluisce lungo il path dal sottoscrittore alla radice. Ogni nodo server intemedio applica tutte le regole della propria RuleList per vedere a quali figli va instradato il messaggio.
		%% Non viene considerato solo il figlio server se coincide con quello sottoscrittore perché in tal caso il messaggio è già stato consegnato dai servers del sottoalbero sottostante che lo contiene.
		%% Si potrebbe discutere se occorre differenziare il caso in cui il figlio sottoscrittore sia un client se cioè un client che si sottoscrive e poi pubblica un essaggio che matcha lo debba ricevere. Io scelgo di non farlo perché poco utile,ma potrebbero esserci casi in cui è utile
		%% Una volta che un server matcha il messaggio con una o più delle proprie regole (Fun(message)==true) instrada il messaggio verso il sottoscrittore usando la parte Path delle regole.
		%% La parte Path delle regole contiene la lista di entry {PNome,PNode} che rappresenta il path fino al sottoscrittor. Il sottoscrittore iniziale è il Primo ELemento della lista mentre l'ultimo elemento è il figlio del server che ha matchato.
		%% Come prima cosa allora invia al figlio (ultimo elemento della lista) il messaggio usando il mesaggio di delivery (che non ricontrola più le regole, ma semplicemente usa il path per consegnare i messaggi = fase di discesa).
		%% Nel consegnare il messggio al figlio lo rimuove (ultimo elemento) dal path. Adesso il figlio farà la stessa cosa con l'ultimo elemento del nuovo Path e così via fino a raggiungere il client finale.
		%% Questo però significherebbe che se il messaggio matcha n regole invierei n copie del messaggio e questo anche se tutti i path coincidono fino ad un certo elemento ovvero se tutte le copie vanno in prima battuta inoltrate allo stesso figlio. Sarebbe meglio che solo dal momento in cui i path si dividono i messaggi venissero duplicati.
		%% Per effettuare questa riduzione del numero di messaggi nella fase di delivery allora la funzione che controlla i match deve riotrnare la lista di regole che matchano che conterranno tutti i  path.
		%% A questo punto le routine di delivery devono estrarre gli n ultimi elementi (figli) rimuovendoli dai path, metterli in una lista e lasciarne per ognuno una sola occorrenza. Cioè lasciare solo i diversi. Adesso spedire i messaggi ai figli in essa contenuti. QQUesto restringe il numero di copie del messaggio inviate ai soli figli diversi nei vari path.
		%% Il server successivo farà la stess cosa con i path ricevuti e quidi solo quando i path divengono differneti ci sarà replicazione dei messaggi cioè solo quando necessario.
		%% La publish deve quindi memorizzare il figlio da cui ha ricevuto il publish, verificare i match della propria RuleList, estarre le n regole che matchano ed iniziare la fase di delivery verso ciascun figlio che compare nei path tranne quello da cui ha ricevuto il publish.
		%% A questo punto inoltrare al parent il messaggio di publish perché i potrebbero esserci sottoscrittori non appartenenti al sottoalbero gestito da questo server. Il processo continua fino alla radice responsabile di tutto l'albero e quindi vede tutti i sottoscrittori.
		%% La delivery riceve i messaggi di delivery e senza ricontrollare i match estrae gli n ultimi eleemnti dai path e invia messaggio di delivery agli elementi figli diversi fra loro.
		%% Message Format = {Attribute,Quantity,Value,ClientName,ClientNode}
		{FromPid,FromNode,publish,Message} -> 
			MatchingRules = match(Message,RuleList),
			if
				length(MatchingRules)=/=0 -> %% Se almeno una regola matcha il messaggio inizia la fase di delivery.
					ChildNum=deliveryMessage(PName,PNode,fetchChildrenList(FromPid,FromNode,Message,MatchingRules),Message, filterEmptyPathRules(removeLastItem(MatchingRules))), %% Passo ad ogni figlio presente come last element nei path delle rules il Messagge e la nuova MatchingRules a cui ho tolto l'ultimo elemento dei path di ogni rule e (a seguire) depurata delle rule con path vuoto.
					io:format("~s publish message match: delivering to ~w children!~n",[PName,ChildNum]),
					catch {MonitorName,MonitorNode}!{PName,PNode,message,lists:concat(["publish message match: delivering to", " ", ChildNum , " ", "children!"])};
				true->
					0
			end,			
			if  %% Se non sono il nodo radice inoltro il messaggio di publish al mio parent cambiando il sottoscrittore in me stesso.
				ParentName =/= 's0' -> %% se parent=null me ne accorgo dal messaggio inviato al monitor
					{ParentName,ParentNode}!{PName,PNode,publish,Message},
					io:format("~s publish message forwarded to parent ~w ~s!~n",[PName,ParentName,ParentNode]),
					catch {MonitorName,MonitorNode}!{PName,PNode,message,lists:concat(["publish message forwarded to parent ", ParentName, " ", ParentNode])};
				true->
					s0
			end,
			loop(Environment,ChildList,RuleList,ClientList);
			
		%% Delivery. Different from publish due to lack of match searching and lack of forwarding message to parent. It no remove child publisher (null,null)
		%% Final clients must handle this message in different way (tipically print Message to screen)
		{FromPid,FromNode,delivery,Message,MatchingRules} -> 
			if
				length(MatchingRules)=/=0 -> %% Se almeno una regola matcha il messaggio inizia la fase di delivery
					ChildNum=deliveryMessage(PName,PNode,fetchChildrenList(null,null,Message,MatchingRules),Message, filterEmptyPathRules(removeLastItem(MatchingRules))), %% Passo ad ogni figlio presente come last element nei path delle rules il Messagge e la nuova MatchingRules a cui ho tolto l'ultimo elemento dei path di ogni rule e (a seguire) depurata delle rule con path vuoto.
					io:format("~s publish message delivered to ~s!~n",[PName,"children"]),
					catch {MonitorName,MonitorNode}!{PName,PNode,message,"publish message delivered to children!"};
				true->
					0
			end,
			loop(Environment,ChildList,RuleList,ClientList);
		
		%% IsServer: true(per i server)/false(per i client)
		{FromPid,FromNode,isserver,Message} ->
			io:format("~s I'm a server! isserver=~s~n",[PName,"true"]),
			%%{FromPid,FromNode}!{PName,PNode,isserver,"true"};
			loop(Environment,ChildList,RuleList,ClientList);
		
		%% Stop the server	
		{FromPid,FromNode,stop,Message} -> 
			io:format("~s server stopped! ~s~n",[PName,Message])
			%%{FromPid,FromNode}!{PName,PNode,stop,lists:concat(["server stopped! ", Message])}		
	end.

%%
%% Local Functions
%%

%%
%% Match messages. Return: [] if there are no matches/[{Fun,ChildName,ChildNode}, ... ] if some matches are found (Lista delle regole matchanti)
%% Used by publish message handling routine in order to get matching rules
%%
match(Message,RuleList) -> 
	match(Message,RuleList,[]).
match(_,[],ReturnValue) ->
	ReturnValue;
match(Message,[{Fun,Path}|Tail],ReturnValue)->
	M=Fun(Message),
	if  
		M -> NewRetVal=lists:append(ReturnValue,[{Fun,Path}]),
			 match(Message,[],NewRetVal);
		true -> match(Message,Tail,ReturnValue)
	end.

%%
%% Fetch uniques delivering child nodes (tail recursive). Extracts last items form every Rule Path, restrict them to just different items, remove publisher if present, return delivering children list
%% In order to not remove any item from the list pass FromPid=null, FromNode=null.
%%
fetchChildrenList(FromPid,FromNode,Message,MatchingRules) -> 
	fetchChildrenList(FromPid,FromNode,Message,MatchingRules,[]).
fetchChildrenList(FromPid,FromNode,Message,[{F,P}|T],Acc) -> 
	if length(P)>0-> 
			LastItem=lists:last(P),
	   		NewAcc=lists:append(Acc,[LastItem]);
	   true ->
			NewAcc=Acc %% Se il path è nullo semplicemente non agiungo il figlio alla lista
	end,
	fetchChildrenList(FromPid,FromNode,Message,T,NewAcc);
fetchChildrenList(FromPid,FromNode,Message,[],Acc) ->
	NewAcc = compactList(Acc),
	NewAcc1 = lists:delete({FromPid,FromNode},NewAcc), %% Remove publishing child (if present). Subscriber belonging to that subtree already handled. Not used in pure delivery phase (pass FromPid=null,FromNode=null)
	NewAcc1.											%% In order to get subscriber client receive also its publishing message you need not to delete {FromPid,FromNode} if is a client of this server (checked against ChildList)

%%
%% Remove last item from every Path (Notice that initially no Path can be empty, there will be at least the client subsciber). After remove last item could be some.
%%

removeLastItem(MatchingRules) ->
	removeLastItem(MatchingRules,[]).
removeLastItem([{Fun,Path}|Tail],Acc) -> 
	if length(Path)>0 -> %% Se il path è vuoto automaticamente rimuove la rule perché non l'appende a NewAcc (NewAcc=Acc), ma questo non toglie che altre regolepossano divenirlo dopo aver rimosso il last item e quindi non toglie la necessità di avere in successione una filterEmptyPathRules
		   NewAcc= lists:append(Acc,[{Fun,lists:sublist(Path,length(Path)-1)}]);
	   true->
	 	   NewAcc=Acc
	end,
	removeLastItem(Tail,NewAcc);
removeLastItem([],Acc) -> 
	Acc.

%%
%% Filter out rules with empty Path
%%
filterEmptyPathRules(MatchingRules)	->
	lists:filter(fun({Fun,Path}) -> length(Path)=/=0 end , MatchingRules). 
									
%%
%% Return a list without duplicates 
%%
compactList(List) -> 
	compactList(List,[]).
compactList([H|T],Acc) -> 
	NewAcc = lists:append(Acc,[H]),
	NewList = lists:filter(fun(X)-> X=/=H end , T),
	compactList(NewList,NewAcc);
compactList([],Acc) -> 
	Acc.
	

%%
%% Delivery message : invia ai figli in ChildDestList il messaggio Message con la MatchingRules da usare (con Path accorciati di 1 elemento e regole con Path vuoti rimosse)
%%		

deliveryMessage(PName,PNode,ChildDestList,Message,MatchingRules) ->
	deliveryMessage(PName,PNode,ChildDestList,Message,MatchingRules,0).
deliveryMessage(PName,PNode,[{ChildName,ChildNode}|T],Message,MatchingRules,Acc) -> 
	{ChildName,ChildNode}!{PName,PNode,delivery,Message,MatchingRules}, %% Mesaggio di delivery al figlio
	NewAcc = Acc + 1,
	deliveryMessage(PName,PNode,T,Message,MatchingRules,NewAcc);
deliveryMessage(_,_,[],_,_,Acc) -> 
	Acc.

%% delClientRules/3
delClientRules(ChildName,ChildNode,RuleList) ->
	lists:filter(fun({_,Path})-> lists:nth(1,Path)=/={ChildName,ChildNode} end,RuleList).
	
	
