#/bin/bash

#Command to start nodes. Use along with os:cmd() from erlang script

gnome-terminal --window-with-profile=erlang --title="test node" -e "erl -sname test -s topology" &


#Sample setting topology from shell
#../src/startnodes.sh
#erl -sname test -run topology

#Sample stopping topology from erlang VM (beam)
#topology:start().			#Open node terminals too
#topology:stop().			#Close node terminals too
#topology:startTopology(). #After stopTopology restart only processes on nodes
#topology:stopTopology(). #Doesn't close node terminals only processes. Doesn't stop monitor

#Sample Fun (subscrition). Must return true|false
#Fun=fun({Attribute,Quantity,Value,ClientName,ClientNode})-> Attribute=="physics" end.
#Fun=fun({Attribute,Quantity,Value,ClientName,ClientNode})-> (Value>20) and (Value<50) end.
#Fun=fun({Attribute,Quantity,Value,ClientName,ClientNode})-> if string:str(Quantity,"Sp")=/=0 -> true; true-> false end.
#Fun=fun({Attribute,Quantity,Value,ClientName,ClientNode})-> RVAL=regexp:match(Quantity,"^S*"), if RVAL==nomatch -> false; true-> true end end.

#Sample Messages
#{c3,node6@ubuntu904desktop}!{self(),node(),subscribe,{Fun,[]}}.
#{c3,node6@ubuntu904desktop}!{self(),node(),unsubscribe,{Fun,[]}}.
#{s4,node2@ubuntu904desktop}!{self(),node(),unsubscribeclient,c1,node5@ubuntu904desktop}.
#{s1,node1@ubuntu904desktop}!{self(),node(),getrulelist,""}.
#{c3,node6@ubuntu904desktop}!{self(),node(),getenv,""}
#{c1,node5@ubuntu904desktop}!{self(),node(),publish,{"physics","speed",10}}.

#Useful hack (node's name dynamic generation ubuntu904desktop@ubuntu904desktop)
#os:cmd(lists:concat(["gnome-terminal -e",' "erl -sname ',element(2,inet:gethostname()),'" &'])).

#Useful get registered atom name
#element(2,erlang:process_info(self(),registered_name)).

#-e, --command=STRING
#    Execute the argument to this option inside the terminal. 
#-x, --execute
#    Execute the remainder of the command line inside the terminal. 
#--window-with-profile=PROFILENAME
#    Open a new window containing a tab with the given profile. More than one of these options can be provided. 
#--tab-with-profile=PROFILENAME
#    Open a tab in the window with the given profile. More than one of these options can be provided, to open several tabs . 
#--window-with-profile-internal-id=PROFILEID
#    Open a new window containing a tab with the given profile ID. Used internally to save sessions. 
#--tab-with-profile-internal-id=PROFILEID
#    Open a tab in the window with the given profile ID. Used internally to save sessions. 
#--role=ROLE
#    Set the role for the last-specified window; applies to only one window; can be specified once for each window you create from the command line. 
#--show-menubar
#    Turn on the menu bar for the last-specified window; applies to only one window; can be specified once for each window you create from the command line. 
#--hide-menubar
#    Turn off the menu bar for the last-specified window; applies to only one window; can be specified once for each window you create from the command line. 
#--geometry=GEOMETRY
#    X geometry specification (see "X" man page), can be specified once per window to be opened. 
#--disable-factory
#    Do not register with the activation name server, do not re-use an active terminal. 
#-t, --title=TITLE
#    Set the terminal's title to TITLE. 
#--working-directory=DIRNAME
#    Set the terminal's working directory to DIRNAME. 
#--usage
#    Display brief usage message. 
#-?, --help
#    Show help message. 