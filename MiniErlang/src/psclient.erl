%% Author: root
%% Created: Feb 10, 2010
%% Description: TODO: Add description to psclient
-module(psclient).

%%
%% Include files
%%
-import(random).

%%
%% Exported Functions
%%
-export([start/0, notifyInterest/1, subscribe/1, getNodeRulesList/1, getNodeParent/1, 
getNodeChildrenList/1, publish/1, publishRandom/2]).

%%
%% API Functions
%%

%%
%% TODO: Add description of start/function_arity
%%
start() -> 
	ok.
%%
%% TODO: Add description of notifyInterest/function_arity
%%
notifyInterest(_Arg0) -> 
	ok.
%%
%% TODO: Add description of subscribe/function_arity
%%
subscribe(_Arg0) -> 
	ok.
%%
%% TODO: Add description of getNodeRulesList/function_arity
%%
getNodeRulesList(_Arg0) -> 
	ok.
%%
%% TODO: Add description of getNodeParent/function_arity
%%
getNodeParent(_Arg0) -> 
	ok.
%%
%% TODO: Add description of getNodeChildrenList/function_arity
%%
getNodeChildrenList(_Arg0) -> 
	ok.
%%
%% Invia il dato al broker. Se chiamata senza argomenti genera il dato automaticamente ogni x secondi
%%
publish(_Arg0) -> 
	ok.
publishRandom(_Arg0,sec) -> 
	io:write(random:uniform(100)),
	ok.


%%
%% Local Functions
%%

