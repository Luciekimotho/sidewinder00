%% Author: Andrea Matti
%% Created: Feb 10, 2010
%% Description: 
-module(psserver).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/0, setParent/1, removeChild/1, removeRule/1, addRule/2, addChild/1, childrenManager/0, 
rulesManager/0]).

%%
%% API Functions
%%

%%
%% TODO: Add description of removeRule/function_arity
%%
start()->
	hello_world.

%%
%% TODO: Add description of removeRule/function_arity
%%
setParent(_node) ->
	ok.

%%
%% TODO: Add description of removeChild/function_arity
%%
removeChild(_Arg0) -> 
	ok.
%%
%% TODO: Add description of removeRule/function_arity
%%
removeRule(_Arg0) -> 
	ok.
%%
%% TODO: Add description of addRule/function_arity
%%
addRule(_Arg0, _Arg1) -> 
	ok.
%%
%% TODO: Add description of addChild/function_arity
%%
addChild(_Arg0) -> 
	ok.
%%
%% TODO: Add description of childrenManager/function_arity
%%
childrenManager() -> 
	ok.
%%
%% TODO: Add description of rulesManager/function_arity
%%
rulesManager() -> 
	ok.


%%
%% Local Functions
%%

