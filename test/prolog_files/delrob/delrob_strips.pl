% DEFINITION OF DELIVERY ROBOT WORLD IN STRIPS NOTATION
:- use_module(library(planner_api)).

%:- planner_workspace(add,ws1).
% ACTIONS
% move(Ag,Pos,Pos_1) is the action of Ag moving from Pos to Pos_1

:- planner_action(add,_WS,
   move(Ag,Pos,Pos_1),
   [precondition:[autonomous(Ag), adjacent(Pos,Pos_1), sitting_at(Ag,Pos)],
          effect:[sitting_at(Ag,Pos_1),not(sitting_at(Ag,Pos))]]).

% pickup(Ag,Obj,Pos) is the action of agent Ag picking up Obj.
:- planner_action(add,_WS,pickup(Ag,Obj),
   [precondition:[autonomous(Ag), Ag \= Obj, location(Pos),
                       sitting_at(Obj,Pos), at(Ag,Pos) ],
          effect:[pickup(Ag,Obj), carrying(Ag,Obj), not(sitting_at(Obj,Pos))]]).

% putdown(Ag,Obj,Pos)
:- planner_action(add,_WS,putdown(Ag,Obj,Pos), 
   [precondition:[autonomous(Ag),  Ag \= Obj, at(Ag,Pos), carrying(Ag,Obj)],
          effect:[sitting_at(Obj,Pos),not(carrying(Ag,Obj))]]).

% unlock(Ag,Door)
:- planner_action(add,_WS,unlock(Ag,Door),
   [precondition:[
        autonomous(Ag), blocks(Door,P_1,_), opens(Key,Door),  
        carrying(Ag,Key), at(Ag,P_1)],
   effect:[unlocked(Door)]]).

% PRIMITIVE RELATIONS
:- planner_predicate(add,_WS,carrying(_,_)).
:- planner_predicate(add,_WS,sitting_at(_,_)).
:- planner_predicate(add,_WS,unlocked(_)).

% DERIVED RELATIONS

:- planner_derived(add,_WS,at(Obj,Pos),[sitting_at(Obj,Pos)] ).
:- planner_derived(add,_WS,at(Obj,Pos),[autonomous(Ag), Ag \= Obj, carrying(Ag,Obj), at(Ag,Pos)]).

:- planner_derived(add,_WS,location(o109),[]).
:- planner_derived(add,_WS,location(o103),[]).
:- planner_derived(add,_WS,location(storage),[]).
:- planner_derived(add,_WS,location(o111),[]).
:- planner_derived(add,_WS,location(mail),[]).
:- planner_derived(add,_WS,location(lab2),[]).


:- planner_derived(add,_WS,adjacent(o109,o103),[]).
:- planner_derived(add,_WS,adjacent(o103,o109),[]).
:- planner_derived(add,_WS,adjacent(o109,storage),[]).
:- planner_derived(add,_WS,adjacent(storage,o109),[]).
:- planner_derived(add,_WS,adjacent(o109,o111),[]).
:- planner_derived(add,_WS,adjacent(o111,o109),[]).
:- planner_derived(add,_WS,adjacent(o103,mail),[]).
:- planner_derived(add,_WS,adjacent(mail,o103),[]).
:- planner_derived(add,_WS,adjacent(lab2,o109),[]).
:- planner_derived(add,_WS,adjacent(P_1,P_2), [blocks(Door,P_1,P_2), unlocked(Door)]).
:- planner_derived(add,_WS,blocks(door1,o103,lab2),[]).
:- planner_derived(add,_WS,opens(k1,door1),[]).
:- planner_derived(add,_WS,autonomous(rob),[]).

% INITIAL SITUATION
:- planner_init(add,_WS,sitting_at(rob,o109)).
:- planner_init(add,_WS,sitting_at(parcel,storage)).
:- planner_init(add,_WS,sitting_at(k1,mail)).

