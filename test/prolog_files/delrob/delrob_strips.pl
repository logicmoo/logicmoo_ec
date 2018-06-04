% DEFINITION OF DELIVERY ROBOT WORLD IN STRIPS NOTATION
:- use_module(library(pddl_valoptic_api)).

%:- optic_workspace(add,ws1).
% ACTIONS
% move(Ag,Pos,Pos_1) is the action of Ag moving from Pos to Pos_1

:- optic_action(add,_WS,
   move(Ag,Pos,Pos_1),
   [precondition:[autonomous(Ag), adjacent(Pos,Pos_1), sitting_at(Ag,Pos)],
          effect:[sitting_at(Ag,Pos_1),not(sitting_at(Ag,Pos))]]).

% pickup(Ag,Obj,Pos) is the action of agent Ag picking up Obj.
:- optic_action(add,_WS,pickup(Ag,Obj),
   [precondition:[autonomous(Ag), Ag \= Obj, location(Pos),
                       sitting_at(Obj,Pos), at(Ag,Pos) ],
          effect:[pickup(Ag,Obj), carrying(Ag,Obj), not(sitting_at(Obj,Pos))]]).

% putdown(Ag,Obj,Pos)
:- optic_action(add,_WS,putdown(Ag,Obj,Pos), 
   [precondition:[autonomous(Ag),  Ag \= Obj, at(Ag,Pos), carrying(Ag,Obj)],
          effect:[sitting_at(Obj,Pos),not(carrying(Ag,Obj))]]).

% unlock(Ag,Door)
:- optic_action(add,_WS,unlock(Ag,Door),
   [precondition:[
        autonomous(Ag), blocks(Door,P_1,_), opens(Key,Door),  
        carrying(Ag,Key), at(Ag,P_1)],
   effect:[unlocked(Door)]]).

% PRIMITIVE RELATIONS
:- optic_predicate(add,_WS,carrying(_,_)).
:- optic_predicate(add,_WS,sitting_at(_,_)).
:- optic_predicate(add,_WS,unlocked(_)).

% DERIVED RELATIONS

:- optic_derived(add,_WS,at(Obj,Pos),[sitting_at(Obj,Pos)] ).
:- optic_derived(add,_WS,at(Obj,Pos),[autonomous(Ag), Ag \= Obj, carrying(Ag,Obj), at(Ag,Pos)]).

:- optic_derived(add,_WS,location(o109),[]).
:- optic_derived(add,_WS,location(o103),[]).
:- optic_derived(add,_WS,location(storage),[]).
:- optic_derived(add,_WS,location(o111),[]).
:- optic_derived(add,_WS,location(mail),[]).
:- optic_derived(add,_WS,location(lab2),[]).


:- optic_derived(add,_WS,adjacent(o109,o103),[]).
:- optic_derived(add,_WS,adjacent(o103,o109),[]).
:- optic_derived(add,_WS,adjacent(o109,storage),[]).
:- optic_derived(add,_WS,adjacent(storage,o109),[]).
:- optic_derived(add,_WS,adjacent(o109,o111),[]).
:- optic_derived(add,_WS,adjacent(o111,o109),[]).
:- optic_derived(add,_WS,adjacent(o103,mail),[]).
:- optic_derived(add,_WS,adjacent(mail,o103),[]).
:- optic_derived(add,_WS,adjacent(lab2,o109),[]).
:- optic_derived(add,_WS,adjacent(P_1,P_2), [blocks(Door,P_1,P_2), unlocked(Door)]).
:- optic_derived(add,_WS,blocks(door1,o103,lab2),[]).
:- optic_derived(add,_WS,opens(k1,door1),[]).
:- optic_derived(add,_WS,autonomous(rob),[]).

% INITIAL SITUATION
:- optic_init(add,_WS,sitting_at(rob,o109)).
:- optic_init(add,_WS,sitting_at(parcel,storage)).
:- optic_init(add,_WS,sitting_at(k1,mail)).

