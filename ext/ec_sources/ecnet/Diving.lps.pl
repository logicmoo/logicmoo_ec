:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',0).
%~ From E:
%~ 
%~ :-( call_pel_directive( translate(unskipped,'/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.lps.pl')))
:-( call_pel_directive( translate(unskipped,'/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.lps.pl'))).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',0).
%~ From E:
%~ 
%~ :-( call_pel_directive( translate(begining,'/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.lps.pl')))
:-( call_pel_directive( translate(begining,'/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.lps.pl'))).
% Sat, 25 Sep 2021 13:18:47 GMT File: <stream>(0x55866e01e000)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; scuba diving
%;

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',14).
% sort object
%~ From E:
%~ 
%~ sort(object)
sort(object).

% sort agent: object
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',14).
%~ From E:
%~ 
%~ subsort(agent,object)
subsort(agent,object).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',16).
% sort diver: agent
%~ From E:
%~ 
%~ subsort(diver,agent)
subsort(diver,agent).

% sort depth: integer
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',16).
%~ From E:
%~ 
%~ subsort(depth,integer)
subsort(depth,integer).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',18).
% sort boat: object
%~ From E:
%~ 
%~ subsort(boat,object)
subsort(boat,object).
%; reference line, anchor line, shotline, SMB line, ...

% sort line: object
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',20).
%~ From E:
%~ 
%~ subsort(line,object)
subsort(line,object).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',23).
% sort equipment: object
%~ From E:
%~ 
%~ subsort(equipment,object)
subsort(equipment,object).

% sort weight: equipment
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',23).
%~ From E:
%~ 
%~ subsort(weight,equipment)
subsort(weight,equipment).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',25).
% sort fin: equipment
%~ From E:
%~ 
%~ subsort(fin,equipment)
subsort(fin,equipment).

% sort airtank: equipment
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',25).
%~ From E:
%~ 
%~ subsort(airtank,equipment)
subsort(airtank,equipment).
%; buoyancy compensator (BC)
%; buoyancy control device (BCD)

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',30).
% sort computer: equipment
%~ From E:
%~ 
%~ subsort(computer,equipment)
subsort(computer,equipment).

% sort bc: equipment
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',30).
%~ From E:
%~ 
%~ subsort(bc,equipment)
subsort(bc,equipment).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',33).
% fluent AtDepth(object,depth)
%~ From E:
%~ 
%~ fluent( atDepth(object,depth))
%~ cpc :- fluents([atDepth/2])
%~ ooo :- [   cl([fluents([atDepth/2])],[])]
%~ cpc :- mpred_prop(atDepth(object,depth),fluent)
%~ ooo :- [   cl([mpred_prop(atDepth(object,depth),fluent)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',33).
cl(mpred_prop(atDepth(object,depth),fluent),[]),cl(fluents(atDepth/2),[]).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',35).
% [object,depth1,depth2,time]
% HoldsAt(AtDepth(object,depth1),time) &
% HoldsAt(AtDepth(object,depth2),time) ->
% depth1 = depth2.
%~ From E:
%~ 
%~ (     holds(atDepth(Object,Depth1),Time) ,     holds(atDepth(Object,Depth2),Time)) ->         Depth1=Depth2
%~ cpc :- (     holds(atDepth(Object,Depth1),Time) ,     holds(atDepth(Object,Depth2),Time)) ->         Depth1=Depth2
%~ ooo :- [   cl(    [   equals(Depth1,Depth2)],       [      holds(atDepth(Object,Depth1),Time),           holds(atDepth(Object,Depth2),Time)])]
cl( equals(Depth1,Depth2),   (     holds(atDepth(Object,Depth1),Time) ,     holds(atDepth(Object,Depth2),Time))).
 %  cl( equals(Depth1,Depth2),   (     holds(atDepth(Object,Depth1),Time) ,     holds(atDepth(Object,Depth2),Time))).
 %  % =================================.

% event Ascend(diver,depth)
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',39).
%~ From E:
%~ 
%~ event( ascend(diver,depth))
%~ cpc :- events([ascend/2])
%~ ooo :- [   cl([events([ascend/2])],[])]
%~ cpc :- actions([ascend/2])
%~ ooo :- [   cl([actions([ascend/2])],[])]
%~ cpc :- mpred_prop(ascend(diver,depth),action)
%~ ooo :- [   cl([mpred_prop(ascend(diver,depth),action)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',39).
( cl(events(ascend/2),[])  ,    cl(mpred_prop(ascend(diver,depth),action),[]) ,     cl(actions(ascend/2),[])).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',42).
% event Descend(diver,depth)
%~ From E:
%~ 
%~ event( descend(diver,depth))
%~ cpc :- events([descend/2])
%~ ooo :- [   cl([events([descend/2])],[])]
%~ cpc :- actions([descend/2])
%~ ooo :- [   cl([actions([descend/2])],[])]
%~ cpc :- mpred_prop(descend(diver,depth),action)
%~ ooo :- [   cl([mpred_prop(descend(diver,depth),action)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',42).
( cl(events(descend/2),[])  ,    cl(mpred_prop(descend(diver,depth),action),[]) ,     cl(actions(descend/2),[])).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',44).
% [diver,depth1,depth2,time]
% HoldsAt(AtDepth(diver,depth1),time) &
% Happens(Descend(diver,depth2),time) ->
% depth2>depth1.
%~ From E:
%~ 
%~ (     holds(atDepth(Diver,Depth1),Time) ,     happens(descend(Diver,Depth2),Time)) ->         Depth2>Depth1
%~ cpc :- (     holds(atDepth(Diver,Depth1),Time) ,     happens(descend(Diver,Depth2),Time)) ->         Depth2>Depth1
%~ ooo :- [   cl(    [   comparison(Depth2,Depth1,>)],       [      holds(atDepth(Diver,Depth1),Time),           happens(descend(Diver,Depth2),Time)])]
cl( comparison(Depth2,Depth1,>),   (     holds(atDepth(Diver,Depth1),Time) ,     happens(descend(Diver,Depth2),Time))).
 %  cl( comparison(Depth2,Depth1,>),   (     holds(atDepth(Diver,Depth1),Time) ,     happens(descend(Diver,Depth2),Time))).
 %  % =================================.


% [diver,depth1,depth2,time]
% HoldsAt(AtDepth(diver,depth1),time) &
% Happens(Ascend(diver,depth2),time) ->
% depth2<depth1.
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',50).
%~ From E:
%~ 
%~ (     holds(atDepth(Diver,Depth1),Time) ,     happens(ascend(Diver,Depth2),Time)) ->         Depth2<Depth1
%~ cpc :- (     holds(atDepth(Diver,Depth1),Time) ,     happens(ascend(Diver,Depth2),Time)) ->         Depth2<Depth1
%~ ooo :- [   cl(    [   comparison(Depth2,Depth1,<)],       [      holds(atDepth(Diver,Depth1),Time),           happens(ascend(Diver,Depth2),Time)])]
cl( comparison(Depth2,Depth1,<),   (     holds(atDepth(Diver,Depth1),Time) ,     happens(ascend(Diver,Depth2),Time))).
 %  cl( comparison(Depth2,Depth1,<),   (     holds(atDepth(Diver,Depth1),Time) ,     happens(ascend(Diver,Depth2),Time))).
 %  % =================================.


% [diver,depth,time]
% Initiates(Descend(diver,depth),AtDepth(diver,depth),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',55).
%~ From E:
%~ 
%~ initiates_at(descend(Diver,Depth),atDepth(Diver,Depth),Time)
%~ cpc :- initiates(descend(Diver,Depth),atDepth(Diver,Depth),Time)
%~ ooo :- [   cl(    [   initiates(descend(Diver,Depth),atDepth(Diver,Depth),Time)],       [])]
cl(initiates(descend(Diver,Depth),atDepth(Diver,Depth),Time),[]).
 %  cl(initiates(descend(Diver,Depth),atDepth(Diver,Depth),Time),[]).
 %  % =================================.


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',57).
% [diver,depth1,depth2,time]
% HoldsAt(AtDepth(diver,depth1),time) ->
% Terminates(Descend(diver,depth2),AtDepth(diver,depth1),time).
%~ From E:
%~ 
%~ holds(atDepth(Diver,Depth1),Time) ->         terminates_at(descend(Diver,Depth2),atDepth(Diver,Depth1),Time)
%~ cpc :- holds(atDepth(Diver,Depth1),Time)
%~ ooo :- [   cl([holds(atDepth(Diver,Depth1),Time)],[])]
%~ cpc :- if(    terminates(descend(Diver,Depth2),at(atDepth(Diver,Depth1),Time)),       cl([holds(atDepth(Diver,Depth1),Time)],[]))
%~ ooo :- [   cl(    [   if(    terminates(descend(Diver,Depth2),at(atDepth(Diver,Depth1),Time)),       cl([holds(atDepth(Diver,Depth1),Time)],[]))],       [])]
cl(    (     descend(Diver,Depth2)terminates atDepth(Diver,Depth1)at Time if     cl(holds(atDepth(Diver,Depth1),Time),[])),       []).
 %  cl(    if(    terminates(descend(Diver,Depth2),at(atDepth(Diver,Depth1),Time)),       cl(holds(atDepth(Diver,Depth1),Time),[])),       []).
 %  % =================================.


% [diver,depth,time]
% Initiates(Ascend(diver,depth),AtDepth(diver,depth),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',62).
%~ From E:
%~ 
%~ initiates_at(ascend(Diver,Depth),atDepth(Diver,Depth),Time)
%~ cpc :- initiates(ascend(Diver,Depth),atDepth(Diver,Depth),Time)
%~ ooo :- [   cl(    [   initiates(ascend(Diver,Depth),atDepth(Diver,Depth),Time)],       [])]
cl(initiates(ascend(Diver,Depth),atDepth(Diver,Depth),Time),[]).
 %  cl(initiates(ascend(Diver,Depth),atDepth(Diver,Depth),Time),[]).
 %  % =================================.


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',64).
% [diver,depth1,depth2,time]
% HoldsAt(AtDepth(diver,depth1),time) ->
% Terminates(Ascend(diver,depth2),AtDepth(diver,depth1),time).
%~ From E:
%~ 
%~ holds(atDepth(Diver,Depth1),Time) ->         terminates_at(ascend(Diver,Depth2),atDepth(Diver,Depth1),Time)
%~ cpc :- holds(atDepth(Diver,Depth1),Time)
%~ ooo :- [   cl([holds(atDepth(Diver,Depth1),Time)],[])]
%~ cpc :- if(    terminates(ascend(Diver,Depth2),at(atDepth(Diver,Depth1),Time)),       cl([holds(atDepth(Diver,Depth1),Time)],[]))
%~ ooo :- [   cl(    [   if(    terminates(ascend(Diver,Depth2),at(atDepth(Diver,Depth1),Time)),       cl([holds(atDepth(Diver,Depth1),Time)],[]))],       [])]
cl(    (     ascend(Diver,Depth2)terminates atDepth(Diver,Depth1)at Time if     cl(holds(atDepth(Diver,Depth1),Time),[])),       []).
 %  cl(    if(    terminates(ascend(Diver,Depth2),at(atDepth(Diver,Depth1),Time)),       cl(holds(atDepth(Diver,Depth1),Time),[])),       []).
 %  % =================================.

% fluent Wearing(diver,equipment)
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',67).
%~ From E:
%~ 
%~ fluent( wearing(diver,equipment))
%~ cpc :- fluents([wearing/2])
%~ ooo :- [   cl([fluents([wearing/2])],[])]
%~ cpc :- mpred_prop(wearing(diver,equipment),fluent)
%~ ooo :- [   cl([mpred_prop(wearing(diver,equipment),fluent)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',67).
cl(mpred_prop(wearing(diver,equipment),fluent),[]),cl(fluents(wearing/2),[]).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',70).
% event PutOn(diver,equipment)
%~ From E:
%~ 
%~ event( putOn(diver,equipment))
%~ cpc :- events([putOn/2])
%~ ooo :- [   cl([events([putOn/2])],[])]
%~ cpc :- actions([putOn/2])
%~ ooo :- [   cl([actions([putOn/2])],[])]
%~ cpc :- mpred_prop(putOn(diver,equipment),action)
%~ ooo :- [   cl([mpred_prop(putOn(diver,equipment),action)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',70).
( cl(events(putOn/2),[])  ,    cl(mpred_prop(putOn(diver,equipment),action),[]) ,     cl(actions(putOn/2),[])).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',72).
% event TakeOff(diver,equipment)
%~ From E:
%~ 
%~ event( takeOff(diver,equipment))
%~ cpc :- events([takeOff/2])
%~ ooo :- [   cl([events([takeOff/2])],[])]
%~ cpc :- actions([takeOff/2])
%~ ooo :- [   cl([actions([takeOff/2])],[])]
%~ cpc :- mpred_prop(takeOff(diver,equipment),action)
%~ ooo :- [   cl([mpred_prop(takeOff(diver,equipment),action)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',72).
( cl(events(takeOff/2),[])  ,    cl(mpred_prop(takeOff(diver,equipment),action),[]) ,     cl(actions(takeOff/2),[])).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',74).
% event Lose(diver,equipment)
%~ From E:
%~ 
%~ event( lose(diver,equipment))
%~ cpc :- events([lose/2])
%~ ooo :- [   cl([events([lose/2])],[])]
%~ cpc :- actions([lose/2])
%~ ooo :- [   cl([actions([lose/2])],[])]
%~ cpc :- mpred_prop(lose(diver,equipment),action)
%~ ooo :- [   cl([mpred_prop(lose(diver,equipment),action)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',74).
( cl(events(lose/2),[])  ,    cl(mpred_prop(lose(diver,equipment),action),[]) ,     cl(actions(lose/2),[])).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',76).
% [diver,equipment,depth,time]
% Releases(PutOn(diver,equipment),AtDepth(equipment,depth),time).
%~ From E:
%~ 
%~ releases_at(putOn(Diver,Equipment),atDepth(Equipment,Depth),Time)
%~ cpc :- releases(putOn(Diver,Equipment),atDepth(Equipment,Depth),Time)
%~ ooo :- [   cl(    [   releases(putOn(Diver,Equipment),atDepth(Equipment,Depth),Time)],       [])]
cl(releases(putOn(Diver,Equipment),atDepth(Equipment,Depth),Time),[]).
 %  cl(releases(putOn(Diver,Equipment),atDepth(Equipment,Depth),Time),[]).
 %  % =================================.


% [diver,equipment,time]
% Releases(PutOn(diver,equipment),UnderWater(equipment),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',80).
%~ From E:
%~ 
%~ releases_at(putOn(Diver,Equipment),underWater(Equipment),Time)
%~ cpc :- releases(putOn(Diver,Equipment),underWater(Equipment),Time)
%~ ooo :- [   cl([releases(putOn(Diver,Equipment),underWater(Equipment),Time)],[])]
cl(releases(putOn(Diver,Equipment),underWater(Equipment),Time),[]).
 %  cl(releases(putOn(Diver,Equipment),underWater(Equipment),Time),[]).
 %  % =================================.


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',82).
% [diver,equipment,time]
% Happens(PutOn(diver,equipment),time) ->
% !{diver1} HoldsAt(Wearing(diver1,equipment),time).
%~ From E:
%~ 
%~ happens(putOn(Diver,Equipment),Time) ->         not( thereExists(Diver1,holds(wearing(Diver1,Equipment),Time)))
%~ cpc :- happens(putOn(Diver,Equipment),Time) ->         not( thereExists(Diver1,holds(wearing(Diver1,Equipment),Time)))
%~ ooo :- [   cl(    [   exists( Diver1,   (     not( holds(wearing(Diver1,Equipment),Time)) ;     not( happens(putOn(Diver,Equipment),Time))))],       [])]
cl(    exists( Diver1,   (     not( holds(wearing(Diver1,Equipment),Time)) ;     not( happens(putOn(Diver,Equipment),Time)))),       []).
 %  ( cl(    exists( Diver1,   (     not( holds(wearing(Diver1,Equipment),Time)) ;     not( happens(putOn(Diver,Equipment),Time)))),       [])).
 %  % =================================.


% [diver,depth,equipment,time]
% HoldsAt(Wearing(diver,equipment),time) ->
% (HoldsAt(AtDepth(diver,depth),time) <->
%  HoldsAt(AtDepth(equipment,depth),time)).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',87).
%~ From E:
%~ 
%~ holds(wearing(Diver,Equipment),Time) ->         (     holds(atDepth(Diver,Depth),Time) <->     holds(atDepth(Equipment,Depth),Time))
%~ cpc :- holds(wearing(Diver,Equipment),Time) ->         (     holds(atDepth(Diver,Depth),Time) <->     holds(atDepth(Equipment,Depth),Time))
%~ ooo :- [   cl(    [   holds(atDepth(Equipment,Depth),Time)],       [      holds(atDepth(Diver,Depth),Time),           holds(wearing(Diver,Equipment),Time)]),     cl(      [   holds(atDepth(Diver,Depth),Time)],           [        holds(atDepth(Equipment,Depth),Time),               holds(wearing(Diver,Equipment),Time)])]
(     cl(      holds(atDepth(Equipment,Depth),Time),           (               holds(atDepth(Diver,Depth),Time) ,               holds(wearing(Diver,Equipment),Time))) ,     cl(      holds(atDepth(Diver,Depth),Time),           (               holds(atDepth(Equipment,Depth),Time) ,               holds(wearing(Diver,Equipment),Time)))).
 %  (     cl(      holds(atDepth(Equipment,Depth),Time),           (               holds(atDepth(Diver,Depth),Time) ,               holds(wearing(Diver,Equipment),Time))) ,     cl(      holds(atDepth(Diver,Depth),Time),           (               holds(atDepth(Equipment,Depth),Time) ,               holds(wearing(Diver,Equipment),Time)))).
 %  % =================================.


% [diver,depth,object,time]
% HoldsAt(Holding(diver,object),time) ->
% (HoldsAt(AtDepth(diver,depth),time) <->
%  HoldsAt(AtDepth(object,depth),time)).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',92).
%~ From E:
%~ 
%~ holds(holding(Diver,Object),Time) ->         (     holds(atDepth(Diver,Depth),Time) <->     holds(atDepth(Object,Depth),Time))
%~ cpc :- holds(holding(Diver,Object),Time) ->         (     holds(atDepth(Diver,Depth),Time) <->     holds(atDepth(Object,Depth),Time))
%~ ooo :- [   cl(    [   holds(atDepth(Object,Depth),Time)],       [      holds(atDepth(Diver,Depth),Time),           holds(holding(Diver,Object),Time)]),     cl(      [   holds(atDepth(Diver,Depth),Time)],           [        holds(atDepth(Object,Depth),Time),               holds(holding(Diver,Object),Time)])]
(     cl(      holds(atDepth(Object,Depth),Time),           (               holds(atDepth(Diver,Depth),Time) ,               holds(holding(Diver,Object),Time))) ,     cl(      holds(atDepth(Diver,Depth),Time),           (               holds(atDepth(Object,Depth),Time) ,               holds(holding(Diver,Object),Time)))).
 %  (     cl(      holds(atDepth(Object,Depth),Time),           (               holds(atDepth(Diver,Depth),Time) ,               holds(holding(Diver,Object),Time))) ,     cl(      holds(atDepth(Diver,Depth),Time),           (               holds(atDepth(Object,Depth),Time) ,               holds(holding(Diver,Object),Time)))).
 %  % =================================.


% [diver,equipment,time]
% HoldsAt(Wearing(diver,equipment),time) ->
% (HoldsAt(UnderWater(diver),time) <->
%  HoldsAt(UnderWater(equipment),time)).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',97).
%~ From E:
%~ 
%~ holds(wearing(Diver,Equipment),Time) ->         <->(holds(underWater(Diver),Time),holds(underWater(Equipment),Time))
%~ cpc :- holds(wearing(Diver,Equipment),Time) ->         <->(holds(underWater(Diver),Time),holds(underWater(Equipment),Time))
%~ ooo :- [   cl(    [   holds(underWater(Equipment),Time)],       [      holds(underWater(Diver),Time),           holds(wearing(Diver,Equipment),Time)]),     cl(      [   holds(underWater(Diver),Time)],           [        holds(underWater(Equipment),Time),               holds(wearing(Diver,Equipment),Time)])]
(     cl(      holds(underWater(Equipment),Time),           (               holds(underWater(Diver),Time) ,               holds(wearing(Diver,Equipment),Time))) ,     cl(      holds(underWater(Diver),Time),           (               holds(underWater(Equipment),Time) ,               holds(wearing(Diver,Equipment),Time)))).
 %  (     cl(      holds(underWater(Equipment),Time),           (               holds(underWater(Diver),Time) ,               holds(wearing(Diver,Equipment),Time))) ,     cl(      holds(underWater(Diver),Time),           (               holds(underWater(Equipment),Time) ,               holds(wearing(Diver,Equipment),Time)))).
 %  % =================================.


% [diver,object,time]
% HoldsAt(Holding(diver,object),time) ->
% (HoldsAt(UnderWater(diver),time) <->
%  HoldsAt(UnderWater(object),time)).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',102).
%~ From E:
%~ 
%~ holds(holding(Diver,Object),Time) ->         <->(holds(underWater(Diver),Time),holds(underWater(Object),Time))
%~ cpc :- holds(holding(Diver,Object),Time) ->         <->(holds(underWater(Diver),Time),holds(underWater(Object),Time))
%~ ooo :- [   cl(    [   holds(underWater(Object),Time)],       [      holds(underWater(Diver),Time),           holds(holding(Diver,Object),Time)]),     cl(      [   holds(underWater(Diver),Time)],           [        holds(underWater(Object),Time),               holds(holding(Diver,Object),Time)])]
(     cl(      holds(underWater(Object),Time),           (               holds(underWater(Diver),Time) ,               holds(holding(Diver,Object),Time))) ,     cl(      holds(underWater(Diver),Time),           (               holds(underWater(Object),Time) ,               holds(holding(Diver,Object),Time)))).
 %  (     cl(      holds(underWater(Object),Time),           (               holds(underWater(Diver),Time) ,               holds(holding(Diver,Object),Time))) ,     cl(      holds(underWater(Diver),Time),           (               holds(underWater(Object),Time) ,               holds(holding(Diver,Object),Time)))).
 %  % =================================.


% [diver,depth,equipment,time]
% HoldsAt(AtDepth(diver,depth),time) &
% HoldsAt(Wearing(diver,equipment),time) ->
% Initiates(TakeOff(diver,equipment),AtDepth(equipment,depth),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',107).
%~ From E:
%~ 
%~ (     holds(atDepth(Diver,Depth),Time) ,     holds(wearing(Diver,Equipment),Time)) ->         initiates_at(takeOff(Diver,Equipment),atDepth(Equipment,Depth),Time)
%~ cpc :- holds(atDepth(Diver,Depth),Time)
%~ ooo :- [   cl([holds(atDepth(Diver,Depth),Time)],[])]
%~ cpc :- holds(wearing(Diver,Equipment),Time)
%~ ooo :- [   cl([holds(wearing(Diver,Equipment),Time)],[])]
%~ cpc :- if(    initiates(takeOff(Diver,Equipment),at(atDepth(Equipment,Depth),Time)),       (           cl([holds(atDepth(Diver,Depth),Time)],[]) ,           cl([holds(wearing(Diver,Equipment),Time)],[])))
%~ ooo :- [   cl(    [   if(    initiates(takeOff(Diver,Equipment),at(atDepth(Equipment,Depth),Time)),       (           cl([holds(atDepth(Diver,Depth),Time)],[]) ,           cl([holds(wearing(Diver,Equipment),Time)],[])))],       [])]
cl(    (     (         takeOff(Diver,Equipment) initiates         atDepth(Equipment,Depth)at Time) if     (         cl(holds(atDepth(Diver,Depth),Time),[]) ,         cl(holds(wearing(Diver,Equipment),Time),[]))),       []).
 %  cl(    if(    initiates(takeOff(Diver,Equipment),at(atDepth(Equipment,Depth),Time)),       (           cl(holds(atDepth(Diver,Depth),Time),[]) ,           cl(holds(wearing(Diver,Equipment),Time),[]))),       []).
 %  % =================================.


% [diver,depth,equipment,time]
% !HoldsAt(AtDepth(diver,depth),time) &
% HoldsAt(Wearing(diver,equipment),time) ->
% Terminates(TakeOff(diver,equipment),AtDepth(equipment,depth),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',112).
%~ From E:
%~ 
%~ (     holds(not(atDepth(Diver,Depth)),Time) ,     holds(wearing(Diver,Equipment),Time)) ->         terminates_at(takeOff(Diver,Equipment),atDepth(Equipment,Depth),Time)
%~ cpc :- holds(not(atDepth(Diver,Depth)),Time)
%~ ooo :- [   cl([],[holds(atDepth(Diver,Depth),Time)])]
%~ cpc :- holds(wearing(Diver,Equipment),Time)
%~ ooo :- [   cl([holds(wearing(Diver,Equipment),Time)],[])]
%~ cpc :- if(    terminates( takeOff(Diver,Equipment),   at(atDepth(Equipment,Depth),Time)),       (           cl([],[holds(atDepth(Diver,Depth),Time)]) ,           cl([holds(wearing(Diver,Equipment),Time)],[])))
%~ ooo :- [   cl(    [   if(    terminates( takeOff(Diver,Equipment),   at(atDepth(Equipment,Depth),Time)),       (           cl([],[holds(atDepth(Diver,Depth),Time)]) ,           cl([holds(wearing(Diver,Equipment),Time)],[])))],       [])]
cl(    (     (         takeOff(Diver,Equipment) terminates         atDepth(Equipment,Depth)at Time) if     (         cl([],holds(atDepth(Diver,Depth),Time)) ,         cl(holds(wearing(Diver,Equipment),Time),[]))),       []).
 %  cl(    if(    terminates( takeOff(Diver,Equipment),   at(atDepth(Equipment,Depth),Time)),       (           cl([],holds(atDepth(Diver,Depth),Time)) ,           cl(holds(wearing(Diver,Equipment),Time),[]))),       []).
 %  % =================================.


% [diver,equipment,time]
% HoldsAt(UnderWater(diver),time) ->
% Initiates(TakeOff(diver,equipment),UnderWater(equipment),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',117).
%~ From E:
%~ 
%~ holds(underWater(Diver),Time) ->         initiates_at(takeOff(Diver,Equipment),underWater(Equipment),Time)
%~ cpc :- holds(underWater(Diver),Time)
%~ ooo :- [   cl([holds(underWater(Diver),Time)],[])]
%~ cpc :- if(    initiates(takeOff(Diver,Equipment),at(underWater(Equipment),Time)),       cl([holds(underWater(Diver),Time)],[]))
%~ ooo :- [   cl(    [   if(    initiates(takeOff(Diver,Equipment),at(underWater(Equipment),Time)),       cl([holds(underWater(Diver),Time)],[]))],       [])]
cl(    (     takeOff(Diver,Equipment)initiates underWater(Equipment)at Time if     cl(holds(underWater(Diver),Time),[])),       []).
 %  cl(    if(    initiates(takeOff(Diver,Equipment),at(underWater(Equipment),Time)),       cl(holds(underWater(Diver),Time),[])),       []).
 %  % =================================.


% [diver,equipment,time]
% !HoldsAt(UnderWater(diver),time) ->
% Terminates(TakeOff(diver,equipment),UnderWater(equipment),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',121).
%~ From E:
%~ 
%~ holds(not(underWater(Diver)),Time) ->         terminates_at(takeOff(Diver,Equipment),underWater(Equipment),Time)
%~ cpc :- holds(not(underWater(Diver)),Time)
%~ ooo :- [   cl([],[holds(underWater(Diver),Time)])]
%~ cpc :- if(    terminates(takeOff(Diver,Equipment),at(underWater(Equipment),Time)),       cl([],[holds(underWater(Diver),Time)]))
%~ ooo :- [   cl(    [   if(    terminates(takeOff(Diver,Equipment),at(underWater(Equipment),Time)),       cl([],[holds(underWater(Diver),Time)]))],       [])]
cl(    (     takeOff(Diver,Equipment)terminates underWater(Equipment)at Time if     cl([],holds(underWater(Diver),Time))),       []).
 %  cl(    if(    terminates(takeOff(Diver,Equipment),at(underWater(Equipment),Time)),       cl([],holds(underWater(Diver),Time))),       []).
 %  % =================================.


% [diver,equipment,depth,time]
% HoldsAt(AtDepth(diver,depth),time) &
% HoldsAt(Wearing(diver,equipment),time) ->
% Initiates(Lose(diver,equipment),AtDepth(equipment,depth),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',125).
%~ From E:
%~ 
%~ (     holds(atDepth(Diver,Depth),Time) ,     holds(wearing(Diver,Equipment),Time)) ->         initiates_at(lose(Diver,Equipment),atDepth(Equipment,Depth),Time)
%~ cpc :- holds(atDepth(Diver,Depth),Time)
%~ ooo :- [   cl([holds(atDepth(Diver,Depth),Time)],[])]
%~ cpc :- holds(wearing(Diver,Equipment),Time)
%~ ooo :- [   cl([holds(wearing(Diver,Equipment),Time)],[])]
%~ cpc :- if(    initiates(lose(Diver,Equipment),at(atDepth(Equipment,Depth),Time)),       (           cl([holds(atDepth(Diver,Depth),Time)],[]) ,           cl([holds(wearing(Diver,Equipment),Time)],[])))
%~ ooo :- [   cl(    [   if(    initiates(lose(Diver,Equipment),at(atDepth(Equipment,Depth),Time)),       (           cl([holds(atDepth(Diver,Depth),Time)],[]) ,           cl([holds(wearing(Diver,Equipment),Time)],[])))],       [])]
cl(    (     lose(Diver,Equipment)initiates atDepth(Equipment,Depth)at Time if     (         cl(holds(atDepth(Diver,Depth),Time),[]) ,         cl(holds(wearing(Diver,Equipment),Time),[]))),       []).
 %  cl(    if(    initiates(lose(Diver,Equipment),at(atDepth(Equipment,Depth),Time)),       (           cl(holds(atDepth(Diver,Depth),Time),[]) ,           cl(holds(wearing(Diver,Equipment),Time),[]))),       []).
 %  % =================================.


% [diver,equipment,depth,time]
% !HoldsAt(AtDepth(diver,depth),time) &
% HoldsAt(Wearing(diver,equipment),time) ->
% Terminates(Lose(diver,equipment),AtDepth(equipment,depth),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',130).
%~ From E:
%~ 
%~ (     holds(not(atDepth(Diver,Depth)),Time) ,     holds(wearing(Diver,Equipment),Time)) ->         terminates_at(lose(Diver,Equipment),atDepth(Equipment,Depth),Time)
%~ cpc :- holds(not(atDepth(Diver,Depth)),Time)
%~ ooo :- [   cl([],[holds(atDepth(Diver,Depth),Time)])]
%~ cpc :- holds(wearing(Diver,Equipment),Time)
%~ ooo :- [   cl([holds(wearing(Diver,Equipment),Time)],[])]
%~ cpc :- if(    terminates(lose(Diver,Equipment),at(atDepth(Equipment,Depth),Time)),       (           cl([],[holds(atDepth(Diver,Depth),Time)]) ,           cl([holds(wearing(Diver,Equipment),Time)],[])))
%~ ooo :- [   cl(    [   if(    terminates(lose(Diver,Equipment),at(atDepth(Equipment,Depth),Time)),       (           cl([],[holds(atDepth(Diver,Depth),Time)]) ,           cl([holds(wearing(Diver,Equipment),Time)],[])))],       [])]
cl(    (     lose(Diver,Equipment)terminates atDepth(Equipment,Depth)at Time if     (         cl([],holds(atDepth(Diver,Depth),Time)) ,         cl(holds(wearing(Diver,Equipment),Time),[]))),       []).
 %  cl(    if(    terminates(lose(Diver,Equipment),at(atDepth(Equipment,Depth),Time)),       (           cl([],holds(atDepth(Diver,Depth),Time)) ,           cl(holds(wearing(Diver,Equipment),Time),[]))),       []).
 %  % =================================.


% [diver,equipment,time]
% HoldsAt(UnderWater(diver),time) ->
% Initiates(Lose(diver,equipment),UnderWater(equipment),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',135).
%~ From E:
%~ 
%~ holds(underWater(Diver),Time) ->         initiates_at(lose(Diver,Equipment),underWater(Equipment),Time)
%~ cpc :- holds(underWater(Diver),Time)
%~ ooo :- [   cl([holds(underWater(Diver),Time)],[])]
%~ cpc :- if(    initiates(lose(Diver,Equipment),at(underWater(Equipment),Time)),       cl([holds(underWater(Diver),Time)],[]))
%~ ooo :- [   cl(    [   if(    initiates(lose(Diver,Equipment),at(underWater(Equipment),Time)),       cl([holds(underWater(Diver),Time)],[]))],       [])]
cl(    (     lose(Diver,Equipment)initiates underWater(Equipment)at Time if     cl(holds(underWater(Diver),Time),[])),       []).
 %  cl(    if(    initiates(lose(Diver,Equipment),at(underWater(Equipment),Time)),       cl(holds(underWater(Diver),Time),[])),       []).
 %  % =================================.


% [diver,equipment,time]
% !HoldsAt(UnderWater(diver),time) ->
% Terminates(Lose(diver,equipment),UnderWater(equipment),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',139).
%~ From E:
%~ 
%~ holds(not(underWater(Diver)),Time) ->         terminates_at(lose(Diver,Equipment),underWater(Equipment),Time)
%~ cpc :- holds(not(underWater(Diver)),Time)
%~ ooo :- [   cl([],[holds(underWater(Diver),Time)])]
%~ cpc :- if(    terminates(lose(Diver,Equipment),at(underWater(Equipment),Time)),       cl([],[holds(underWater(Diver),Time)]))
%~ ooo :- [   cl(    [   if(    terminates(lose(Diver,Equipment),at(underWater(Equipment),Time)),       cl([],[holds(underWater(Diver),Time)]))],       [])]
cl(    (     lose(Diver,Equipment)terminates underWater(Equipment)at Time if     cl([],holds(underWater(Diver),Time))),       []).
 %  cl(    if(    terminates(lose(Diver,Equipment),at(underWater(Equipment),Time)),       cl([],holds(underWater(Diver),Time))),       []).
 %  % =================================.

% fluent Holding(diver,object)
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',141).
%~ From E:
%~ 
%~ fluent( holding(diver,object))
%~ cpc :- fluents([holding/2])
%~ ooo :- [   cl([fluents([holding/2])],[])]
%~ cpc :- mpred_prop(holding(diver,object),fluent)
%~ ooo :- [   cl([mpred_prop(holding(diver,object),fluent)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',141).
cl(mpred_prop(holding(diver,object),fluent),[]),cl(fluents(holding/2),[]).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',144).
% [diver1,diver2,time]
% HoldsAt(Holding(diver1,diver2),time) ->
% !HoldsAt(Holding(diver2,diver1),time).
%~ From E:
%~ 
%~ holds(holding(Diver1,Diver2),Time) ->         holds(not(holding(Diver2,Diver1)),Time)
%~ cpc :- holds(holding(Diver1,Diver2),Time) ->         holds(not(holding(Diver2,Diver1)),Time)
%~ ooo :- [   cl( [], [  [   holds(holding(Diver2,Diver1),Time),     holds(holding(Diver1,Diver2),Time)])]]
cl( [],   (     holds(holding(Diver2,Diver1),Time) ,     holds(holding(Diver1,Diver2),Time))).
 %  cl( [],   (     holds(holding(Diver2,Diver1),Time) ,     holds(holding(Diver1,Diver2),Time))).
 %  % =================================.

% event Grab(diver,object)
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',147).
%~ From E:
%~ 
%~ event( grab(diver,object))
%~ cpc :- events([grab/2])
%~ ooo :- [   cl([events([grab/2])],[])]
%~ cpc :- actions([grab/2])
%~ ooo :- [   cl([actions([grab/2])],[])]
%~ cpc :- mpred_prop(grab(diver,object),action)
%~ ooo :- [   cl([mpred_prop(grab(diver,object),action)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',147).
( cl(events(grab/2),[])  ,    cl(mpred_prop(grab(diver,object),action),[]) ,     cl(actions(grab/2),[])).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',150).
% event LetGoOf(diver,object)
%~ From E:
%~ 
%~ event( letGoOf(diver,object))
%~ cpc :- events([letGoOf/2])
%~ ooo :- [   cl([events([letGoOf/2])],[])]
%~ cpc :- actions([letGoOf/2])
%~ ooo :- [   cl([actions([letGoOf/2])],[])]
%~ cpc :- mpred_prop(letGoOf(diver,object),action)
%~ ooo :- [   cl([mpred_prop(letGoOf(diver,object),action)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',150).
( cl(events(letGoOf/2),[])  ,    cl(mpred_prop(letGoOf(diver,object),action),[]) ,     cl(actions(letGoOf/2),[])).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',152).
% [diver,object,time]
% Initiates(Grab(diver,object),Holding(diver,object),time).
%~ From E:
%~ 
%~ initiates_at(grab(Diver,Object),holding(Diver,Object),Time)
%~ cpc :- initiates(grab(Diver,Object),holding(Diver,Object),Time)
%~ ooo :- [   cl(    [   initiates(grab(Diver,Object),holding(Diver,Object),Time)],       [])]
cl(initiates(grab(Diver,Object),holding(Diver,Object),Time),[]).
 %  cl(initiates(grab(Diver,Object),holding(Diver,Object),Time),[]).
 %  % =================================.


% [diver,object,time]
% Terminates(LetGoOf(diver,object),Holding(diver,object),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',156).
%~ From E:
%~ 
%~ terminates_at(letGoOf(Diver,Object),holding(Diver,Object),Time)
%~ cpc :- terminates(letGoOf(Diver,Object),holding(Diver,Object),Time)
%~ ooo :- [   cl(    [   terminates(letGoOf(Diver,Object),holding(Diver,Object),Time)],       [])]
cl(terminates(letGoOf(Diver,Object),holding(Diver,Object),Time),[]).
 %  cl(terminates(letGoOf(Diver,Object),holding(Diver,Object),Time),[]).
 %  % =================================.


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',158).
% [diver,object,depth,time]
% Releases(Grab(diver,object),AtDepth(object,depth),time).
%~ From E:
%~ 
%~ releases_at(grab(Diver,Object),atDepth(Object,Depth),Time)
%~ cpc :- releases(grab(Diver,Object),atDepth(Object,Depth),Time)
%~ ooo :- [   cl(    [   releases(grab(Diver,Object),atDepth(Object,Depth),Time)],       [])]
cl(releases(grab(Diver,Object),atDepth(Object,Depth),Time),[]).
 %  cl(releases(grab(Diver,Object),atDepth(Object,Depth),Time),[]).
 %  % =================================.


% [diver,object,time]
% Releases(Grab(diver,object),UnderWater(object),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',162).
%~ From E:
%~ 
%~ releases_at(grab(Diver,Object),underWater(Object),Time)
%~ cpc :- releases(grab(Diver,Object),underWater(Object),Time)
%~ ooo :- [   cl([releases(grab(Diver,Object),underWater(Object),Time)],[])]
cl(releases(grab(Diver,Object),underWater(Object),Time),[]).
 %  cl(releases(grab(Diver,Object),underWater(Object),Time),[]).
 %  % =================================.


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',164).
% [diver,object,depth,time]
% HoldsAt(AtDepth(diver,depth),time) &
% HoldsAt(Holding(diver,object),time) ->
% Initiates(LetGoOf(diver,object),AtDepth(object,depth),time).
%~ From E:
%~ 
%~ (     holds(atDepth(Diver,Depth),Time) ,     holds(holding(Diver,Object),Time)) ->         initiates_at(letGoOf(Diver,Object),atDepth(Object,Depth),Time)
%~ cpc :- holds(atDepth(Diver,Depth),Time)
%~ ooo :- [   cl([holds(atDepth(Diver,Depth),Time)],[])]
%~ cpc :- holds(holding(Diver,Object),Time)
%~ ooo :- [   cl([holds(holding(Diver,Object),Time)],[])]
%~ cpc :- if(    initiates(letGoOf(Diver,Object),at(atDepth(Object,Depth),Time)),       (           cl([holds(atDepth(Diver,Depth),Time)],[]) ,           cl([holds(holding(Diver,Object),Time)],[])))
%~ ooo :- [   cl(    [   if(    initiates(letGoOf(Diver,Object),at(atDepth(Object,Depth),Time)),       (           cl([holds(atDepth(Diver,Depth),Time)],[]) ,           cl([holds(holding(Diver,Object),Time)],[])))],       [])]
cl(    (     letGoOf(Diver,Object)initiates atDepth(Object,Depth)at Time if     (         cl(holds(atDepth(Diver,Depth),Time),[]) ,         cl(holds(holding(Diver,Object),Time),[]))),       []).
 %  cl(    if(    initiates(letGoOf(Diver,Object),at(atDepth(Object,Depth),Time)),       (           cl(holds(atDepth(Diver,Depth),Time),[]) ,           cl(holds(holding(Diver,Object),Time),[]))),       []).
 %  % =================================.


% [diver,object,depth,time]
% !HoldsAt(AtDepth(diver,depth),time) &
% HoldsAt(Holding(diver,object),time) ->
% Terminates(LetGoOf(diver,object),AtDepth(object,depth),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',170).
%~ From E:
%~ 
%~ (     holds(not(atDepth(Diver,Depth)),Time) ,     holds(holding(Diver,Object),Time)) ->         terminates_at(letGoOf(Diver,Object),atDepth(Object,Depth),Time)
%~ cpc :- holds(not(atDepth(Diver,Depth)),Time)
%~ ooo :- [   cl([],[holds(atDepth(Diver,Depth),Time)])]
%~ cpc :- holds(holding(Diver,Object),Time)
%~ ooo :- [   cl([holds(holding(Diver,Object),Time)],[])]
%~ cpc :- if(    terminates(letGoOf(Diver,Object),at(atDepth(Object,Depth),Time)),       (           cl([],[holds(atDepth(Diver,Depth),Time)]) ,           cl([holds(holding(Diver,Object),Time)],[])))
%~ ooo :- [   cl(    [   if(    terminates(letGoOf(Diver,Object),at(atDepth(Object,Depth),Time)),       (           cl([],[holds(atDepth(Diver,Depth),Time)]) ,           cl([holds(holding(Diver,Object),Time)],[])))],       [])]
cl(    (     letGoOf(Diver,Object)terminates atDepth(Object,Depth)at Time if     (         cl([],holds(atDepth(Diver,Depth),Time)) ,         cl(holds(holding(Diver,Object),Time),[]))),       []).
 %  cl(    if(    terminates(letGoOf(Diver,Object),at(atDepth(Object,Depth),Time)),       (           cl([],holds(atDepth(Diver,Depth),Time)) ,           cl(holds(holding(Diver,Object),Time),[]))),       []).
 %  % =================================.


% [diver,object,time]
% HoldsAt(UnderWater(diver),time) ->
% Initiates(LetGoOf(diver,object),UnderWater(object),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',175).
%~ From E:
%~ 
%~ holds(underWater(Diver),Time) ->         initiates_at(letGoOf(Diver,Object),underWater(Object),Time)
%~ cpc :- holds(underWater(Diver),Time)
%~ ooo :- [   cl([holds(underWater(Diver),Time)],[])]
%~ cpc :- if(    initiates(letGoOf(Diver,Object),at(underWater(Object),Time)),       cl([holds(underWater(Diver),Time)],[]))
%~ ooo :- [   cl(    [   if(    initiates(letGoOf(Diver,Object),at(underWater(Object),Time)),       cl([holds(underWater(Diver),Time)],[]))],       [])]
cl(    (     letGoOf(Diver,Object)initiates underWater(Object)at Time if     cl(holds(underWater(Diver),Time),[])),       []).
 %  cl(    if(    initiates(letGoOf(Diver,Object),at(underWater(Object),Time)),       cl(holds(underWater(Diver),Time),[])),       []).
 %  % =================================.


% [diver,object,time]
% !HoldsAt(UnderWater(diver),time) ->
% Terminates(LetGoOf(diver,object),UnderWater(object),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',179).
%~ From E:
%~ 
%~ holds(not(underWater(Diver)),Time) ->         terminates_at(letGoOf(Diver,Object),underWater(Object),Time)
%~ cpc :- holds(not(underWater(Diver)),Time)
%~ ooo :- [   cl([],[holds(underWater(Diver),Time)])]
%~ cpc :- if(    terminates(letGoOf(Diver,Object),at(underWater(Object),Time)),       cl([],[holds(underWater(Diver),Time)]))
%~ ooo :- [   cl(    [   if(    terminates(letGoOf(Diver,Object),at(underWater(Object),Time)),       cl([],[holds(underWater(Diver),Time)]))],       [])]
cl(    (     letGoOf(Diver,Object)terminates underWater(Object)at Time if     cl([],holds(underWater(Diver),Time))),       []).
 %  cl(    if(    terminates(letGoOf(Diver,Object),at(underWater(Object),Time)),       cl([],holds(underWater(Diver),Time))),       []).
 %  % =================================.


% [diver,equipment,time]
% Initiates(PutOn(diver,equipment),Wearing(diver,equipment),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',183).
%~ From E:
%~ 
%~ initiates_at(putOn(Diver,Equipment),wearing(Diver,Equipment),Time)
%~ cpc :- initiates(putOn(Diver,Equipment),wearing(Diver,Equipment),Time)
%~ ooo :- [   cl(    [   initiates(putOn(Diver,Equipment),wearing(Diver,Equipment),Time)],       [])]
cl(    initiates(putOn(Diver,Equipment),wearing(Diver,Equipment),Time),       []).
 %  cl(    initiates(putOn(Diver,Equipment),wearing(Diver,Equipment),Time),       []).
 %  % =================================.


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',185).
% [diver,equipment,time]
% Happens(PutOn(diver,equipment),time) ->
% !HoldsAt(UnderWater(diver),time).
%~ From E:
%~ 
%~ happens(putOn(Diver,Equipment),Time) ->         holds(not(underWater(Diver)),Time)
%~ cpc :- happens(putOn(Diver,Equipment),Time) ->         holds(not(underWater(Diver)),Time)
%~ ooo :- [   cl( [], [  [   holds(underWater(Diver),Time),     happens(putOn(Diver,Equipment),Time)])]]
cl( [],   (     holds(underWater(Diver),Time) ,     happens(putOn(Diver,Equipment),Time))).
 %  cl( [],   (     holds(underWater(Diver),Time) ,     happens(putOn(Diver,Equipment),Time))).
 %  % =================================.


% [diver,equipment,time]
% Terminates(TakeOff(diver,equipment),Wearing(diver,equipment),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',190).
%~ From E:
%~ 
%~ terminates_at(takeOff(Diver,Equipment),wearing(Diver,Equipment),Time)
%~ cpc :- terminates(takeOff(Diver,Equipment),wearing(Diver,Equipment),Time)
%~ ooo :- [   cl(    [   terminates(takeOff(Diver,Equipment),wearing(Diver,Equipment),Time)],       [])]
cl(    terminates(takeOff(Diver,Equipment),wearing(Diver,Equipment),Time),       []).
 %  cl(    terminates(takeOff(Diver,Equipment),wearing(Diver,Equipment),Time),       []).
 %  % =================================.


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',192).
% [diver,equipment,time]
% Terminates(Lose(diver,equipment),Wearing(diver,equipment),time).
%~ From E:
%~ 
%~ terminates_at(lose(Diver,Equipment),wearing(Diver,Equipment),Time)
%~ cpc :- terminates(lose(Diver,Equipment),wearing(Diver,Equipment),Time)
%~ ooo :- [   cl(    [   terminates(lose(Diver,Equipment),wearing(Diver,Equipment),Time)],       [])]
cl(    terminates(lose(Diver,Equipment),wearing(Diver,Equipment),Time),       []).
 %  cl(    terminates(lose(Diver,Equipment),wearing(Diver,Equipment),Time),       []).
 %  % =================================.

% fluent Vertical(diver)
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',194).
%~ From E:
%~ 
%~ fluent( vertical(diver))
%~ cpc :- fluents([vertical/1])
%~ ooo :- [   cl([fluents([vertical/1])],[])]
%~ cpc :- mpred_prop(vertical(diver),fluent)
%~ ooo :- [   cl([mpred_prop(vertical(diver),fluent)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',194).
cl(mpred_prop(vertical(diver),fluent),[]),cl(fluents(vertical/1),[]).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',197).
% fluent HorizontalDown(diver)
%~ From E:
%~ 
%~ fluent( horizontalDown(diver))
%~ cpc :- fluents([horizontalDown/1])
%~ ooo :- [   cl([fluents([horizontalDown/1])],[])]
%~ cpc :- mpred_prop(horizontalDown(diver),fluent)
%~ ooo :- [   cl([mpred_prop(horizontalDown(diver),fluent)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',197).
cl(mpred_prop(horizontalDown(diver),fluent),[]),cl(fluents(horizontalDown/1),[]).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',199).
% fluent Inverted(diver)
%~ From E:
%~ 
%~ fluent( inverted(diver))
%~ cpc :- fluents([inverted/1])
%~ ooo :- [   cl([fluents([inverted/1])],[])]
%~ cpc :- mpred_prop(inverted(diver),fluent)
%~ ooo :- [   cl([mpred_prop(inverted(diver),fluent)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',199).
cl(mpred_prop(inverted(diver),fluent),[]),cl(fluents(inverted/1),[]).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',201).
% fluent HorizontalUp(diver)
%~ From E:
%~ 
%~ fluent( horizontalUp(diver))
%~ cpc :- fluents([horizontalUp/1])
%~ ooo :- [   cl([fluents([horizontalUp/1])],[])]
%~ cpc :- mpred_prop(horizontalUp(diver),fluent)
%~ ooo :- [   cl([mpred_prop(horizontalUp(diver),fluent)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',201).
cl(mpred_prop(horizontalUp(diver),fluent),[]),cl(fluents(horizontalUp/1),[]).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',203).
% xor Vertical, HorizontalDown, Inverted, HorizontalUp
%~ From E:
%~ 
%~ xor( [vertical,horizontalDown,inverted,horizontalUp])
%~ cpc :- xor( [vertical,horizontalDown,inverted,horizontalUp])
%~ ooo :- [   cl([xor([vertical,horizontalDown,inverted,horizontalUp])],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',203).
cl(xor((vertical,horizontalDown,inverted,horizontalUp)),[]).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',205).
% event RotatePitch(diver)
%~ From E:
%~ 
%~ event( rotatePitch(diver))
%~ cpc :- events([rotatePitch/1])
%~ ooo :- [   cl([events([rotatePitch/1])],[])]
%~ cpc :- actions([rotatePitch/1])
%~ ooo :- [   cl([actions([rotatePitch/1])],[])]
%~ cpc :- mpred_prop(rotatePitch(diver),action)
%~ ooo :- [   cl([mpred_prop(rotatePitch(diver),action)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',205).
( cl(events(rotatePitch/1),[])  ,    cl(mpred_prop(rotatePitch(diver),action),[]) ,     cl(actions(rotatePitch/1),[])).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',207).
% [diver,time]
% HoldsAt(Vertical(diver),time) ->
% Initiates(RotatePitch(diver),HorizontalDown(diver),time).
%~ From E:
%~ 
%~ holds(vertical(Diver),Time) ->         initiates_at(rotatePitch(Diver),horizontalDown(Diver),Time)
%~ cpc :- holds(vertical(Diver),Time)
%~ ooo :- [   cl([holds(vertical(Diver),Time)],[])]
%~ cpc :- if(    initiates(rotatePitch(Diver),at(horizontalDown(Diver),Time)),       cl([holds(vertical(Diver),Time)],[]))
%~ ooo :- [   cl(    [   if(    initiates(rotatePitch(Diver),at(horizontalDown(Diver),Time)),       cl([holds(vertical(Diver),Time)],[]))],       [])]
cl(    (     rotatePitch(Diver)initiates horizontalDown(Diver)at Time if     cl(holds(vertical(Diver),Time),[])),       []).
 %  cl(    if(    initiates(rotatePitch(Diver),at(horizontalDown(Diver),Time)),       cl(holds(vertical(Diver),Time),[])),       []).
 %  % =================================.


% [diver,time]
% HoldsAt(HorizontalDown(diver),time) ->
% Initiates(RotatePitch(diver),Inverted(diver),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',212).
%~ From E:
%~ 
%~ holds(horizontalDown(Diver),Time) ->         initiates_at(rotatePitch(Diver),inverted(Diver),Time)
%~ cpc :- holds(horizontalDown(Diver),Time)
%~ ooo :- [   cl([holds(horizontalDown(Diver),Time)],[])]
%~ cpc :- if(    initiates(rotatePitch(Diver),at(inverted(Diver),Time)),       cl([holds(horizontalDown(Diver),Time)],[]))
%~ ooo :- [   cl(    [   if(    initiates(rotatePitch(Diver),at(inverted(Diver),Time)),       cl([holds(horizontalDown(Diver),Time)],[]))],       [])]
cl(    (     rotatePitch(Diver)initiates inverted(Diver)at Time if     cl(holds(horizontalDown(Diver),Time),[])),       []).
 %  cl(    if(    initiates(rotatePitch(Diver),at(inverted(Diver),Time)),       cl(holds(horizontalDown(Diver),Time),[])),       []).
 %  % =================================.


% [diver,time]
% HoldsAt(HorizontalDown(diver),time) ->
% Terminates(RotatePitch(diver),HorizontalDown(diver),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',216).
%~ From E:
%~ 
%~ holds(horizontalDown(Diver),Time) ->         terminates_at(rotatePitch(Diver),horizontalDown(Diver),Time)
%~ cpc :- holds(horizontalDown(Diver),Time)
%~ ooo :- [   cl([holds(horizontalDown(Diver),Time)],[])]
%~ cpc :- if(    terminates(rotatePitch(Diver),at(horizontalDown(Diver),Time)),       cl([holds(horizontalDown(Diver),Time)],[]))
%~ ooo :- [   cl(    [   if(    terminates(rotatePitch(Diver),at(horizontalDown(Diver),Time)),       cl([holds(horizontalDown(Diver),Time)],[]))],       [])]
cl(    (     rotatePitch(Diver)terminates horizontalDown(Diver)at Time if     cl(holds(horizontalDown(Diver),Time),[])),       []).
 %  cl(    if(    terminates(rotatePitch(Diver),at(horizontalDown(Diver),Time)),       cl(holds(horizontalDown(Diver),Time),[])),       []).
 %  % =================================.


% [diver,time]
% HoldsAt(Inverted(diver),time) ->
% Initiates(RotatePitch(diver),HorizontalUp(diver),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',220).
%~ From E:
%~ 
%~ holds(inverted(Diver),Time) ->         initiates_at(rotatePitch(Diver),horizontalUp(Diver),Time)
%~ cpc :- holds(inverted(Diver),Time)
%~ ooo :- [   cl([holds(inverted(Diver),Time)],[])]
%~ cpc :- if(    initiates(rotatePitch(Diver),at(horizontalUp(Diver),Time)),       cl([holds(inverted(Diver),Time)],[]))
%~ ooo :- [   cl(    [   if(    initiates(rotatePitch(Diver),at(horizontalUp(Diver),Time)),       cl([holds(inverted(Diver),Time)],[]))],       [])]
cl(    (     rotatePitch(Diver)initiates horizontalUp(Diver)at Time if     cl(holds(inverted(Diver),Time),[])),       []).
 %  cl(    if(    initiates(rotatePitch(Diver),at(horizontalUp(Diver),Time)),       cl(holds(inverted(Diver),Time),[])),       []).
 %  % =================================.


% [diver,time]
% HoldsAt(Inverted(diver),time) ->
% Terminates(RotatePitch(diver),Inverted(diver),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',224).
%~ From E:
%~ 
%~ holds(inverted(Diver),Time) ->         terminates_at(rotatePitch(Diver),inverted(Diver),Time)
%~ cpc :- holds(inverted(Diver),Time)
%~ ooo :- [   cl([holds(inverted(Diver),Time)],[])]
%~ cpc :- if(    terminates(rotatePitch(Diver),at(inverted(Diver),Time)),       cl([holds(inverted(Diver),Time)],[]))
%~ ooo :- [   cl(    [   if(    terminates(rotatePitch(Diver),at(inverted(Diver),Time)),       cl([holds(inverted(Diver),Time)],[]))],       [])]
cl(    (     rotatePitch(Diver)terminates inverted(Diver)at Time if     cl(holds(inverted(Diver),Time),[])),       []).
 %  cl(    if(    terminates(rotatePitch(Diver),at(inverted(Diver),Time)),       cl(holds(inverted(Diver),Time),[])),       []).
 %  % =================================.


% [diver,time]
% HoldsAt(HorizontalUp(diver),time) ->
% Initiates(RotatePitch(diver),Vertical(diver),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',228).
%~ From E:
%~ 
%~ holds(horizontalUp(Diver),Time) ->         initiates_at(rotatePitch(Diver),vertical(Diver),Time)
%~ cpc :- holds(horizontalUp(Diver),Time)
%~ ooo :- [   cl([holds(horizontalUp(Diver),Time)],[])]
%~ cpc :- if(    initiates(rotatePitch(Diver),at(vertical(Diver),Time)),       cl([holds(horizontalUp(Diver),Time)],[]))
%~ ooo :- [   cl(    [   if(    initiates(rotatePitch(Diver),at(vertical(Diver),Time)),       cl([holds(horizontalUp(Diver),Time)],[]))],       [])]
cl(    (     rotatePitch(Diver)initiates vertical(Diver)at Time if     cl(holds(horizontalUp(Diver),Time),[])),       []).
 %  cl(    if(    initiates(rotatePitch(Diver),at(vertical(Diver),Time)),       cl(holds(horizontalUp(Diver),Time),[])),       []).
 %  % =================================.


% [diver,time]
% HoldsAt(HorizontalUp(diver),time) ->
% Terminates(RotatePitch(diver),HorizontalUp(diver),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',232).
%~ From E:
%~ 
%~ holds(horizontalUp(Diver),Time) ->         terminates_at(rotatePitch(Diver),horizontalUp(Diver),Time)
%~ cpc :- holds(horizontalUp(Diver),Time)
%~ ooo :- [   cl([holds(horizontalUp(Diver),Time)],[])]
%~ cpc :- if(    terminates(rotatePitch(Diver),at(horizontalUp(Diver),Time)),       cl([holds(horizontalUp(Diver),Time)],[]))
%~ ooo :- [   cl(    [   if(    terminates(rotatePitch(Diver),at(horizontalUp(Diver),Time)),       cl([holds(horizontalUp(Diver),Time)],[]))],       [])]
cl(    (     rotatePitch(Diver)terminates horizontalUp(Diver)at Time if     cl(holds(horizontalUp(Diver),Time),[])),       []).
 %  cl(    if(    terminates(rotatePitch(Diver),at(horizontalUp(Diver),Time)),       cl(holds(horizontalUp(Diver),Time),[])),       []).
 %  % =================================.

% event RotateYaw(diver)
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',234).
%~ From E:
%~ 
%~ event( rotateYaw(diver))
%~ cpc :- events([rotateYaw/1])
%~ ooo :- [   cl([events([rotateYaw/1])],[])]
%~ cpc :- actions([rotateYaw/1])
%~ ooo :- [   cl([actions([rotateYaw/1])],[])]
%~ cpc :- mpred_prop(rotateYaw(diver),action)
%~ ooo :- [   cl([mpred_prop(rotateYaw(diver),action)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',234).
( cl(events(rotateYaw/1),[])  ,    cl(mpred_prop(rotateYaw(diver),action),[]) ,     cl(actions(rotateYaw/1),[])).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',237).
%; try taking out Holding condition here
% [diver,time]
% Happens(Ascend1(diver),time) &
% !Happens(RapidAscendToSurface(diver),time) &
% !({diver1} HoldsAt(Holding(diver,diver1),time)) ->
% Happens(RotateYaw(diver),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',239).
%~ From E:
%~ 
%~ ( happens(ascend1(Diver),Time)  ,    not( happens(rapidAscendToSurface(Diver),Time)) ,     not( thereExists(Diver1,holds(holding(Diver,Diver1),Time)))) ->         happens(rotateYaw(Diver),Time)
%~ cpc :- ( happens(ascend1(Diver),Time)  ,    not( happens(rapidAscendToSurface(Diver),Time)) ,     not( thereExists(Diver1,holds(holding(Diver,Diver1),Time)))) ->         happens(rotateYaw(Diver),Time)
%~ ooo :- [   cl(    [   exists( Diver1,   ( happens(rotateYaw(Diver),Time)  ;    not( happens(ascend1(Diver),Time)) ;     happens(rapidAscendToSurface(Diver),Time) ;     holds(holding(Diver,Diver1),Time)))],       [])]
cl(    exists( Diver1,   ( happens(rotateYaw(Diver),Time)  ;    not( happens(ascend1(Diver),Time)) ;     happens(rapidAscendToSurface(Diver),Time) ;     holds(holding(Diver,Diver1),Time))),       []).
 %  ( cl(    exists( Diver1,   ( happens(rotateYaw(Diver),Time)  ;    not( happens(ascend1(Diver),Time)) ;     happens(rapidAscendToSurface(Diver),Time) ;     holds(holding(Diver,Diver1),Time))),       [])).
 %  % =================================.

% fluent UnderWater(object)
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',243).
%~ From E:
%~ 
%~ fluent( underWater(object))
%~ cpc :- fluents([underWater/1])
%~ ooo :- [   cl([fluents([underWater/1])],[])]
%~ cpc :- mpred_prop(underWater(object),fluent)
%~ ooo :- [   cl([mpred_prop(underWater(object),fluent)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',243).
cl(mpred_prop(underWater(object),fluent),[]),cl(fluents(underWater/1),[]).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',246).
% [object,depth,time]
% depth>% 0 &
% HoldsAt(AtDepth(object,depth),time) ->
% HoldsAt(UnderWater(object),time).
%~ From E:
%~ 
%~ Depth>0,holds(atDepth(Object,Depth),Time) ->         holds(underWater(Object),Time)
%~ cpc :- Depth>0,holds(atDepth(Object,Depth),Time) ->         holds(underWater(Object),Time)
%~ ooo :- [   cl(    [   holds(underWater(Object),Time)],       [      comparison(Depth,0,>),           holds(atDepth(Object,Depth),Time)])]
cl(    holds(underWater(Object),Time),       comparison(Depth,0,>),holds(atDepth(Object,Depth),Time)).
 %  cl(    holds(underWater(Object),Time),       comparison(Depth,0,>),holds(atDepth(Object,Depth),Time)).
 %  % =================================.

% event EnterWater(object)
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',250).
%~ From E:
%~ 
%~ event( enterWater(object))
%~ cpc :- events([enterWater/1])
%~ ooo :- [   cl([events([enterWater/1])],[])]
%~ cpc :- mpred_prop(enterWater(object),event)
%~ ooo :- [   cl([mpred_prop(enterWater(object),event)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',250).
cl(mpred_prop(enterWater(object),event),[]),cl(events(enterWater/1),[]).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',253).
% event Surface(object)
%~ From E:
%~ 
%~ event( surface(object))
%~ cpc :- events([surface/1])
%~ ooo :- [   cl([events([surface/1])],[])]
%~ cpc :- mpred_prop(surface(object),event)
%~ ooo :- [   cl([mpred_prop(surface(object),event)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',253).
cl(mpred_prop(surface(object),event),[]),cl(events(surface/1),[]).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',255).
% [object,time]
% Initiates(EnterWater(object),UnderWater(object),time).
%~ From E:
%~ 
%~ initiates_at(enterWater(Object),underWater(Object),Time)
%~ cpc :- initiates(enterWater(Object),underWater(Object),Time)
%~ ooo :- [   cl([initiates(enterWater(Object),underWater(Object),Time)],[])]
cl(initiates(enterWater(Object),underWater(Object),Time),[]).
 %  cl(initiates(enterWater(Object),underWater(Object),Time),[]).
 %  % =================================.


% [diver,time]
% Happens(EnterWater(diver),time) ->
% !{diver1} HoldsAt(Holding(diver1,diver),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',259).
%~ From E:
%~ 
%~ happens(enterWater(Diver),Time) ->         not( thereExists(Diver1,holds(holding(Diver1,Diver),Time)))
%~ cpc :- happens(enterWater(Diver),Time) ->         not( thereExists(Diver1,holds(holding(Diver1,Diver),Time)))
%~ ooo :- [   cl(    [   exists( Diver1,   (     not( holds(holding(Diver1,Diver),Time)) ;     not( happens(enterWater(Diver),Time))))],       [])]
cl(    exists( Diver1,   (     not( holds(holding(Diver1,Diver),Time)) ;     not( happens(enterWater(Diver),Time)))),       []).
 %  ( cl(    exists( Diver1,   (     not( holds(holding(Diver1,Diver),Time)) ;     not( happens(enterWater(Diver),Time)))),       [])).
 %  % =================================.


% [object,depth,time]
% depth=% 0 ->
% Initiates(EnterWater(object),AtDepth(object,depth),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',263).
%~ From E:
%~ 
%~ Depth=0 ->         initiates_at(enterWater(Object),atDepth(Object,Depth),Time)
%~ cpc :- Depth=0
%~ ooo :- [   cl([equals(Depth,0)],[])]
%~ cpc :- if(    initiates(enterWater(Object),at(atDepth(Object,Depth),Time)),       cl([equals(Depth,0)],[]))
%~ ooo :- [   cl(    [   if(    initiates(enterWater(Object),at(atDepth(Object,Depth),Time)),       cl([equals(Depth,0)],[]))],       [])]
cl(    (     enterWater(Object)initiates atDepth(Object,Depth)at Time if     cl(equals(Depth,0),[])),       []).
 %  cl(    if(    initiates(enterWater(Object),at(atDepth(Object,Depth),Time)),       cl(equals(Depth,0),[])),       []).
 %  % =================================.


% [object,time]
% Terminates(Surface(object),UnderWater(object),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',267).
%~ From E:
%~ 
%~ terminates_at(surface(Object),underWater(Object),Time)
%~ cpc :- terminates(surface(Object),underWater(Object),Time)
%~ ooo :- [   cl([terminates(surface(Object),underWater(Object),Time)],[])]
cl(terminates(surface(Object),underWater(Object),Time),[]).
 %  cl(terminates(surface(Object),underWater(Object),Time),[]).
 %  % =================================.


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',269).
% [diver,time]
% Terminates(Surface(diver),PositivelyBuoyant(diver),time).
%~ From E:
%~ 
%~ terminates_at(surface(Diver),positivelyBuoyant(Diver),Time)
%~ cpc :- terminates(surface(Diver),positivelyBuoyant(Diver),Time)
%~ ooo :- [   cl([terminates(surface(Diver),positivelyBuoyant(Diver),Time)],[])]
cl(terminates(surface(Diver),positivelyBuoyant(Diver),Time),[]).
 %  cl(terminates(surface(Diver),positivelyBuoyant(Diver),Time),[]).
 %  % =================================.


% [diver,time]
% Terminates(Surface(diver),NegativelyBuoyant(diver),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',273).
%~ From E:
%~ 
%~ terminates_at(surface(Diver),negativelyBuoyant(Diver),Time)
%~ cpc :- terminates(surface(Diver),negativelyBuoyant(Diver),Time)
%~ ooo :- [   cl([terminates(surface(Diver),negativelyBuoyant(Diver),Time)],[])]
cl(terminates(surface(Diver),negativelyBuoyant(Diver),Time),[]).
 %  cl(terminates(surface(Diver),negativelyBuoyant(Diver),Time),[]).
 %  % =================================.


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',275).
% [diver,time]
% Terminates(Surface(diver),NeutrallyBuoyant(diver),time).
%~ From E:
%~ 
%~ terminates_at(surface(Diver),neutrallyBuoyant(Diver),Time)
%~ cpc :- terminates(surface(Diver),neutrallyBuoyant(Diver),Time)
%~ ooo :- [   cl([terminates(surface(Diver),neutrallyBuoyant(Diver),Time)],[])]
cl(terminates(surface(Diver),neutrallyBuoyant(Diver),Time),[]).
 %  cl(terminates(surface(Diver),neutrallyBuoyant(Diver),Time),[]).
 %  % =================================.


% [object,depth,time]
% Terminates(Surface(object),AtDepth(object,depth),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',279).
%~ From E:
%~ 
%~ terminates_at(surface(Object),atDepth(Object,Depth),Time)
%~ cpc :- terminates(surface(Object),atDepth(Object,Depth),Time)
%~ ooo :- [   cl([terminates(surface(Object),atDepth(Object,Depth),Time)],[])]
cl(terminates(surface(Object),atDepth(Object,Depth),Time),[]).
 %  cl(terminates(surface(Object),atDepth(Object,Depth),Time),[]).
 %  % =================================.


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',281).
% [diver,time]
 % Happens(EnterWater(diver),time) ->
% HoldsAt(Vertical(diver),time).
%~ From E:
%~ 
%~ happens(enterWater(Diver),Time) ->         holds(vertical(Diver),Time)
%~ cpc :- happens(enterWater(Diver),Time) ->         holds(vertical(Diver),Time)
%~ ooo :- [   cl(    [   holds(vertical(Diver),Time)],       [      happens(enterWater(Diver),Time)])]
cl(holds(vertical(Diver),Time),happens(enterWater(Diver),Time)).
 %  cl(holds(vertical(Diver),Time),happens(enterWater(Diver),Time)).
 %  % =================================.

% fluent StandingOn(diver,boat)
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',283).
%~ From E:
%~ 
%~ fluent( standingOn(diver,boat))
%~ cpc :- fluents([standingOn/2])
%~ ooo :- [   cl([fluents([standingOn/2])],[])]
%~ cpc :- mpred_prop(standingOn(diver,boat),fluent)
%~ ooo :- [   cl([mpred_prop(standingOn(diver,boat),fluent)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',283).
cl(mpred_prop(standingOn(diver,boat),fluent),[]),cl(fluents(standingOn/2),[]).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',286).
% event StandOn(diver,boat)
%~ From E:
%~ 
%~ event( standOn(diver,boat))
%~ cpc :- events([standOn/2])
%~ ooo :- [   cl([events([standOn/2])],[])]
%~ cpc :- actions([standOn/2])
%~ ooo :- [   cl([actions([standOn/2])],[])]
%~ cpc :- mpred_prop(standOn(diver,boat),action)
%~ ooo :- [   cl([mpred_prop(standOn(diver,boat),action)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',286).
( cl(events(standOn/2),[])  ,    cl(mpred_prop(standOn(diver,boat),action),[]) ,     cl(actions(standOn/2),[])).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',288).
% [diver,boat,time]
% Terminates(EnterWater(diver),StandingOn(diver,boat),time).
%~ From E:
%~ 
%~ terminates_at(enterWater(Diver),standingOn(Diver,Boat),Time)
%~ cpc :- terminates(enterWater(Diver),standingOn(Diver,Boat),Time)
%~ ooo :- [   cl([terminates(enterWater(Diver),standingOn(Diver,Boat),Time)],[])]
cl(terminates(enterWater(Diver),standingOn(Diver,Boat),Time),[]).
 %  cl(terminates(enterWater(Diver),standingOn(Diver,Boat),Time),[]).
 %  % =================================.


% [diver,boat,time]
% Initiates(StandOn(diver,boat),StandingOn(diver,boat),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',292).
%~ From E:
%~ 
%~ initiates_at(standOn(Diver,Boat),standingOn(Diver,Boat),Time)
%~ cpc :- initiates(standOn(Diver,Boat),standingOn(Diver,Boat),Time)
%~ ooo :- [   cl(    [   initiates(standOn(Diver,Boat),standingOn(Diver,Boat),Time)],       [])]
cl(initiates(standOn(Diver,Boat),standingOn(Diver,Boat),Time),[]).
 %  cl(initiates(standOn(Diver,Boat),standingOn(Diver,Boat),Time),[]).
 %  % =================================.

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',294).
% fluent PositivelyBuoyant(diver)
%~ From E:
%~ 
%~ fluent( positivelyBuoyant(diver))
%~ cpc :- fluents([positivelyBuoyant/1])
%~ ooo :- [   cl([fluents([positivelyBuoyant/1])],[])]
%~ cpc :- mpred_prop(positivelyBuoyant(diver),fluent)
%~ ooo :- [   cl([mpred_prop(positivelyBuoyant(diver),fluent)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',294).
cl(mpred_prop(positivelyBuoyant(diver),fluent),[]),cl(fluents(positivelyBuoyant/1),[]).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',296).
% fluent NeutrallyBuoyant(diver)
%~ From E:
%~ 
%~ fluent( neutrallyBuoyant(diver))
%~ cpc :- fluents([neutrallyBuoyant/1])
%~ ooo :- [   cl([fluents([neutrallyBuoyant/1])],[])]
%~ cpc :- mpred_prop(neutrallyBuoyant(diver),fluent)
%~ ooo :- [   cl([mpred_prop(neutrallyBuoyant(diver),fluent)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',296).
cl(mpred_prop(neutrallyBuoyant(diver),fluent),[]),cl(fluents(neutrallyBuoyant/1),[]).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',298).
% fluent NegativelyBuoyant(diver)
%~ From E:
%~ 
%~ fluent( negativelyBuoyant(diver))
%~ cpc :- fluents([negativelyBuoyant/1])
%~ ooo :- [   cl([fluents([negativelyBuoyant/1])],[])]
%~ cpc :- mpred_prop(negativelyBuoyant(diver),fluent)
%~ ooo :- [   cl([mpred_prop(negativelyBuoyant(diver),fluent)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',298).
cl(mpred_prop(negativelyBuoyant(diver),fluent),[]),cl(fluents(negativelyBuoyant/1),[]).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',300).
% mutex PositivelyBuoyant, NeutrallyBuoyant, NegativelyBuoyant
%~ From E:
%~ 
%~ :-( call_pel_directive( mutex(positivelyBuoyant)))
:-( call_pel_directive( mutex(positivelyBuoyant))).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',300).
%~ From E:
%~ 
%~ :-( call_pel_directive( mutex(neutrallyBuoyant)))
:-( call_pel_directive( mutex(neutrallyBuoyant))).
%~ From E:
%~ 
%~ :-( call_pel_directive( mutex(negativelyBuoyant)))
:-( call_pel_directive( mutex(negativelyBuoyant))).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',302).
% [diver,time]
% HoldsAt(PositivelyBuoyant(diver),time) ->
% HoldsAt(UnderWater(diver),time).
%~ From E:
%~ 
%~ holds(positivelyBuoyant(Diver),Time) ->         holds(underWater(Diver),Time)
%~ cpc :- holds(positivelyBuoyant(Diver),Time) ->         holds(underWater(Diver),Time)
%~ ooo :- [   cl(    [   holds(underWater(Diver),Time)],       [      holds(positivelyBuoyant(Diver),Time)])]
cl(holds(underWater(Diver),Time),holds(positivelyBuoyant(Diver),Time)).
 %  cl(holds(underWater(Diver),Time),holds(positivelyBuoyant(Diver),Time)).
 %  % =================================.


% [diver,time]
% HoldsAt(NeutrallyBuoyant(diver),time) ->
% HoldsAt(UnderWater(diver),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',307).
%~ From E:
%~ 
%~ holds(neutrallyBuoyant(Diver),Time) ->         holds(underWater(Diver),Time)
%~ cpc :- holds(neutrallyBuoyant(Diver),Time) ->         holds(underWater(Diver),Time)
%~ ooo :- [   cl(    [   holds(underWater(Diver),Time)],       [      holds(neutrallyBuoyant(Diver),Time)])]
cl(holds(underWater(Diver),Time),holds(neutrallyBuoyant(Diver),Time)).
 %  cl(holds(underWater(Diver),Time),holds(neutrallyBuoyant(Diver),Time)).
 %  % =================================.


% [diver,time]
% HoldsAt(NegativelyBuoyant(diver),time) ->
% HoldsAt(UnderWater(diver),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',311).
%~ From E:
%~ 
%~ holds(negativelyBuoyant(Diver),Time) ->         holds(underWater(Diver),Time)
%~ cpc :- holds(negativelyBuoyant(Diver),Time) ->         holds(underWater(Diver),Time)
%~ ooo :- [   cl(    [   holds(underWater(Diver),Time)],       [      holds(negativelyBuoyant(Diver),Time)])]
cl(holds(underWater(Diver),Time),holds(negativelyBuoyant(Diver),Time)).
 %  cl(holds(underWater(Diver),Time),holds(negativelyBuoyant(Diver),Time)).
 %  % =================================.

% event PressDeflateButton(diver,bc)
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',313).
%~ From E:
%~ 
%~ event( pressDeflateButton(diver,bc))
%~ cpc :- events([pressDeflateButton/2])
%~ ooo :- [   cl([events([pressDeflateButton/2])],[])]
%~ cpc :- actions([pressDeflateButton/2])
%~ ooo :- [   cl([actions([pressDeflateButton/2])],[])]
%~ cpc :- mpred_prop(pressDeflateButton(diver,bc),action)
%~ ooo :- [   cl([mpred_prop(pressDeflateButton(diver,bc),action)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',313).
( cl(events(pressDeflateButton/2),[])  ,    cl(mpred_prop(pressDeflateButton(diver,bc),action),[]) ,     cl(actions(pressDeflateButton/2),[])).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',316).
% event PressDumpButton(diver,bc)
%~ From E:
%~ 
%~ event( pressDumpButton(diver,bc))
%~ cpc :- events([pressDumpButton/2])
%~ ooo :- [   cl([events([pressDumpButton/2])],[])]
%~ cpc :- actions([pressDumpButton/2])
%~ ooo :- [   cl([actions([pressDumpButton/2])],[])]
%~ cpc :- mpred_prop(pressDumpButton(diver,bc),action)
%~ ooo :- [   cl([mpred_prop(pressDumpButton(diver,bc),action)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',316).
( cl(events(pressDumpButton/2),[])  ,    cl(mpred_prop(pressDumpButton(diver,bc),action),[]) ,     cl(actions(pressDumpButton/2),[])).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',318).
% event PressInflateButton(diver,bc)
%~ From E:
%~ 
%~ event( pressInflateButton(diver,bc))
%~ cpc :- events([pressInflateButton/2])
%~ ooo :- [   cl([events([pressInflateButton/2])],[])]
%~ cpc :- actions([pressInflateButton/2])
%~ ooo :- [   cl([actions([pressInflateButton/2])],[])]
%~ cpc :- mpred_prop(pressInflateButton(diver,bc),action)
%~ ooo :- [   cl([mpred_prop(pressInflateButton(diver,bc),action)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',318).
( cl(events(pressInflateButton/2),[])  ,    cl(mpred_prop(pressInflateButton(diver,bc),action),[]) ,     cl(actions(pressInflateButton/2),[])).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',320).
% [diver,bc,time]
% Happens(PressDeflateButton(diver,bc),time) ->
% HoldsAt(Vertical(diver),time) &
% HoldsAt(UnderWater(bc),time).
%~ From E:
%~ 
%~ happens(pressDeflateButton(Diver,Bc),Time) ->         holds(vertical(Diver),Time),holds(underWater(Bc),Time)
%~ cpc :- happens(pressDeflateButton(Diver,Bc),Time) ->         holds(vertical(Diver),Time),holds(underWater(Bc),Time)
%~ ooo :- [   cl(    [   holds(vertical(Diver),Time)],       [      happens(pressDeflateButton(Diver,Bc),Time)]),     cl(      [   holds(underWater(Bc),Time)],           [        happens(pressDeflateButton(Diver,Bc),Time)])]
(     cl(      holds(vertical(Diver),Time),           happens(pressDeflateButton(Diver,Bc),Time)) ,     cl(      holds(underWater(Bc),Time),           happens(pressDeflateButton(Diver,Bc),Time))).
 %  (     cl(      holds(vertical(Diver),Time),           happens(pressDeflateButton(Diver,Bc),Time)) ,     cl(      holds(underWater(Bc),Time),           happens(pressDeflateButton(Diver,Bc),Time))).
 %  % =================================.


% [diver,bc,time]
% Happens(PressDumpButton(diver,bc),time) ->
% HoldsAt(Vertical(diver),time) &
% HoldsAt(UnderWater(bc),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',326).
%~ From E:
%~ 
%~ happens(pressDumpButton(Diver,Bc),Time) ->         holds(vertical(Diver),Time),holds(underWater(Bc),Time)
%~ cpc :- happens(pressDumpButton(Diver,Bc),Time) ->         holds(vertical(Diver),Time),holds(underWater(Bc),Time)
%~ ooo :- [   cl(    [   holds(vertical(Diver),Time)],       [      happens(pressDumpButton(Diver,Bc),Time)]),     cl(      [   holds(underWater(Bc),Time)],           [        happens(pressDumpButton(Diver,Bc),Time)])]
(     cl(      holds(vertical(Diver),Time),           happens(pressDumpButton(Diver,Bc),Time)) ,     cl(      holds(underWater(Bc),Time),           happens(pressDumpButton(Diver,Bc),Time))).
 %  (     cl(      holds(vertical(Diver),Time),           happens(pressDumpButton(Diver,Bc),Time)) ,     cl(      holds(underWater(Bc),Time),           happens(pressDumpButton(Diver,Bc),Time))).
 %  % =================================.


% [diver,bc,time]
 % Happens(PressDumpButton(diver,bc),time) ->
% HoldsAt(UncontrolledBuoyancy(diver),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',329).
%~ From E:
%~ 
%~ happens(pressDumpButton(Diver,Bc),Time) ->         holds(uncontrolledBuoyancy(Diver),Time)
%~ cpc :- happens(pressDumpButton(Diver,Bc),Time) ->         holds(uncontrolledBuoyancy(Diver),Time)
%~ ooo :- [   cl(    [   holds(uncontrolledBuoyancy(Diver),Time)],       [      happens(pressDumpButton(Diver,Bc),Time)])]
cl(    holds(uncontrolledBuoyancy(Diver),Time),       happens(pressDumpButton(Diver,Bc),Time)).
 %  cl(    holds(uncontrolledBuoyancy(Diver),Time),       happens(pressDumpButton(Diver,Bc),Time)).
 %  % =================================.


% [diver,bc,time]
% HoldsAt(Wearing(diver,bc),time) ->
% Initiates(PressDeflateButton(diver,bc),NegativelyBuoyant(diver),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',334).
%~ From E:
%~ 
%~ holds(wearing(Diver,Bc),Time) ->         initiates_at(pressDeflateButton(Diver,Bc),negativelyBuoyant(Diver),Time)
%~ cpc :- holds(wearing(Diver,Bc),Time)
%~ ooo :- [   cl([holds(wearing(Diver,Bc),Time)],[])]
%~ cpc :- if(    initiates(pressDeflateButton(Diver,Bc),at(negativelyBuoyant(Diver),Time)),       cl([holds(wearing(Diver,Bc),Time)],[]))
%~ ooo :- [   cl(    [   if(    initiates(pressDeflateButton(Diver,Bc),at(negativelyBuoyant(Diver),Time)),       cl([holds(wearing(Diver,Bc),Time)],[]))],       [])]
cl(    (     pressDeflateButton(Diver,Bc)initiates negativelyBuoyant(Diver)at Time if     cl(holds(wearing(Diver,Bc),Time),[])),       []).
 %  cl(    if(    initiates(pressDeflateButton(Diver,Bc),at(negativelyBuoyant(Diver),Time)),       cl(holds(wearing(Diver,Bc),Time),[])),       []).
 %  % =================================.


% [diver,bc,time]
% HoldsAt(Wearing(diver,bc),time) ->
% Terminates(PressDeflateButton(diver,bc),NeutrallyBuoyant(diver),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',338).
%~ From E:
%~ 
%~ holds(wearing(Diver,Bc),Time) ->         terminates_at(pressDeflateButton(Diver,Bc),neutrallyBuoyant(Diver),Time)
%~ cpc :- holds(wearing(Diver,Bc),Time)
%~ ooo :- [   cl([holds(wearing(Diver,Bc),Time)],[])]
%~ cpc :- if(    terminates(pressDeflateButton(Diver,Bc),at(neutrallyBuoyant(Diver),Time)),       cl([holds(wearing(Diver,Bc),Time)],[]))
%~ ooo :- [   cl(    [   if(    terminates(pressDeflateButton(Diver,Bc),at(neutrallyBuoyant(Diver),Time)),       cl([holds(wearing(Diver,Bc),Time)],[]))],       [])]
cl(    (     pressDeflateButton(Diver,Bc)terminates neutrallyBuoyant(Diver)at Time if     cl(holds(wearing(Diver,Bc),Time),[])),       []).
 %  cl(    if(    terminates(pressDeflateButton(Diver,Bc),at(neutrallyBuoyant(Diver),Time)),       cl(holds(wearing(Diver,Bc),Time),[])),       []).
 %  % =================================.


% [diver,bc,time]
% HoldsAt(Wearing(diver,bc),time) ->
% Terminates(PressDeflateButton(diver,bc),PositivelyBuoyant(diver),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',342).
%~ From E:
%~ 
%~ holds(wearing(Diver,Bc),Time) ->         terminates_at(pressDeflateButton(Diver,Bc),positivelyBuoyant(Diver),Time)
%~ cpc :- holds(wearing(Diver,Bc),Time)
%~ ooo :- [   cl([holds(wearing(Diver,Bc),Time)],[])]
%~ cpc :- if(    terminates(pressDeflateButton(Diver,Bc),at(positivelyBuoyant(Diver),Time)),       cl([holds(wearing(Diver,Bc),Time)],[]))
%~ ooo :- [   cl(    [   if(    terminates(pressDeflateButton(Diver,Bc),at(positivelyBuoyant(Diver),Time)),       cl([holds(wearing(Diver,Bc),Time)],[]))],       [])]
cl(    (     pressDeflateButton(Diver,Bc)terminates positivelyBuoyant(Diver)at Time if     cl(holds(wearing(Diver,Bc),Time),[])),       []).
 %  cl(    if(    terminates(pressDeflateButton(Diver,Bc),at(positivelyBuoyant(Diver),Time)),       cl(holds(wearing(Diver,Bc),Time),[])),       []).
 %  % =================================.


% [diver,bc,time]
% HoldsAt(Wearing(diver,bc),time) ->
% Initiates(PressDumpButton(diver,bc),NegativelyBuoyant(diver),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',346).
%~ From E:
%~ 
%~ holds(wearing(Diver,Bc),Time) ->         initiates_at(pressDumpButton(Diver,Bc),negativelyBuoyant(Diver),Time)
%~ cpc :- holds(wearing(Diver,Bc),Time)
%~ ooo :- [   cl([holds(wearing(Diver,Bc),Time)],[])]
%~ cpc :- if(    initiates(pressDumpButton(Diver,Bc),at(negativelyBuoyant(Diver),Time)),       cl([holds(wearing(Diver,Bc),Time)],[]))
%~ ooo :- [   cl(    [   if(    initiates(pressDumpButton(Diver,Bc),at(negativelyBuoyant(Diver),Time)),       cl([holds(wearing(Diver,Bc),Time)],[]))],       [])]
cl(    (     pressDumpButton(Diver,Bc)initiates negativelyBuoyant(Diver)at Time if     cl(holds(wearing(Diver,Bc),Time),[])),       []).
 %  cl(    if(    initiates(pressDumpButton(Diver,Bc),at(negativelyBuoyant(Diver),Time)),       cl(holds(wearing(Diver,Bc),Time),[])),       []).
 %  % =================================.


% [diver,bc,time]
% HoldsAt(Wearing(diver,bc),time) ->
% Terminates(PressDumpButton(diver,bc),NeutrallyBuoyant(diver),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',350).
%~ From E:
%~ 
%~ holds(wearing(Diver,Bc),Time) ->         terminates_at(pressDumpButton(Diver,Bc),neutrallyBuoyant(Diver),Time)
%~ cpc :- holds(wearing(Diver,Bc),Time)
%~ ooo :- [   cl([holds(wearing(Diver,Bc),Time)],[])]
%~ cpc :- if(    terminates(pressDumpButton(Diver,Bc),at(neutrallyBuoyant(Diver),Time)),       cl([holds(wearing(Diver,Bc),Time)],[]))
%~ ooo :- [   cl(    [   if(    terminates(pressDumpButton(Diver,Bc),at(neutrallyBuoyant(Diver),Time)),       cl([holds(wearing(Diver,Bc),Time)],[]))],       [])]
cl(    (     pressDumpButton(Diver,Bc)terminates neutrallyBuoyant(Diver)at Time if     cl(holds(wearing(Diver,Bc),Time),[])),       []).
 %  cl(    if(    terminates(pressDumpButton(Diver,Bc),at(neutrallyBuoyant(Diver),Time)),       cl(holds(wearing(Diver,Bc),Time),[])),       []).
 %  % =================================.


% [diver,bc,time]
% HoldsAt(Wearing(diver,bc),time) ->
% Terminates(PressDumpButton(diver,bc),PositivelyBuoyant(diver),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',354).
%~ From E:
%~ 
%~ holds(wearing(Diver,Bc),Time) ->         terminates_at(pressDumpButton(Diver,Bc),positivelyBuoyant(Diver),Time)
%~ cpc :- holds(wearing(Diver,Bc),Time)
%~ ooo :- [   cl([holds(wearing(Diver,Bc),Time)],[])]
%~ cpc :- if(    terminates(pressDumpButton(Diver,Bc),at(positivelyBuoyant(Diver),Time)),       cl([holds(wearing(Diver,Bc),Time)],[]))
%~ ooo :- [   cl(    [   if(    terminates(pressDumpButton(Diver,Bc),at(positivelyBuoyant(Diver),Time)),       cl([holds(wearing(Diver,Bc),Time)],[]))],       [])]
cl(    (     pressDumpButton(Diver,Bc)terminates positivelyBuoyant(Diver)at Time if     cl(holds(wearing(Diver,Bc),Time),[])),       []).
 %  cl(    if(    terminates(pressDumpButton(Diver,Bc),at(positivelyBuoyant(Diver),Time)),       cl(holds(wearing(Diver,Bc),Time),[])),       []).
 %  % =================================.


% [diver,bc,time]
% HoldsAt(Wearing(diver,bc),time) ->
% Initiates(PressInflateButton(diver,bc),NeutrallyBuoyant(diver),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',358).
%~ From E:
%~ 
%~ holds(wearing(Diver,Bc),Time) ->         initiates_at(pressInflateButton(Diver,Bc),neutrallyBuoyant(Diver),Time)
%~ cpc :- holds(wearing(Diver,Bc),Time)
%~ ooo :- [   cl([holds(wearing(Diver,Bc),Time)],[])]
%~ cpc :- if(    initiates(pressInflateButton(Diver,Bc),at(neutrallyBuoyant(Diver),Time)),       cl([holds(wearing(Diver,Bc),Time)],[]))
%~ ooo :- [   cl(    [   if(    initiates(pressInflateButton(Diver,Bc),at(neutrallyBuoyant(Diver),Time)),       cl([holds(wearing(Diver,Bc),Time)],[]))],       [])]
cl(    (     pressInflateButton(Diver,Bc)initiates neutrallyBuoyant(Diver)at Time if     cl(holds(wearing(Diver,Bc),Time),[])),       []).
 %  cl(    if(    initiates(pressInflateButton(Diver,Bc),at(neutrallyBuoyant(Diver),Time)),       cl(holds(wearing(Diver,Bc),Time),[])),       []).
 %  % =================================.


% [diver,bc,time]
% HoldsAt(Wearing(diver,bc),time) ->
% Terminates(PressInflateButton(diver,bc),PositivelyBuoyant(diver),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',362).
%~ From E:
%~ 
%~ holds(wearing(Diver,Bc),Time) ->         terminates_at(pressInflateButton(Diver,Bc),positivelyBuoyant(Diver),Time)
%~ cpc :- holds(wearing(Diver,Bc),Time)
%~ ooo :- [   cl([holds(wearing(Diver,Bc),Time)],[])]
%~ cpc :- if(    terminates(pressInflateButton(Diver,Bc),at(positivelyBuoyant(Diver),Time)),       cl([holds(wearing(Diver,Bc),Time)],[]))
%~ ooo :- [   cl(    [   if(    terminates(pressInflateButton(Diver,Bc),at(positivelyBuoyant(Diver),Time)),       cl([holds(wearing(Diver,Bc),Time)],[]))],       [])]
cl(    (     pressInflateButton(Diver,Bc)terminates positivelyBuoyant(Diver)at Time if     cl(holds(wearing(Diver,Bc),Time),[])),       []).
 %  cl(    if(    terminates(pressInflateButton(Diver,Bc),at(positivelyBuoyant(Diver),Time)),       cl(holds(wearing(Diver,Bc),Time),[])),       []).
 %  % =================================.


% [diver,bc,time]
% HoldsAt(Wearing(diver,bc),time) ->
% Terminates(PressInflateButton(diver,bc),NegativelyBuoyant(diver),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',366).
%~ From E:
%~ 
%~ holds(wearing(Diver,Bc),Time) ->         terminates_at(pressInflateButton(Diver,Bc),negativelyBuoyant(Diver),Time)
%~ cpc :- holds(wearing(Diver,Bc),Time)
%~ ooo :- [   cl([holds(wearing(Diver,Bc),Time)],[])]
%~ cpc :- if(    terminates(pressInflateButton(Diver,Bc),at(negativelyBuoyant(Diver),Time)),       cl([holds(wearing(Diver,Bc),Time)],[]))
%~ ooo :- [   cl(    [   if(    terminates(pressInflateButton(Diver,Bc),at(negativelyBuoyant(Diver),Time)),       cl([holds(wearing(Diver,Bc),Time)],[]))],       [])]
cl(    (     pressInflateButton(Diver,Bc)terminates negativelyBuoyant(Diver)at Time if     cl(holds(wearing(Diver,Bc),Time),[])),       []).
 %  cl(    if(    terminates(pressInflateButton(Diver,Bc),at(negativelyBuoyant(Diver),Time)),       cl(holds(wearing(Diver,Bc),Time),[])),       []).
 %  % =================================.


% [diver,weight,time]
% HoldsAt(Wearing(diver,weight),time) ->
% Initiates(TakeOff(diver,weight),PositivelyBuoyant(diver),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',370).
%~ From E:
%~ 
%~ holds(wearing(Diver,Weight),Time) ->         initiates_at(takeOff(Diver,Weight),positivelyBuoyant(Diver),Time)
%~ cpc :- holds(wearing(Diver,Weight),Time)
%~ ooo :- [   cl([holds(wearing(Diver,Weight),Time)],[])]
%~ cpc :- if(    initiates(takeOff(Diver,Weight),at(positivelyBuoyant(Diver),Time)),       cl([holds(wearing(Diver,Weight),Time)],[]))
%~ ooo :- [   cl(    [   if(    initiates(takeOff(Diver,Weight),at(positivelyBuoyant(Diver),Time)),       cl([holds(wearing(Diver,Weight),Time)],[]))],       [])]
cl(    (     takeOff(Diver,Weight)initiates positivelyBuoyant(Diver)at Time if     cl(holds(wearing(Diver,Weight),Time),[])),       []).
 %  cl(    if(    initiates(takeOff(Diver,Weight),at(positivelyBuoyant(Diver),Time)),       cl(holds(wearing(Diver,Weight),Time),[])),       []).
 %  % =================================.


% [diver,weight,time]
% HoldsAt(Wearing(diver,weight),time) ->
% Terminates(TakeOff(diver,weight),NegativelyBuoyant(diver),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',374).
%~ From E:
%~ 
%~ holds(wearing(Diver,Weight),Time) ->         terminates_at(takeOff(Diver,Weight),negativelyBuoyant(Diver),Time)
%~ cpc :- holds(wearing(Diver,Weight),Time)
%~ ooo :- [   cl([holds(wearing(Diver,Weight),Time)],[])]
%~ cpc :- if(    terminates(takeOff(Diver,Weight),at(negativelyBuoyant(Diver),Time)),       cl([holds(wearing(Diver,Weight),Time)],[]))
%~ ooo :- [   cl(    [   if(    terminates(takeOff(Diver,Weight),at(negativelyBuoyant(Diver),Time)),       cl([holds(wearing(Diver,Weight),Time)],[]))],       [])]
cl(    (     takeOff(Diver,Weight)terminates negativelyBuoyant(Diver)at Time if     cl(holds(wearing(Diver,Weight),Time),[])),       []).
 %  cl(    if(    terminates(takeOff(Diver,Weight),at(negativelyBuoyant(Diver),Time)),       cl(holds(wearing(Diver,Weight),Time),[])),       []).
 %  % =================================.


% [diver,weight,time]
% HoldsAt(Wearing(diver,weight),time) ->
% Terminates(TakeOff(diver,weight),NeutrallyBuoyant(diver),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',378).
%~ From E:
%~ 
%~ holds(wearing(Diver,Weight),Time) ->         terminates_at(takeOff(Diver,Weight),neutrallyBuoyant(Diver),Time)
%~ cpc :- holds(wearing(Diver,Weight),Time)
%~ ooo :- [   cl([holds(wearing(Diver,Weight),Time)],[])]
%~ cpc :- if(    terminates(takeOff(Diver,Weight),at(neutrallyBuoyant(Diver),Time)),       cl([holds(wearing(Diver,Weight),Time)],[]))
%~ ooo :- [   cl(    [   if(    terminates(takeOff(Diver,Weight),at(neutrallyBuoyant(Diver),Time)),       cl([holds(wearing(Diver,Weight),Time)],[]))],       [])]
cl(    (     takeOff(Diver,Weight)terminates neutrallyBuoyant(Diver)at Time if     cl(holds(wearing(Diver,Weight),Time),[])),       []).
 %  cl(    if(    terminates(takeOff(Diver,Weight),at(neutrallyBuoyant(Diver),Time)),       cl(holds(wearing(Diver,Weight),Time),[])),       []).
 %  % =================================.

% fluent UncontrolledBuoyancy(diver)
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',380).
%~ From E:
%~ 
%~ fluent( uncontrolledBuoyancy(diver))
%~ cpc :- fluents([uncontrolledBuoyancy/1])
%~ ooo :- [   cl([fluents([uncontrolledBuoyancy/1])],[])]
%~ cpc :- mpred_prop(uncontrolledBuoyancy(diver),fluent)
%~ ooo :- [   cl([mpred_prop(uncontrolledBuoyancy(diver),fluent)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',380).
cl(mpred_prop(uncontrolledBuoyancy(diver),fluent),[]),cl(fluents(uncontrolledBuoyancy/1),[]).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',383).
% event LoseBuoyancyControl(diver)
%~ From E:
%~ 
%~ event( loseBuoyancyControl(diver))
%~ cpc :- events([loseBuoyancyControl/1])
%~ ooo :- [   cl([events([loseBuoyancyControl/1])],[])]
%~ cpc :- actions([loseBuoyancyControl/1])
%~ ooo :- [   cl([actions([loseBuoyancyControl/1])],[])]
%~ cpc :- mpred_prop(loseBuoyancyControl(diver),action)
%~ ooo :- [   cl([mpred_prop(loseBuoyancyControl(diver),action)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',383).
( cl(events(loseBuoyancyControl/1),[])  ,    cl(mpred_prop(loseBuoyancyControl(diver),action),[]) ,     cl(actions(loseBuoyancyControl/1),[])).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',385).
% predicate IsInexperiencedDiver(diver)
%~ From E:
%~ 
%~ predicate( isInexperiencedDiver(diver))
%~ cpc :- predicates([isInexperiencedDiver/1])
%~ ooo :- [   cl([predicates([isInexperiencedDiver/1])],[])]
%~ cpc :- mpred_prop(isInexperiencedDiver(diver),predicate)
%~ ooo :- [   cl([mpred_prop(isInexperiencedDiver(diver),predicate)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',385).
cl(mpred_prop(isInexperiencedDiver(diver),predicate),[]),cl(predicates(isInexperiencedDiver/1),[]).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',387).
% [diver,time]
% Happens(LoseBuoyancyControl(diver),time) ->
% IsInexperiencedDiver(diver).
%~ From E:
%~ 
%~ happens(loseBuoyancyControl(Diver),Time) ->         isInexperiencedDiver(Diver)
%~ cpc :- happens(loseBuoyancyControl(Diver),Time) ->         isInexperiencedDiver(Diver)
%~ ooo :- [   cl(    [   isInexperiencedDiver(Diver)],       [      happens(loseBuoyancyControl(Diver),Time)])]
cl(isInexperiencedDiver(Diver),happens(loseBuoyancyControl(Diver),Time)).
 %  cl(isInexperiencedDiver(Diver),happens(loseBuoyancyControl(Diver),Time)).
 %  % =================================.


% [diver,time]
% Initiates(LoseBuoyancyControl(diver),UncontrolledBuoyancy(diver),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',392).
%~ From E:
%~ 
%~ initiates_at(loseBuoyancyControl(Diver),uncontrolledBuoyancy(Diver),Time)
%~ cpc :- initiates(loseBuoyancyControl(Diver),uncontrolledBuoyancy(Diver),Time)
%~ ooo :- [   cl([initiates(loseBuoyancyControl(Diver),uncontrolledBuoyancy(Diver),Time)],[])]
cl(initiates(loseBuoyancyControl(Diver),uncontrolledBuoyancy(Diver),Time),[]).
 %  cl(initiates(loseBuoyancyControl(Diver),uncontrolledBuoyancy(Diver),Time),[]).
 %  % =================================.


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',394).
% [diver,time]
% Initiates(LoseBuoyancyControl(diver),PositivelyBuoyant(diver),time).
%~ From E:
%~ 
%~ initiates_at(loseBuoyancyControl(Diver),positivelyBuoyant(Diver),Time)
%~ cpc :- initiates(loseBuoyancyControl(Diver),positivelyBuoyant(Diver),Time)
%~ ooo :- [   cl([initiates(loseBuoyancyControl(Diver),positivelyBuoyant(Diver),Time)],[])]
cl(initiates(loseBuoyancyControl(Diver),positivelyBuoyant(Diver),Time),[]).
 %  cl(initiates(loseBuoyancyControl(Diver),positivelyBuoyant(Diver),Time),[]).
 %  % =================================.


% [diver,time]
% Terminates(LoseBuoyancyControl(diver),NegativelyBuoyant(diver),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',398).
%~ From E:
%~ 
%~ terminates_at(loseBuoyancyControl(Diver),negativelyBuoyant(Diver),Time)
%~ cpc :- terminates(loseBuoyancyControl(Diver),negativelyBuoyant(Diver),Time)
%~ ooo :- [   cl([terminates(loseBuoyancyControl(Diver),negativelyBuoyant(Diver),Time)],[])]
cl(terminates(loseBuoyancyControl(Diver),negativelyBuoyant(Diver),Time),[]).
 %  cl(terminates(loseBuoyancyControl(Diver),negativelyBuoyant(Diver),Time),[]).
 %  % =================================.


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',400).
% [diver,time]
% Terminates(LoseBuoyancyControl(diver),NeutrallyBuoyant(diver),time).
%~ From E:
%~ 
%~ terminates_at(loseBuoyancyControl(Diver),neutrallyBuoyant(Diver),Time)
%~ cpc :- terminates(loseBuoyancyControl(Diver),neutrallyBuoyant(Diver),Time)
%~ ooo :- [   cl([terminates(loseBuoyancyControl(Diver),neutrallyBuoyant(Diver),Time)],[])]
cl(terminates(loseBuoyancyControl(Diver),neutrallyBuoyant(Diver),Time),[]).
 %  cl(terminates(loseBuoyancyControl(Diver),neutrallyBuoyant(Diver),Time),[]).
 %  % =================================.


%; determining fluent

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',404).
% fluent AscendDescendAmount(diver,depth)
%~ From E:
%~ 
%~ fluent( ascendDescendAmount(diver,depth))
%~ cpc :- fluents([ascendDescendAmount/2])
%~ ooo :- [   cl([fluents([ascendDescendAmount/2])],[])]
%~ cpc :- mpred_prop(ascendDescendAmount(diver,depth),fluent)
%~ ooo :- [   cl([mpred_prop(ascendDescendAmount(diver,depth),fluent)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',404).
cl(mpred_prop(ascendDescendAmount(diver,depth),fluent),[]),cl(fluents(ascendDescendAmount/2),[]).

% noninertial AscendDescendAmount
%~ From E:
%~ 
%~ :-( call_pel_directive( noninertial(ascendDescendAmount)))
:-( call_pel_directive( noninertial(ascendDescendAmount))).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',407).
% [diver,depth1,depth2,time]
% HoldsAt(AscendDescendAmount(diver,depth1),time) &
% HoldsAt(AscendDescendAmount(diver,depth2),time) ->
% depth1=depth2.
%~ From E:
%~ 
%~ (     holds(ascendDescendAmount(Diver,Depth1),Time) ,     holds(ascendDescendAmount(Diver,Depth2),Time)) ->         Depth1=Depth2
%~ cpc :- (     holds(ascendDescendAmount(Diver,Depth1),Time) ,     holds(ascendDescendAmount(Diver,Depth2),Time)) ->         Depth1=Depth2
%~ ooo :- [   cl(    [   equals(Depth1,Depth2)],       [      holds(ascendDescendAmount(Diver,Depth1),Time),           holds(ascendDescendAmount(Diver,Depth2),Time)])]
cl( equals(Depth1,Depth2),   (     holds(ascendDescendAmount(Diver,Depth1),Time) ,     holds(ascendDescendAmount(Diver,Depth2),Time))).
 %  cl( equals(Depth1,Depth2),   (     holds(ascendDescendAmount(Diver,Depth1),Time) ,     holds(ascendDescendAmount(Diver,Depth2),Time))).
 %  % =================================.


% [diver,depth,time]
% Happens(Descend(diver,depth),time) ->
% HoldsAt(NegativelyBuoyant(diver),time) &
% ({depth1}
%  HoldsAt(AscendDescendAmount(diver,depth1),time) &
%  HoldsAt(AtDepth(diver,depth-depth1),time)).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',413).
%~ From E:
%~ 
%~ happens(descend(Diver,Depth),Time) ->         (     holds(negativelyBuoyant(Diver),Time) ,     thereExists( Depth1,     (     holds(ascendDescendAmount(Diver,Depth1),Time) ,     holds(atDepth(Diver,Depth-Depth1),Time))))
%~ cpc :- happens(descend(Diver,Depth),Time) ->         (     holds(negativelyBuoyant(Diver),Time) ,     thereExists( Depth1,     (     holds(ascendDescendAmount(Diver,Depth1),Time) ,     holds(atDepth(Diver,Depth-Depth1),Time))))
%~ ooo :- [   cl(    [   exists( Depth1,   (     ( holds(negativelyBuoyant(Diver),Time)  ,        holds(ascendDescendAmount(Diver,Depth1),Time) ,         holds(atDepth(Diver,Depth-Depth1),Time)) ;     not( happens(descend(Diver,Depth),Time))))],       [])]
cl(    exists( Depth1,   (     ( holds(negativelyBuoyant(Diver),Time)  ,        holds(ascendDescendAmount(Diver,Depth1),Time) ,         holds(atDepth(Diver,Depth-Depth1),Time)) ;     not( happens(descend(Diver,Depth),Time)))),       []).
 %  cl(    exists( Depth1,   (     ( holds(negativelyBuoyant(Diver),Time)  ,        holds(ascendDescendAmount(Diver,Depth1),Time) ,         holds(atDepth(Diver,Depth-Depth1),Time)) ;     not( happens(descend(Diver,Depth),Time)))),       []).
 %  % =================================.

% event KickUp(diver)
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',418).
%~ From E:
%~ 
%~ event( kickUp(diver))
%~ cpc :- events([kickUp/1])
%~ ooo :- [   cl([events([kickUp/1])],[])]
%~ cpc :- actions([kickUp/1])
%~ ooo :- [   cl([actions([kickUp/1])],[])]
%~ cpc :- mpred_prop(kickUp(diver),action)
%~ ooo :- [   cl([mpred_prop(kickUp(diver),action)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',418).
( cl(events(kickUp/1),[])  ,    cl(mpred_prop(kickUp(diver),action),[]) ,     cl(actions(kickUp/1),[])).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',421).
% [diver,depth,time]
% Happens(Ascend(diver,depth),time) ->
% (HoldsAt(PositivelyBuoyant(diver),time) |
%  (HoldsAt(NeutrallyBuoyant(diver),time) & Happens(KickUp(diver),time))) &
% ({depth1}
%  HoldsAt(AscendDescendAmount(diver,depth1),time) &
%  HoldsAt(AtDepth(diver,depth+depth1),time)).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',421).
%~ From E:
%~ 
%~ happens(ascend(Diver,Depth),Time) ->         (     (         holds(positivelyBuoyant(Diver),Time) ;         holds(neutrallyBuoyant(Diver),Time),happens(kickUp(Diver),Time)) ,     thereExists( Depth1,     (     holds(ascendDescendAmount(Diver,Depth1),Time) ,     holds(atDepth(Diver,Depth+Depth1),Time))))
%~ cpc :- happens(ascend(Diver,Depth),Time) ->         (     (         holds(positivelyBuoyant(Diver),Time) ;         holds(neutrallyBuoyant(Diver),Time),happens(kickUp(Diver),Time)) ,     thereExists( Depth1,     (     holds(ascendDescendAmount(Diver,Depth1),Time) ,     holds(atDepth(Diver,Depth+Depth1),Time))))
%~ ooo :- [   cl(    [   exists( Depth1,   (     ( (     holds(positivelyBuoyant(Diver),Time) ;     holds(neutrallyBuoyant(Diver),Time),happens(kickUp(Diver),Time))  ,        holds(ascendDescendAmount(Diver,Depth1),Time) ,         holds(atDepth(Diver,Depth+Depth1),Time)) ;     not( happens(ascend(Diver,Depth),Time))))],       [])]
cl(    exists( Depth1,   (     ( (     holds(positivelyBuoyant(Diver),Time) ;     holds(neutrallyBuoyant(Diver),Time),happens(kickUp(Diver),Time))  ,        holds(ascendDescendAmount(Diver,Depth1),Time) ,         holds(atDepth(Diver,Depth+Depth1),Time)) ;     not( happens(ascend(Diver,Depth),Time)))),       []).
 %  cl(    exists( Depth1,   (     ( (     holds(positivelyBuoyant(Diver),Time) ;     holds(neutrallyBuoyant(Diver),Time),happens(kickUp(Diver),Time))  ,        holds(ascendDescendAmount(Diver,Depth1),Time) ,         holds(atDepth(Diver,Depth+Depth1),Time)) ;     not( happens(ascend(Diver,Depth),Time)))),       []).
 %  % =================================.


% [diver,time]
% Happens(KickUp(diver),time) ->
% HoldsAt(Vertical(diver),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',430).
%~ From E:
%~ 
%~ happens(kickUp(Diver),Time) ->         holds(vertical(Diver),Time)
%~ cpc :- happens(kickUp(Diver),Time) ->         holds(vertical(Diver),Time)
%~ ooo :- [   cl([holds(vertical(Diver),Time)],[happens(kickUp(Diver),Time)])]
cl(holds(vertical(Diver),Time),happens(kickUp(Diver),Time)).
 %  cl(holds(vertical(Diver),Time),happens(kickUp(Diver),Time)).
 %  % =================================.

% event SwimAround(diver)
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',432).
%~ From E:
%~ 
%~ event( swimAround(diver))
%~ cpc :- events([swimAround/1])
%~ ooo :- [   cl([events([swimAround/1])],[])]
%~ cpc :- actions([swimAround/1])
%~ ooo :- [   cl([actions([swimAround/1])],[])]
%~ cpc :- mpred_prop(swimAround(diver),action)
%~ ooo :- [   cl([mpred_prop(swimAround(diver),action)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',432).
( cl(events(swimAround/1),[])  ,    cl(mpred_prop(swimAround(diver),action),[]) ,     cl(actions(swimAround/1),[])).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',435).
% [diver,time]
% Happens(SwimAround(diver),time) ->
% HoldsAt(HorizontalDown(diver),time).
%~ From E:
%~ 
%~ happens(swimAround(Diver),Time) ->         holds(horizontalDown(Diver),Time)
%~ cpc :- happens(swimAround(Diver),Time) ->         holds(horizontalDown(Diver),Time)
%~ ooo :- [   cl(    [   holds(horizontalDown(Diver),Time)],       [      happens(swimAround(Diver),Time)])]
cl(holds(horizontalDown(Diver),Time),happens(swimAround(Diver),Time)).
 %  cl(holds(horizontalDown(Diver),Time),happens(swimAround(Diver),Time)).
 %  % =================================.


%; signaling

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',441).
% event SignalDescend(diver,diver)
%~ From E:
%~ 
%~ event( signalDescend(diver,diver))
%~ cpc :- events([signalDescend/2])
%~ ooo :- [   cl([events([signalDescend/2])],[])]
%~ cpc :- actions([signalDescend/2])
%~ ooo :- [   cl([actions([signalDescend/2])],[])]
%~ cpc :- mpred_prop(signalDescend(diver,diver),action)
%~ ooo :- [   cl([mpred_prop(signalDescend(diver,diver),action)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',441).
( cl(events(signalDescend/2),[])  ,    cl(mpred_prop(signalDescend(diver,diver),action),[]) ,     cl(actions(signalDescend/2),[])).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',443).
% event SignalOutOfTime(diver,diver)
%~ From E:
%~ 
%~ event( signalOutOfTime(diver,diver))
%~ cpc :- events([signalOutOfTime/2])
%~ ooo :- [   cl([events([signalOutOfTime/2])],[])]
%~ cpc :- actions([signalOutOfTime/2])
%~ ooo :- [   cl([actions([signalOutOfTime/2])],[])]
%~ cpc :- mpred_prop(signalOutOfTime(diver,diver),action)
%~ ooo :- [   cl([mpred_prop(signalOutOfTime(diver,diver),action)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',443).
( cl(events(signalOutOfTime/2),[])  ,    cl(mpred_prop(signalOutOfTime(diver,diver),action),[]) ,     cl(actions(signalOutOfTime/2),[])).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',445).
% event SignalAscend(diver,diver)
%~ From E:
%~ 
%~ event( signalAscend(diver,diver))
%~ cpc :- events([signalAscend/2])
%~ ooo :- [   cl([events([signalAscend/2])],[])]
%~ cpc :- actions([signalAscend/2])
%~ ooo :- [   cl([actions([signalAscend/2])],[])]
%~ cpc :- mpred_prop(signalAscend(diver,diver),action)
%~ ooo :- [   cl([mpred_prop(signalAscend(diver,diver),action)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',445).
( cl(events(signalAscend/2),[])  ,    cl(mpred_prop(signalAscend(diver,diver),action),[]) ,     cl(actions(signalAscend/2),[])).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',447).
%;[diver1,diver2,time]
%;Happens(SignalAscend(diver1,diver2),time) ->
%;Happens(SignalOutOfTime(diver1,diver2),time-1).
%;[diver1,diver2,time]
%;Happens(SignalDescend(diver1,diver2),time) ->
%;HoldsAt(See(diver1,diver2),time) &
%;HoldsAt(See(diver2,diver1),time).
%;[diver1,diver2,time]
%;Happens(SignalOutOfTime(diver1,diver2),time) ->
%;HoldsAt(See(diver1,diver2),time) &
%;HoldsAt(See(diver2,diver1),time).
%;[diver1,diver2,time]
%;Happens(SignalAscend(diver1,diver2),time) ->
%;HoldsAt(See(diver1,diver2),time) &
%;HoldsAt(See(diver2,diver1),time).
%;event LookAt(agent,object)
%;fluent See(agent,object)
%;[agent,object,time]
%;Initiates(LookAt(agent,object),See(agent,object),time).
%;[agent,object1,object2,time]
%;object1!=object2 ->
%;Terminates(LookAt(agent,object1),
%;           See(agent,object2),
%;           time).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',479).
% event Descend1(diver)
%~ From E:
%~ 
%~ event( descend1(diver))
%~ cpc :- events([descend1/1])
%~ ooo :- [   cl([events([descend1/1])],[])]
%~ cpc :- actions([descend1/1])
%~ ooo :- [   cl([actions([descend1/1])],[])]
%~ cpc :- mpred_prop(descend1(diver),action)
%~ ooo :- [   cl([mpred_prop(descend1(diver),action)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',479).
( cl(events(descend1/1),[])  ,    cl(mpred_prop(descend1(diver),action),[]) ,     cl(actions(descend1/1),[])).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',481).
% event Ascend1(diver)
%~ From E:
%~ 
%~ event( ascend1(diver))
%~ cpc :- events([ascend1/1])
%~ ooo :- [   cl([events([ascend1/1])],[])]
%~ cpc :- actions([ascend1/1])
%~ ooo :- [   cl([actions([ascend1/1])],[])]
%~ cpc :- mpred_prop(ascend1(diver),action)
%~ ooo :- [   cl([mpred_prop(ascend1(diver),action)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',481).
( cl(events(ascend1/1),[])  ,    cl(mpred_prop(ascend1(diver),action),[]) ,     cl(actions(ascend1/1),[])).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',483).
%;[diver,object,time]
%;Terminates(Descend1(diver),See(diver,object),time).
%;[diver,object,time]
%;Terminates(Ascend1(diver),See(diver,object),time).
%;[diver,object,time]
%;Terminates(RotateYaw(diver),See(diver,object),time).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',492).
% event RapidAscendToSurface(diver)
%~ From E:
%~ 
%~ event( rapidAscendToSurface(diver))
%~ cpc :- events([rapidAscendToSurface/1])
%~ ooo :- [   cl([events([rapidAscendToSurface/1])],[])]
%~ cpc :- actions([rapidAscendToSurface/1])
%~ ooo :- [   cl([actions([rapidAscendToSurface/1])],[])]
%~ cpc :- mpred_prop(rapidAscendToSurface(diver),action)
%~ ooo :- [   cl([mpred_prop(rapidAscendToSurface(diver),action)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',492).
( cl(events(rapidAscendToSurface/1),[])  ,    cl(mpred_prop(rapidAscendToSurface(diver),action),[]) ,     cl(actions(rapidAscendToSurface/1),[])).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',494).
% [diver,time]
% Happens(Descend1(diver),time) <->
% ({depth} Happens(Descend(diver,depth),time)).
%~ From E:
%~ 
%~ (     happens(descend1(Diver),Time) <->     thereExists(Depth,happens(descend(Diver,Depth),Time)))
%~ cpc :- (     happens(descend1(Diver),Time) <->     thereExists(Depth,happens(descend(Diver,Depth),Time)))
%~ ooo :- [   cl(    [   exists( Depth,   exists( Depth,   (     (         happens(descend(Diver,Depth),Time) ;         not( happens(descend1(Diver),Time))) ,     (         happens(descend1(Diver),Time) ;         not( happens(descend(Diver,Depth),Time))))))],       [])]
cl(    exists( Depth,   exists( Depth,   (     (         happens(descend(Diver,Depth),Time) ;         not( happens(descend1(Diver),Time))) ,     (         happens(descend1(Diver),Time) ;         not( happens(descend(Diver,Depth),Time)))))),       []).
 %  cl(    exists( Depth,   exists( Depth,   (     (         happens(descend(Diver,Depth),Time) ;         not( happens(descend1(Diver),Time))) ,     (         happens(descend1(Diver),Time) ;         not( happens(descend(Diver,Depth),Time)))))),       []).
 %  % =================================.


% [diver,time]
% Happens(Ascend1(diver),time) <->
% ({depth} Happens(Ascend(diver,depth),time)).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',499).
%~ From E:
%~ 
%~ (     happens(ascend1(Diver),Time) <->     thereExists(Depth,happens(ascend(Diver,Depth),Time)))
%~ cpc :- (     happens(ascend1(Diver),Time) <->     thereExists(Depth,happens(ascend(Diver,Depth),Time)))
%~ ooo :- [   cl(    [   exists( Depth,   exists( Depth,   (     (         happens(ascend(Diver,Depth),Time) ;         not( happens(ascend1(Diver),Time))) ,     (         happens(ascend1(Diver),Time) ;         not( happens(ascend(Diver,Depth),Time))))))],       [])]
cl(    exists( Depth,   exists( Depth,   (     (         happens(ascend(Diver,Depth),Time) ;         not( happens(ascend1(Diver),Time))) ,     (         happens(ascend1(Diver),Time) ;         not( happens(ascend(Diver,Depth),Time)))))),       []).
 %  cl(    exists( Depth,   exists( Depth,   (     (         happens(ascend(Diver,Depth),Time) ;         not( happens(ascend1(Diver),Time))) ,     (         happens(ascend1(Diver),Time) ;         not( happens(ascend(Diver,Depth),Time)))))),       []).
 %  % =================================.


% [diver,time]
% Happens(RapidAscendToSurface(diver),time) ->
% Happens(Ascend(diver,0),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',503).
%~ From E:
%~ 
%~ happens(rapidAscendToSurface(Diver),Time) ->         happens(ascend(Diver,0),Time)
%~ cpc :- happens(rapidAscendToSurface(Diver),Time) ->         happens(ascend(Diver,0),Time)
%~ ooo :- [   cl(    [   happens(ascend(Diver,0),Time)],       [      happens(rapidAscendToSurface(Diver),Time)])]
cl(happens(ascend(Diver,0),Time),happens(rapidAscendToSurface(Diver),Time)).
 %  cl(happens(ascend(Diver,0),Time),happens(rapidAscendToSurface(Diver),Time)).
 %  % =================================.

% event AscendLine(diver,line)
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',505).
%~ From E:
%~ 
%~ event( ascendLine(diver,line))
%~ cpc :- events([ascendLine/2])
%~ ooo :- [   cl([events([ascendLine/2])],[])]
%~ cpc :- actions([ascendLine/2])
%~ ooo :- [   cl([actions([ascendLine/2])],[])]
%~ cpc :- mpred_prop(ascendLine(diver,line),action)
%~ ooo :- [   cl([mpred_prop(ascendLine(diver,line),action)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',505).
( cl(events(ascendLine/2),[])  ,    cl(mpred_prop(ascendLine(diver,line),action),[]) ,     cl(actions(ascendLine/2),[])).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',508).
% [diver,line,time]
% Happens(AscendLine(diver,line),time) ->
% Happens(Ascend1(diver),time).
%~ From E:
%~ 
%~ happens(ascendLine(Diver,Line),Time) ->         happens(ascend1(Diver),Time)
%~ cpc :- happens(ascendLine(Diver,Line),Time) ->         happens(ascend1(Diver),Time)
%~ ooo :- [   cl(    [   happens(ascend1(Diver),Time)],       [      happens(ascendLine(Diver,Line),Time)])]
cl(happens(ascend1(Diver),Time),happens(ascendLine(Diver,Line),Time)).
 %  cl(happens(ascend1(Diver),Time),happens(ascendLine(Diver,Line),Time)).
 %  % =================================.

% fluent Disoriented(diver)
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',511).
%~ From E:
%~ 
%~ fluent( disoriented(diver))
%~ cpc :- fluents([disoriented/1])
%~ ooo :- [   cl([fluents([disoriented/1])],[])]
%~ cpc :- mpred_prop(disoriented(diver),fluent)
%~ ooo :- [   cl([mpred_prop(disoriented(diver),fluent)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',511).
cl(mpred_prop(disoriented(diver),fluent),[]),cl(fluents(disoriented/1),[]).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',514).
% event BecomeDisoriented(diver)
%~ From E:
%~ 
%~ event( becomeDisoriented(diver))
%~ cpc :- events([becomeDisoriented/1])
%~ ooo :- [   cl([events([becomeDisoriented/1])],[])]
%~ cpc :- actions([becomeDisoriented/1])
%~ ooo :- [   cl([actions([becomeDisoriented/1])],[])]
%~ cpc :- mpred_prop(becomeDisoriented(diver),action)
%~ ooo :- [   cl([mpred_prop(becomeDisoriented(diver),action)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',514).
( cl(events(becomeDisoriented/1),[])  ,    cl(mpred_prop(becomeDisoriented(diver),action),[]) ,     cl(actions(becomeDisoriented/1),[])).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',516).
% event BecomeReoriented(diver)
%~ From E:
%~ 
%~ event( becomeReoriented(diver))
%~ cpc :- events([becomeReoriented/1])
%~ ooo :- [   cl([events([becomeReoriented/1])],[])]
%~ cpc :- actions([becomeReoriented/1])
%~ ooo :- [   cl([actions([becomeReoriented/1])],[])]
%~ cpc :- mpred_prop(becomeReoriented(diver),action)
%~ ooo :- [   cl([mpred_prop(becomeReoriented(diver),action)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',516).
( cl(events(becomeReoriented/1),[])  ,    cl(mpred_prop(becomeReoriented(diver),action),[]) ,     cl(actions(becomeReoriented/1),[])).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',518).
% [diver,time]
% Initiates(BecomeDisoriented(diver),Disoriented(diver),time).
%~ From E:
%~ 
%~ initiates_at(becomeDisoriented(Diver),disoriented(Diver),Time)
%~ cpc :- initiates(becomeDisoriented(Diver),disoriented(Diver),Time)
%~ ooo :- [   cl([initiates(becomeDisoriented(Diver),disoriented(Diver),Time)],[])]
cl(initiates(becomeDisoriented(Diver),disoriented(Diver),Time),[]).
 %  cl(initiates(becomeDisoriented(Diver),disoriented(Diver),Time),[]).
 %  % =================================.


% [diver,time]
% Terminates(BecomeReoriented(diver),Disoriented(diver),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',522).
%~ From E:
%~ 
%~ terminates_at(becomeReoriented(Diver),disoriented(Diver),Time)
%~ cpc :- terminates(becomeReoriented(Diver),disoriented(Diver),Time)
%~ ooo :- [   cl([terminates(becomeReoriented(Diver),disoriented(Diver),Time)],[])]
cl(terminates(becomeReoriented(Diver),disoriented(Diver),Time),[]).
 %  cl(terminates(becomeReoriented(Diver),disoriented(Diver),Time),[]).
 %  % =================================.

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',524).
% fluent DisturbedSilt()
%~ From E:
%~ 
%~ fluent(disturbedSilt())
%~ cpc :- fluents([disturbedSilt/0])
%~ ooo :- [   cl([fluents([disturbedSilt/0])],[])]
%~ cpc :- mpred_prop(disturbedSilt(),fluent)
%~ ooo :- [   cl([mpred_prop(disturbedSilt(),fluent)],[])]
cl(mpred_prop(disturbedSilt(),fluent),[]),cl((fluents disturbedSilt/0),[]).
 %  cl(mpred_prop(disturbedSilt,fluent),[]),cl(fluents(disturbedSilt/0),[]).
 %  % =================================.

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',526).
% event DisturbSilt(diver)
%~ From E:
%~ 
%~ event( disturbSilt(diver))
%~ cpc :- events([disturbSilt/1])
%~ ooo :- [   cl([events([disturbSilt/1])],[])]
%~ cpc :- actions([disturbSilt/1])
%~ ooo :- [   cl([actions([disturbSilt/1])],[])]
%~ cpc :- mpred_prop(disturbSilt(diver),action)
%~ ooo :- [   cl([mpred_prop(disturbSilt(diver),action)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',526).
( cl(events(disturbSilt/1),[])  ,    cl(mpred_prop(disturbSilt(diver),action),[]) ,     cl(actions(disturbSilt/1),[])).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',528).
% [diver,time]
% Initiates(DisturbSilt(diver),DisturbedSilt(),time).
%~ From E:
%~ 
%~ initiates_at(disturbSilt(Diver),disturbedSilt(),Time)
%~ cpc :- initiates(disturbSilt(Diver),disturbedSilt(),Time)
%~ ooo :- [   cl([initiates(disturbSilt(Diver),disturbedSilt(),Time)],[])]
cl(initiates(disturbSilt(Diver),disturbedSilt(),Time),[]).
 %  cl(initiates(disturbSilt(Diver),disturbedSilt,Time),[]).
 %  % =================================.


% [diver,time]
% Happens(BecomeDisoriented(diver),time) ->
% (!HoldsAt(DisturbedSilt(),time-1) &
%  HoldsAt(DisturbedSilt(),time)).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',532).
%~ From E:
%~ 
%~ happens(becomeDisoriented(Diver),Time) ->         holds(not(disturbedSilt()),Time-1),holds(disturbedSilt(),Time)
%~ cpc :- happens(becomeDisoriented(Diver),Time) ->         holds(not(disturbedSilt()),Time-1),holds(disturbedSilt(),Time)
%~ ooo :- [   cl( [], [  [   holds(disturbedSilt(),Time-1),     happens(becomeDisoriented(Diver),Time)])],     cl(      [   holds(disturbedSilt(),Time)],           [        happens(becomeDisoriented(Diver),Time)])]
(     cl([],(holds(disturbedSilt(),Time-1),happens(becomeDisoriented(Diver),Time))) ,     cl(holds(disturbedSilt(),Time),happens(becomeDisoriented(Diver),Time))).
 %  (     cl([],(holds(disturbedSilt,Time-1),happens(becomeDisoriented(Diver),Time))) ,     cl(holds(disturbedSilt,Time),happens(becomeDisoriented(Diver),Time))).
 %  % =================================.

% event Panic(diver)
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',535).
%~ From E:
%~ 
%~ event( panic(diver))
%~ cpc :- events([panic/1])
%~ ooo :- [   cl([events([panic/1])],[])]
%~ cpc :- actions([panic/1])
%~ ooo :- [   cl([actions([panic/1])],[])]
%~ cpc :- mpred_prop(panic(diver),action)
%~ ooo :- [   cl([mpred_prop(panic(diver),action)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',535).
( cl(events(panic/1),[])  ,    cl(mpred_prop(panic(diver),action),[]) ,     cl(actions(panic/1),[])).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',538).
% [diver,time]
 % Happens(Panic(diver),time) ->
% HoldsAt(Disoriented(diver),time) |
% HoldsAt(UncontrolledBuoyancy(diver),time) |
% ({equipment} Happens(Lose(diver,equipment),time-1)) |
% Happens(Vomit(diver),time-1).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',538).
%~ From E:
%~ 
%~ happens(panic(Diver),Time) ->         ( holds(disoriented(Diver),Time)  ;    holds(uncontrolledBuoyancy(Diver),Time) ;     thereExists(Equipment,happens(lose(Diver,Equipment),Time-1)) ;     happens(vomit(Diver),Time-1))
%~ cpc :- happens(panic(Diver),Time) ->         ( holds(disoriented(Diver),Time)  ;    holds(uncontrolledBuoyancy(Diver),Time) ;     thereExists(Equipment,happens(lose(Diver,Equipment),Time-1)) ;     happens(vomit(Diver),Time-1))
%~ ooo :- [   cl(    [   exists( Equipment,   ( holds(disoriented(Diver),Time)  ;    holds(uncontrolledBuoyancy(Diver),Time) ;     happens(lose(Diver,Equipment),Time-1) ;     happens(vomit(Diver),Time-1) ;     not( happens(panic(Diver),Time))))],       [])]
cl(    exists( Equipment,   ( holds(disoriented(Diver),Time)  ;    holds(uncontrolledBuoyancy(Diver),Time) ;     happens(lose(Diver,Equipment),Time-1) ;     happens(vomit(Diver),Time-1) ;     not( happens(panic(Diver),Time)))),       []).
 %  ( cl(    exists( Equipment,   ( holds(disoriented(Diver),Time)  ;    holds(uncontrolledBuoyancy(Diver),Time) ;     happens(lose(Diver,Equipment),Time-1) ;     happens(vomit(Diver),Time-1) ;     not( happens(panic(Diver),Time)))),       [])).
 %  % =================================.

% event Vomit(diver)
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',543).
%~ From E:
%~ 
%~ event( vomit(diver))
%~ cpc :- events([vomit/1])
%~ ooo :- [   cl([events([vomit/1])],[])]
%~ cpc :- actions([vomit/1])
%~ ooo :- [   cl([actions([vomit/1])],[])]
%~ cpc :- mpred_prop(vomit(diver),action)
%~ ooo :- [   cl([mpred_prop(vomit(diver),action)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',543).
( cl(events(vomit/1),[])  ,    cl(mpred_prop(vomit(diver),action),[]) ,     cl(actions(vomit/1),[])).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',546).
%; conditions

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',548).
% fluent Unconscious(diver)
%~ From E:
%~ 
%~ fluent( unconscious(diver))
%~ cpc :- fluents([unconscious/1])
%~ ooo :- [   cl([fluents([unconscious/1])],[])]
%~ cpc :- mpred_prop(unconscious(diver),fluent)
%~ ooo :- [   cl([mpred_prop(unconscious(diver),fluent)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',548).
cl(mpred_prop(unconscious(diver),fluent),[]),cl(fluents(unconscious/1),[]).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',550).
% event GoUnconscious(diver)
%~ From E:
%~ 
%~ event( goUnconscious(diver))
%~ cpc :- events([goUnconscious/1])
%~ ooo :- [   cl([events([goUnconscious/1])],[])]
%~ cpc :- actions([goUnconscious/1])
%~ ooo :- [   cl([actions([goUnconscious/1])],[])]
%~ cpc :- mpred_prop(goUnconscious(diver),action)
%~ ooo :- [   cl([mpred_prop(goUnconscious(diver),action)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',550).
( cl(events(goUnconscious/1),[])  ,    cl(mpred_prop(goUnconscious(diver),action),[]) ,     cl(actions(goUnconscious/1),[])).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',552).
% event RegainConsciousness(diver)
%~ From E:
%~ 
%~ event( regainConsciousness(diver))
%~ cpc :- events([regainConsciousness/1])
%~ ooo :- [   cl([events([regainConsciousness/1])],[])]
%~ cpc :- actions([regainConsciousness/1])
%~ ooo :- [   cl([actions([regainConsciousness/1])],[])]
%~ cpc :- mpred_prop(regainConsciousness(diver),action)
%~ ooo :- [   cl([mpred_prop(regainConsciousness(diver),action)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',552).
( cl(events(regainConsciousness/1),[])  ,    cl(mpred_prop(regainConsciousness(diver),action),[]) ,     cl(actions(regainConsciousness/1),[])).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',554).
% [diver,time]
% Initiates(GoUnconscious(diver),Unconscious(diver),time).
%~ From E:
%~ 
%~ initiates_at(goUnconscious(Diver),unconscious(Diver),Time)
%~ cpc :- initiates(goUnconscious(Diver),unconscious(Diver),Time)
%~ ooo :- [   cl([initiates(goUnconscious(Diver),unconscious(Diver),Time)],[])]
cl(initiates(goUnconscious(Diver),unconscious(Diver),Time),[]).
 %  cl(initiates(goUnconscious(Diver),unconscious(Diver),Time),[]).
 %  % =================================.


% [diver,time]
% Terminates(RegainConsciousness(diver),Unconscious(diver),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',558).
%~ From E:
%~ 
%~ terminates_at(regainConsciousness(Diver),unconscious(Diver),Time)
%~ cpc :- terminates(regainConsciousness(Diver),unconscious(Diver),Time)
%~ ooo :- [   cl([terminates(regainConsciousness(Diver),unconscious(Diver),Time)],[])]
cl(terminates(regainConsciousness(Diver),unconscious(Diver),Time),[]).
 %  cl(terminates(regainConsciousness(Diver),unconscious(Diver),Time),[]).
 %  % =================================.


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',560).
% [diver,time]
% Happens(GoUnconscious(diver),time) ->
% Happens(RapidAscendToSurface(diver),time).
%~ From E:
%~ 
%~ happens(goUnconscious(Diver),Time) ->         happens(rapidAscendToSurface(Diver),Time)
%~ cpc :- happens(goUnconscious(Diver),Time) ->         happens(rapidAscendToSurface(Diver),Time)
%~ ooo :- [   cl(    [   happens(rapidAscendToSurface(Diver),Time)],       [      happens(goUnconscious(Diver),Time)])]
cl(    happens(rapidAscendToSurface(Diver),Time),       happens(goUnconscious(Diver),Time)).
 %  cl(    happens(rapidAscendToSurface(Diver),Time),       happens(goUnconscious(Diver),Time)).
 %  % =================================.

% fluent HasEarPain(diver)
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',563).
%~ From E:
%~ 
%~ fluent( hasEarPain(diver))
%~ cpc :- fluents([hasEarPain/1])
%~ ooo :- [   cl([fluents([hasEarPain/1])],[])]
%~ cpc :- mpred_prop(hasEarPain(diver),fluent)
%~ ooo :- [   cl([mpred_prop(hasEarPain(diver),fluent)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',563).
cl(mpred_prop(hasEarPain(diver),fluent),[]),cl(fluents(hasEarPain/1),[]).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',566).
% event StartEarPain(diver)
%~ From E:
%~ 
%~ event( startEarPain(diver))
%~ cpc :- events([startEarPain/1])
%~ ooo :- [   cl([events([startEarPain/1])],[])]
%~ cpc :- actions([startEarPain/1])
%~ ooo :- [   cl([actions([startEarPain/1])],[])]
%~ cpc :- mpred_prop(startEarPain(diver),action)
%~ ooo :- [   cl([mpred_prop(startEarPain(diver),action)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',566).
( cl(events(startEarPain/1),[])  ,    cl(mpred_prop(startEarPain(diver),action),[]) ,     cl(actions(startEarPain/1),[])).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',568).
% [diver,time]
 % Initiates(StartEarPain(diver),HasEarPain(diver),time).
%~ From E:
%~ 
%~ initiates_at(startEarPain(Diver),hasEarPain(Diver),Time)
%~ cpc :- initiates(startEarPain(Diver),hasEarPain(Diver),Time)
%~ ooo :- [   cl([initiates(startEarPain(Diver),hasEarPain(Diver),Time)],[])]
cl(initiates(startEarPain(Diver),hasEarPain(Diver),Time),[]).
 %  cl(initiates(startEarPain(Diver),hasEarPain(Diver),Time),[]).
 %  % =================================.

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',570).
% fluent HasRupturedEardrum(diver)
%~ From E:
%~ 
%~ fluent( hasRupturedEardrum(diver))
%~ cpc :- fluents([hasRupturedEardrum/1])
%~ ooo :- [   cl([fluents([hasRupturedEardrum/1])],[])]
%~ cpc :- mpred_prop(hasRupturedEardrum(diver),fluent)
%~ ooo :- [   cl([mpred_prop(hasRupturedEardrum(diver),fluent)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',570).
cl(mpred_prop(hasRupturedEardrum(diver),fluent),[]),cl(fluents(hasRupturedEardrum/1),[]).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',572).
% event RuptureEardrum(diver)
%~ From E:
%~ 
%~ event( ruptureEardrum(diver))
%~ cpc :- events([ruptureEardrum/1])
%~ ooo :- [   cl([events([ruptureEardrum/1])],[])]
%~ cpc :- actions([ruptureEardrum/1])
%~ ooo :- [   cl([actions([ruptureEardrum/1])],[])]
%~ cpc :- mpred_prop(ruptureEardrum(diver),action)
%~ ooo :- [   cl([mpred_prop(ruptureEardrum(diver),action)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',572).
( cl(events(ruptureEardrum/1),[])  ,    cl(mpred_prop(ruptureEardrum(diver),action),[]) ,     cl(actions(ruptureEardrum/1),[])).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',574).
% [diver,time]
% Initiates(RuptureEardrum(diver),HasRupturedEardrum(diver),time).
%~ From E:
%~ 
%~ initiates_at(ruptureEardrum(Diver),hasRupturedEardrum(Diver),Time)
%~ cpc :- initiates(ruptureEardrum(Diver),hasRupturedEardrum(Diver),Time)
%~ ooo :- [   cl([initiates(ruptureEardrum(Diver),hasRupturedEardrum(Diver),Time)],[])]
cl(initiates(ruptureEardrum(Diver),hasRupturedEardrum(Diver),Time),[]).
 %  cl(initiates(ruptureEardrum(Diver),hasRupturedEardrum(Diver),Time),[]).
 %  % =================================.

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',576).
% fluent ConditionOK(diver)
%~ From E:
%~ 
%~ fluent( conditionOK(diver))
%~ cpc :- fluents([conditionOK/1])
%~ ooo :- [   cl([fluents([conditionOK/1])],[])]
%~ cpc :- mpred_prop(conditionOK(diver),fluent)
%~ ooo :- [   cl([mpred_prop(conditionOK(diver),fluent)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',576).
cl(mpred_prop(conditionOK(diver),fluent),[]),cl(fluents(conditionOK/1),[]).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',578).
% fluent HasDecompressionIllness(diver)
%~ From E:
%~ 
%~ fluent( hasDecompressionIllness(diver))
%~ cpc :- fluents([hasDecompressionIllness/1])
%~ ooo :- [   cl([fluents([hasDecompressionIllness/1])],[])]
%~ cpc :- mpred_prop(hasDecompressionIllness(diver),fluent)
%~ ooo :- [   cl([mpred_prop(hasDecompressionIllness(diver),fluent)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',578).
cl(mpred_prop(hasDecompressionIllness(diver),fluent),[]),cl(fluents(hasDecompressionIllness/1),[]).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',580).
% event StartDecompressionIllness(diver)
%~ From E:
%~ 
%~ event( startDecompressionIllness(diver))
%~ cpc :- events([startDecompressionIllness/1])
%~ ooo :- [   cl([events([startDecompressionIllness/1])],[])]
%~ cpc :- actions([startDecompressionIllness/1])
%~ ooo :- [   cl([actions([startDecompressionIllness/1])],[])]
%~ cpc :- mpred_prop(startDecompressionIllness(diver),action)
%~ ooo :- [   cl([mpred_prop(startDecompressionIllness(diver),action)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',580).
( cl(events(startDecompressionIllness/1),[])  ,    cl(mpred_prop(startDecompressionIllness(diver),action),[]) ,     cl(actions(startDecompressionIllness/1),[])).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',582).
% [diver,time]
% Initiates(StartDecompressionIllness(diver),
%           HasDecompressionIllness(diver),
%           time).
%~ From E:
%~ 
%~ initiates_at(startDecompressionIllness(Diver),hasDecompressionIllness(Diver),Time)
%~ cpc :- initiates(startDecompressionIllness(Diver),hasDecompressionIllness(Diver),Time)
%~ ooo :- [   cl(    [   initiates(startDecompressionIllness(Diver),hasDecompressionIllness(Diver),Time)],       [])]
cl(initiates(startDecompressionIllness(Diver),hasDecompressionIllness(Diver),Time),[]).
 %  cl(initiates(startDecompressionIllness(Diver),hasDecompressionIllness(Diver),Time),[]).
 %  % =================================.

% fluent SignalingDecompress(computer,diver)
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',586).
%~ From E:
%~ 
%~ fluent( signalingDecompress(computer,diver))
%~ cpc :- fluents([signalingDecompress/2])
%~ ooo :- [   cl([fluents([signalingDecompress/2])],[])]
%~ cpc :- mpred_prop(signalingDecompress(computer,diver),fluent)
%~ ooo :- [   cl([mpred_prop(signalingDecompress(computer,diver),fluent)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',586).
cl(mpred_prop(signalingDecompress(computer,diver),fluent),[]),cl(fluents(signalingDecompress/2),[]).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',589).
% fluent SignalingLowOnAir(computer,airtank,diver)
%~ From E:
%~ 
%~ fluent( signalingLowOnAir(computer,airtank,diver))
%~ cpc :- fluents([signalingLowOnAir/3])
%~ ooo :- [   cl([fluents([signalingLowOnAir/3])],[])]
%~ cpc :- mpred_prop(signalingLowOnAir(computer,airtank,diver),fluent)
%~ ooo :- [   cl([mpred_prop(signalingLowOnAir(computer,airtank,diver),fluent)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',589).
cl(mpred_prop(signalingLowOnAir(computer,airtank,diver),fluent),[]),cl(fluents(signalingLowOnAir/3),[]).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',591).
% [computer,airtank,diver,time]
% HoldsAt(SignalingLowOnAir(computer,airtank,diver),time) ->
% HoldsAt(LowOnAir(airtank),time).
%~ From E:
%~ 
%~ holds(signalingLowOnAir(Computer,Airtank,Diver),Time) ->         holds(lowOnAir(Airtank),Time)
%~ cpc :- holds(signalingLowOnAir(Computer,Airtank,Diver),Time) ->         holds(lowOnAir(Airtank),Time)
%~ ooo :- [   cl(    [   holds(lowOnAir(Airtank),Time)],       [      holds(signalingLowOnAir(Computer,Airtank,Diver),Time)])]
cl(    holds(lowOnAir(Airtank),Time),       holds(signalingLowOnAir(Computer,Airtank,Diver),Time)).
 %  cl(    holds(lowOnAir(Airtank),Time),       holds(signalingLowOnAir(Computer,Airtank,Diver),Time)).
 %  % =================================.


% [computer,diver,time]
% HoldsAt(SignalingDecompress(computer,diver),time) ->
% !{time1} time1<time & Happens(Decompress(diver),time1).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',596).
%~ From E:
%~ 
%~ holds(signalingDecompress(Computer,Diver),Time) ->         not( thereExists( Time1,   Time1<Time,happens(decompress(Diver),Time1)))
%~ cpc :- holds(signalingDecompress(Computer,Diver),Time) ->         not( thereExists( Time1,   Time1<Time,happens(decompress(Diver),Time1)))
%~ ooo :- [   cl(    [   exists( Time1,   ( not(comparison(Time1,Time,<))  ;    not( happens(decompress(Diver),Time1)) ;     not( holds(signalingDecompress(Computer,Diver),Time))))],       [])]
cl(    exists( Time1,   ( not comparison(Time1,Time,<)  ;    not( happens(decompress(Diver),Time1)) ;     not( holds(signalingDecompress(Computer,Diver),Time)))),       []).
 %  ( cl(    exists( Time1,   ( not(comparison(Time1,Time,<))  ;    not( happens(decompress(Diver),Time1)) ;     not( holds(signalingDecompress(Computer,Diver),Time)))),       [])).
 %  % =================================.

% event Decompress(diver)
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',598).
%~ From E:
%~ 
%~ event( decompress(diver))
%~ cpc :- events([decompress/1])
%~ ooo :- [   cl([events([decompress/1])],[])]
%~ cpc :- actions([decompress/1])
%~ ooo :- [   cl([actions([decompress/1])],[])]
%~ cpc :- mpred_prop(decompress(diver),action)
%~ ooo :- [   cl([mpred_prop(decompress(diver),action)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',598).
( cl(events(decompress/1),[])  ,    cl(mpred_prop(decompress(diver),action),[]) ,     cl(actions(decompress/1),[])).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',601).
% event EqualizeEars(diver)
%~ From E:
%~ 
%~ event( equalizeEars(diver))
%~ cpc :- events([equalizeEars/1])
%~ ooo :- [   cl([events([equalizeEars/1])],[])]
%~ cpc :- actions([equalizeEars/1])
%~ ooo :- [   cl([actions([equalizeEars/1])],[])]
%~ cpc :- mpred_prop(equalizeEars(diver),action)
%~ ooo :- [   cl([mpred_prop(equalizeEars(diver),action)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',601).
( cl(events(equalizeEars/1),[])  ,    cl(mpred_prop(equalizeEars(diver),action),[]) ,     cl(actions(equalizeEars/1),[])).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',603).
% [diver,time]
% (Happens(Descend1(diver),time) | Happens(Ascend1(diver),time)) &
% !Happens(EqualizeEars(diver),time) ->
% Happens(StartEarPain(diver),time) &
% Happens(RuptureEardrum(diver),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',603).
%~ From E:
%~ 
%~ (     happens(descend1(Diver),Time);happens(ascend1(Diver),Time) ,     not( happens(equalizeEars(Diver),Time))) ->         happens(startEarPain(Diver),Time),happens(ruptureEardrum(Diver),Time)
%~ cpc :- (     happens(descend1(Diver),Time);happens(ascend1(Diver),Time) ,     not( happens(equalizeEars(Diver),Time))) ->         happens(startEarPain(Diver),Time),happens(ruptureEardrum(Diver),Time)
%~ ooo :- [   cl(    [   happens(startEarPain(Diver),Time),     happens(equalizeEars(Diver),Time)],       [      happens(descend1(Diver),Time)]),     cl(      [   happens(startEarPain(Diver),Time),     happens(equalizeEars(Diver),Time)],           [        happens(ascend1(Diver),Time)]),     cl(      [   happens(ruptureEardrum(Diver),Time),     happens(equalizeEars(Diver),Time)],           [        happens(descend1(Diver),Time)]),     cl(      [   happens(ruptureEardrum(Diver),Time),     happens(equalizeEars(Diver),Time)],           [        happens(ascend1(Diver),Time)])]
( cl(    happens(startEarPain(Diver),Time),happens(equalizeEars(Diver),Time),       happens(descend1(Diver),Time))  ,    cl(      happens(startEarPain(Diver),Time),happens(equalizeEars(Diver),Time),           happens(ascend1(Diver),Time)) ,     cl(      happens(ruptureEardrum(Diver),Time),happens(equalizeEars(Diver),Time),           happens(descend1(Diver),Time)) ,     cl(      happens(ruptureEardrum(Diver),Time),happens(equalizeEars(Diver),Time),           happens(ascend1(Diver),Time))).
 %  ( cl(    happens(startEarPain(Diver),Time),happens(equalizeEars(Diver),Time),       happens(descend1(Diver),Time))  ,    cl(      happens(startEarPain(Diver),Time),happens(equalizeEars(Diver),Time),           happens(ascend1(Diver),Time)) ,     cl(      happens(ruptureEardrum(Diver),Time),happens(equalizeEars(Diver),Time),           happens(descend1(Diver),Time)) ,     cl(      happens(ruptureEardrum(Diver),Time),happens(equalizeEars(Diver),Time),           happens(ascend1(Diver),Time))).
 %  % =================================.


% [diver,time]
% Happens(Ascend1(diver),time) &
% !Happens(Decompress(diver),time) ->
% Happens(StartDecompressionIllness(diver),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',610).
%~ From E:
%~ 
%~ happens(ascend1(Diver),Time),not(happens(decompress(Diver),Time)) ->         happens(startDecompressionIllness(Diver),Time)
%~ cpc :- happens(ascend1(Diver),Time),not(happens(decompress(Diver),Time)) ->         happens(startDecompressionIllness(Diver),Time)
%~ ooo :- [   cl(    [   happens(startDecompressionIllness(Diver),Time),     happens(decompress(Diver),Time)],       [      happens(ascend1(Diver),Time)])]
cl(    (     happens(startDecompressionIllness(Diver),Time) ,     happens(decompress(Diver),Time)),       happens(ascend1(Diver),Time)).
 %  cl(    (     happens(startDecompressionIllness(Diver),Time) ,     happens(decompress(Diver),Time)),       happens(ascend1(Diver),Time)).
 %  % =================================.


% [diver1,diver2,time]
% HoldsAt(Holding(diver1,diver2),time) &
% Happens(Ascend1(diver1),time) &
% !Happens(Decompress(diver2),time) ->
% Happens(StartDecompressionIllness(diver2),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',615).
%~ From E:
%~ 
%~ ( holds(holding(Diver1,Diver2),Time)  ,    happens(ascend1(Diver1),Time) ,     not( happens(decompress(Diver2),Time))) ->         happens(startDecompressionIllness(Diver2),Time)
%~ cpc :- ( holds(holding(Diver1,Diver2),Time)  ,    happens(ascend1(Diver1),Time) ,     not( happens(decompress(Diver2),Time))) ->         happens(startDecompressionIllness(Diver2),Time)
%~ ooo :- [   cl(    [   happens(startDecompressionIllness(Diver2),Time),     happens(decompress(Diver2),Time)],       [      holds(holding(Diver1,Diver2),Time),           happens(ascend1(Diver1),Time)])]
cl(    (     happens(startDecompressionIllness(Diver2),Time) ,     happens(decompress(Diver2),Time)),       (           holds(holding(Diver1,Diver2),Time) ,           happens(ascend1(Diver1),Time))).
 %  cl(    (     happens(startDecompressionIllness(Diver2),Time) ,     happens(decompress(Diver2),Time)),       (           holds(holding(Diver1,Diver2),Time) ,           happens(ascend1(Diver1),Time))).
 %  % =================================.


% [diver,time]
% Happens(Decompress(diver),time) ->
% ({depth} depth>0 & HoldsAt(AtDepth(diver,depth),time)) &
% !HoldsAt(UncontrolledBuoyancy(diver),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',621).
%~ From E:
%~ 
%~ happens(decompress(Diver),Time) ->         (     thereExists(Depth,(Depth>0,holds(atDepth(Diver,Depth),Time))) ,     holds(not(uncontrolledBuoyancy(Diver)),Time))
%~ cpc :- happens(decompress(Diver),Time) ->         (     thereExists(Depth,(Depth>0,holds(atDepth(Diver,Depth),Time))) ,     holds(not(uncontrolledBuoyancy(Diver)),Time))
%~ ooo :- [   cl(    [   exists( Depth,   (     ( comparison(Depth,0,>)  ,        holds(atDepth(Diver,Depth),Time) ,         not( holds(uncontrolledBuoyancy(Diver),Time))) ;     not( happens(decompress(Diver),Time))))],       [])]
cl(    exists( Depth,   (     ( comparison(Depth,0,>)  ,        holds(atDepth(Diver,Depth),Time) ,         not( holds(uncontrolledBuoyancy(Diver),Time))) ;     not( happens(decompress(Diver),Time)))),       []).
 %  cl(    exists( Depth,   (     ( comparison(Depth,0,>)  ,        holds(atDepth(Diver,Depth),Time) ,         not( holds(uncontrolledBuoyancy(Diver),Time))) ;     not( happens(decompress(Diver),Time)))),       []).
 %  % =================================.

% fluent HasHeadache(diver)
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',624).
%~ From E:
%~ 
%~ fluent( hasHeadache(diver))
%~ cpc :- fluents([hasHeadache/1])
%~ ooo :- [   cl([fluents([hasHeadache/1])],[])]
%~ cpc :- mpred_prop(hasHeadache(diver),fluent)
%~ ooo :- [   cl([mpred_prop(hasHeadache(diver),fluent)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',624).
cl(mpred_prop(hasHeadache(diver),fluent),[]),cl(fluents(hasHeadache/1),[]).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',627).
% [diver,time]
% HoldsAt(ConditionOK(diver),time) ->
% !HoldsAt(Unconscious(diver),time) &
% !HoldsAt(HasEarPain(diver),time) &
% !HoldsAt(HasRupturedEardrum(diver),time) &
% !HoldsAt(HasDecompressionIllness(diver),time) &
% !HoldsAt(HasHeadache(diver),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',627).
%~ From E:
%~ 
%~ holds(conditionOK(Diver),Time) ->         ( holds(not(unconscious(Diver)),Time)  ,    holds(not(hasEarPain(Diver)),Time) ,     holds(not(hasRupturedEardrum(Diver)),Time) ,     holds(not(hasDecompressionIllness(Diver)),Time) ,     holds(not(hasHeadache(Diver)),Time))
%~ cpc :- holds(conditionOK(Diver),Time) ->         ( holds(not(unconscious(Diver)),Time)  ,    holds(not(hasEarPain(Diver)),Time) ,     holds(not(hasRupturedEardrum(Diver)),Time) ,     holds(not(hasDecompressionIllness(Diver)),Time) ,     holds(not(hasHeadache(Diver)),Time))
%~ ooo :- [   cl( [], [  [   holds(unconscious(Diver),Time),     holds(conditionOK(Diver),Time)])],     cl( [], [    [   holds(hasEarPain(Diver),Time),     holds(conditionOK(Diver),Time)])],     cl( [], [    [   holds(hasRupturedEardrum(Diver),Time),     holds(conditionOK(Diver),Time)])],     cl( [], [    [   holds(hasDecompressionIllness(Diver),Time),     holds(conditionOK(Diver),Time)])],     cl( [], [    [   holds(hasHeadache(Diver),Time),     holds(conditionOK(Diver),Time)])]]
( cl([],(holds(unconscious(Diver),Time),holds(conditionOK(Diver),Time)))  ,    cl([],(holds(hasEarPain(Diver),Time),holds(conditionOK(Diver),Time))) ,     cl( [],     holds(hasRupturedEardrum(Diver),Time),holds(conditionOK(Diver),Time)) ,     cl( [],     holds(hasDecompressionIllness(Diver),Time),holds(conditionOK(Diver),Time)) ,     cl([],(holds(hasHeadache(Diver),Time),holds(conditionOK(Diver),Time)))).
 %  ( cl([],(holds(unconscious(Diver),Time),holds(conditionOK(Diver),Time)))  ,    cl([],(holds(hasEarPain(Diver),Time),holds(conditionOK(Diver),Time))) ,     cl( [],     holds(hasRupturedEardrum(Diver),Time),holds(conditionOK(Diver),Time)) ,     cl( [],     holds(hasDecompressionIllness(Diver),Time),holds(conditionOK(Diver),Time)) ,     cl([],(holds(hasHeadache(Diver),Time),holds(conditionOK(Diver),Time)))).
 %  % =================================.

% event BeAirlifted(diver)
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',634).
%~ From E:
%~ 
%~ event( beAirlifted(diver))
%~ cpc :- events([beAirlifted/1])
%~ ooo :- [   cl([events([beAirlifted/1])],[])]
%~ cpc :- actions([beAirlifted/1])
%~ ooo :- [   cl([actions([beAirlifted/1])],[])]
%~ cpc :- mpred_prop(beAirlifted(diver),action)
%~ ooo :- [   cl([mpred_prop(beAirlifted(diver),action)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',634).
( cl(events(beAirlifted/1),[])  ,    cl(mpred_prop(beAirlifted(diver),action),[]) ,     cl(actions(beAirlifted/1),[])).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',637).
% event TakeInWater(diver)
%~ From E:
%~ 
%~ event( takeInWater(diver))
%~ cpc :- events([takeInWater/1])
%~ ooo :- [   cl([events([takeInWater/1])],[])]
%~ cpc :- actions([takeInWater/1])
%~ ooo :- [   cl([actions([takeInWater/1])],[])]
%~ cpc :- mpred_prop(takeInWater(diver),action)
%~ ooo :- [   cl([mpred_prop(takeInWater(diver),action)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',637).
( cl(events(takeInWater/1),[])  ,    cl(mpred_prop(takeInWater(diver),action),[]) ,     cl(actions(takeInWater/1),[])).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',639).
% fluent LowOnAir(airtank)
%~ From E:
%~ 
%~ fluent( lowOnAir(airtank))
%~ cpc :- fluents([lowOnAir/1])
%~ ooo :- [   cl([fluents([lowOnAir/1])],[])]
%~ cpc :- mpred_prop(lowOnAir(airtank),fluent)
%~ ooo :- [   cl([mpred_prop(lowOnAir(airtank),fluent)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',639).
cl(mpred_prop(lowOnAir(airtank),fluent),[]),cl(fluents(lowOnAir/1),[]).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',641).
% event BecomeLowOnAir(airtank)
%~ From E:
%~ 
%~ event( becomeLowOnAir(airtank))
%~ cpc :- events([becomeLowOnAir/1])
%~ ooo :- [   cl([events([becomeLowOnAir/1])],[])]
%~ cpc :- mpred_prop(becomeLowOnAir(airtank),event)
%~ ooo :- [   cl([mpred_prop(becomeLowOnAir(airtank),event)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',641).
cl(mpred_prop(becomeLowOnAir(airtank),event),[]),cl(events(becomeLowOnAir/1),[]).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',643).
% [airtank,time]
% Initiates(BecomeLowOnAir(airtank),LowOnAir(airtank),time).
%~ From E:
%~ 
%~ initiates_at(becomeLowOnAir(Airtank),lowOnAir(Airtank),Time)
%~ cpc :- initiates(becomeLowOnAir(Airtank),lowOnAir(Airtank),Time)
%~ ooo :- [   cl([initiates(becomeLowOnAir(Airtank),lowOnAir(Airtank),Time)],[])]
cl(initiates(becomeLowOnAir(Airtank),lowOnAir(Airtank),Time),[]).
 %  cl(initiates(becomeLowOnAir(Airtank),lowOnAir(Airtank),Time),[]).
 %  % =================================.


%; initial state
% [diver]
 % HoldsAt(ConditionOK(diver),0).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',647).
%~ From E:
%~ 
%~ holds(conditionOK(Diver),0)
%~ cpc :- holds(conditionOK(Diver),0)
%~ ooo :- [   cl([holds(conditionOK(Diver),0)],[])]
cl(holds(conditionOK(Diver),0),[]).
 %  cl(holds(conditionOK(Diver),0),[]).
 %  % =================================.


% [diver]
 % HoldsAt(Vertical(diver),0).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',647).
%~ From E:
%~ 
%~ holds(vertical(Diver),0)
%~ cpc :- holds(vertical(Diver),0)
%~ ooo :- [   cl([holds(vertical(Diver),0)],[])]
cl(holds(vertical(Diver),0),[]).
 %  cl(holds(vertical(Diver),0),[]).
 %  % =================================.


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',649).
% !HoldsAt(DisturbedSilt(),0).
%~ From E:
%~ 
%~ holds(not(disturbedSilt()),0)
initially( not disturbedSilt()).
 %  initial_state([not(disturbedSilt)]).
 %  % =================================.


% [diver]
 % !HoldsAt(UncontrolledBuoyancy(diver),0).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',649).
%~ From E:
%~ 
%~ holds(not(uncontrolledBuoyancy(Diver)),0)
%~ cpc :- holds(not(uncontrolledBuoyancy(Diver)),0)
%~ ooo :- [   cl([],[holds(uncontrolledBuoyancy(Diver),0)])]
cl([],holds(uncontrolledBuoyancy(Diver),0)).
 %  cl([],holds(uncontrolledBuoyancy(Diver),0)).
 %  % =================================.


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',651).
% [diver]
 % !HoldsAt(Disoriented(diver),0).
%~ From E:
%~ 
%~ holds(not(disoriented(Diver)),0)
%~ cpc :- holds(not(disoriented(Diver)),0)
%~ ooo :- [   cl([],[holds(disoriented(Diver),0)])]
cl([],holds(disoriented(Diver),0)).
 %  cl([],holds(disoriented(Diver),0)).
 %  % =================================.


% [diver]
 % !HoldsAt(PositivelyBuoyant(diver),0) &
%         !HoldsAt(NeutrallyBuoyant(diver),0) &
%         !HoldsAt(NegativelyBuoyant(diver),0).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',651).
%~ From E:
%~ 
%~ ( holds(not(positivelyBuoyant(Diver)),0)  ,    holds(not(neutrallyBuoyant(Diver)),0) ,     holds(not(negativelyBuoyant(Diver)),0))
%~ cpc :- holds(not(positivelyBuoyant(Diver)),0)
%~ ooo :- [   cl([],[holds(positivelyBuoyant(Diver),0)])]
%~ cpc :- holds(not(neutrallyBuoyant(Diver)),0)
%~ ooo :- [   cl([],[holds(neutrallyBuoyant(Diver),0)])]
%~ cpc :- holds(not(negativelyBuoyant(Diver)),0)
%~ ooo :- [   cl([],[holds(negativelyBuoyant(Diver),0)])]
( cl([],holds(positivelyBuoyant(Diver),0))  ,    cl([],holds(neutrallyBuoyant(Diver),0)) ,     cl([],holds(negativelyBuoyant(Diver),0))).
 %  ( cl([],holds(positivelyBuoyant(Diver),0))  ,    cl([],holds(neutrallyBuoyant(Diver),0)) ,     cl([],holds(negativelyBuoyant(Diver),0))).
 %  % =================================.


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',655).
% [diver,object]
 % !HoldsAt(Wearing(diver,object),0).
%~ From E:
%~ 
%~ holds(not(wearing(Diver,Object)),0)
%~ cpc :- holds(not(wearing(Diver,Object)),0)
%~ ooo :- [   cl([],[holds(wearing(Diver,Object),0)])]
cl([],holds(wearing(Diver,Object),0)).
 %  cl([],holds(wearing(Diver,Object),0)).
 %  % =================================.


% [diver,object]
 % !HoldsAt(Holding(diver,object),0).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',655).
%~ From E:
%~ 
%~ holds(not(holding(Diver,Object)),0)
%~ cpc :- holds(not(holding(Diver,Object)),0)
%~ ooo :- [   cl([],[holds(holding(Diver,Object),0)])]
cl([],holds(holding(Diver,Object),0)).
 %  cl([],holds(holding(Diver,Object),0)).
 %  % =================================.


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',657).
% [diver1,diver2]
 % !HoldsAt(Separated(diver1,diver2),0).
%~ From E:
%~ 
%~ holds(not(separated(Diver1,Diver2)),0)
%~ cpc :- holds(not(separated(Diver1,Diver2)),0)
%~ ooo :- [   cl([],[holds(separated(Diver1,Diver2),0)])]
cl([],holds(separated(Diver1,Diver2),0)).
 %  cl([],holds(separated(Diver1,Diver2),0)).
 %  % =================================.


%;[agent,object] !HoldsAt(See(agent,object),0).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',660).
% fluent Separated(diver,diver)
%~ From E:
%~ 
%~ fluent( separated(diver,diver))
%~ cpc :- fluents([separated/2])
%~ ooo :- [   cl([fluents([separated/2])],[])]
%~ cpc :- mpred_prop(separated(diver,diver),fluent)
%~ ooo :- [   cl([mpred_prop(separated(diver,diver),fluent)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',660).
cl(mpred_prop(separated(diver,diver),fluent),[]),cl(fluents(separated/2),[]).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',662).
% [diver1,diver2,time]
% HoldsAt(Separated(diver1,diver2),time) ->
% HoldsAt(Separated(diver2,diver1),time).
%~ From E:
%~ 
%~ holds(separated(Diver1,Diver2),Time) ->         holds(separated(Diver2,Diver1),Time)
%~ cpc :- holds(separated(Diver1,Diver2),Time) ->         holds(separated(Diver2,Diver1),Time)
%~ ooo :- [   cl(    [   holds(separated(Diver2,Diver1),Time)],       [      holds(separated(Diver1,Diver2),Time)])]
cl(    holds(separated(Diver2,Diver1),Time),       holds(separated(Diver1,Diver2),Time)).
 %  cl(    holds(separated(Diver2,Diver1),Time),       holds(separated(Diver1,Diver2),Time)).
 %  % =================================.

% event BecomeSeparated(diver,diver)
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',665).
%~ From E:
%~ 
%~ event( becomeSeparated(diver,diver))
%~ cpc :- events([becomeSeparated/2])
%~ ooo :- [   cl([events([becomeSeparated/2])],[])]
%~ cpc :- actions([becomeSeparated/2])
%~ ooo :- [   cl([actions([becomeSeparated/2])],[])]
%~ cpc :- mpred_prop(becomeSeparated(diver,diver),action)
%~ ooo :- [   cl([mpred_prop(becomeSeparated(diver,diver),action)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',665).
( cl(events(becomeSeparated/2),[])  ,    cl(mpred_prop(becomeSeparated(diver,diver),action),[]) ,     cl(actions(becomeSeparated/2),[])).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',668).
% event BeReunitedWith(diver,diver)
%~ From E:
%~ 
%~ event( beReunitedWith(diver,diver))
%~ cpc :- events([beReunitedWith/2])
%~ ooo :- [   cl([events([beReunitedWith/2])],[])]
%~ cpc :- actions([beReunitedWith/2])
%~ ooo :- [   cl([actions([beReunitedWith/2])],[])]
%~ cpc :- mpred_prop(beReunitedWith(diver,diver),action)
%~ ooo :- [   cl([mpred_prop(beReunitedWith(diver,diver),action)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',668).
( cl(events(beReunitedWith/2),[])  ,    cl(mpred_prop(beReunitedWith(diver,diver),action),[]) ,     cl(actions(beReunitedWith/2),[])).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',670).
% [diver1,diver2,time]
% Initiates(BecomeSeparated(diver1,diver2),Separated(diver1,diver2),time).
%~ From E:
%~ 
%~ initiates_at( becomeSeparated(Diver1,Diver2),   separated(Diver1,Diver2),     Time)
%~ cpc :- initiates( becomeSeparated(Diver1,Diver2),   separated(Diver1,Diver2),     Time)
%~ ooo :- [   cl(    [   initiates( becomeSeparated(Diver1,Diver2),   separated(Diver1,Diver2),     Time)],       [])]
cl(    initiates( becomeSeparated(Diver1,Diver2),   separated(Diver1,Diver2),     Time),       []).
 %  cl(    initiates( becomeSeparated(Diver1,Diver2),   separated(Diver1,Diver2),     Time),       []).
 %  % =================================.


% [diver1,diver2,time]
% Initiates(BecomeSeparated(diver1,diver2),Separated(diver2,diver1),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',674).
%~ From E:
%~ 
%~ initiates_at( becomeSeparated(Diver1,Diver2),   separated(Diver2,Diver1),     Time)
%~ cpc :- initiates( becomeSeparated(Diver1,Diver2),   separated(Diver2,Diver1),     Time)
%~ ooo :- [   cl(    [   initiates( becomeSeparated(Diver1,Diver2),   separated(Diver2,Diver1),     Time)],       [])]
cl(    initiates( becomeSeparated(Diver1,Diver2),   separated(Diver2,Diver1),     Time),       []).
 %  cl(    initiates( becomeSeparated(Diver1,Diver2),   separated(Diver2,Diver1),     Time),       []).
 %  % =================================.


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',676).
% [diver1,diver2,time]
% Terminates(BeReunitedWith(diver1,diver2),Separated(diver1,diver2),time).
%~ From E:
%~ 
%~ terminates_at( beReunitedWith(Diver1,Diver2),   separated(Diver1,Diver2),     Time)
%~ cpc :- terminates( beReunitedWith(Diver1,Diver2),   separated(Diver1,Diver2),     Time)
%~ ooo :- [   cl(    [   terminates( beReunitedWith(Diver1,Diver2),   separated(Diver1,Diver2),     Time)],       [])]
cl(    terminates( beReunitedWith(Diver1,Diver2),   separated(Diver1,Diver2),     Time),       []).
 %  cl(    terminates( beReunitedWith(Diver1,Diver2),   separated(Diver1,Diver2),     Time),       []).
 %  % =================================.


% [diver1,diver2,time]
% Terminates(BeReunitedWith(diver1,diver2),Separated(diver2,diver1),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',680).
%~ From E:
%~ 
%~ terminates_at( beReunitedWith(Diver1,Diver2),   separated(Diver2,Diver1),     Time)
%~ cpc :- terminates( beReunitedWith(Diver1,Diver2),   separated(Diver2,Diver1),     Time)
%~ ooo :- [   cl(    [   terminates( beReunitedWith(Diver1,Diver2),   separated(Diver2,Diver1),     Time)],       [])]
cl(    terminates( beReunitedWith(Diver1,Diver2),   separated(Diver2,Diver1),     Time),       []).
 %  cl(    terminates( beReunitedWith(Diver1,Diver2),   separated(Diver2,Diver1),     Time),       []).
 %  % =================================.


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',682).
%; End of file.
%~ From E:
%~ 
%~ :-( call_pel_directive( translate(ending,'/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.lps.pl')))
:-( call_pel_directive( translate(ending,'/opt/logicmoo_workspace/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.lps.pl'))).
