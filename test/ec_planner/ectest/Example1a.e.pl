:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(pfc).
% Wed, 01 Apr 2020 20:05:36 GMT
% From ../ectest/Example1a.e.pl:4
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_includes.e',257).

 /*  loading(load_e_pl,
   	'/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/Example1a.e').
 */
%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; deduction

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/Example1a.e:12
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/Example1a.e',13).
% option timediff off
:- set_ec_option(timediff, off).
:- if(is_e_toplevel).
:- endif.

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/Example1a.e:14
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/Example1a.e',15).
% load foundations/Root.e
:- load_e('foundations/Root.e', changed).
:- if(is_e_toplevel).
:- endif.

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/Example1a.e:15
% load foundations/EC.e
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/Example1a.e',15).
:- load_e('foundations/EC.e', changed).
:- if(is_e_toplevel).
:- endif.

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/Example1a.e:17
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/Example1a.e',18).
% sort agent
==> sort(agent).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/Example1a.e:19
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/Example1a.e',20).
% fluent Awake(agent)
fluent(awake(agent)).
==> mpred_prop(awake(agent),fluent).
==> meta_argtypes(awake(agent)).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/Example1a.e:20
%;;   executable(wake_up(_X)).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/Example1a.e:21
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/Example1a.e',22).
% event WakeUp(agent)
event(wakeUp(agent)).
==> mpred_prop(wakeUp(agent),event).
==> meta_argtypes(wakeUp(agent)).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/Example1a.e:23
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/Example1a.e',24).
%;;   axiom(initiates(wake_up(X),awake(X),T),[]).
% [agent,time]
 % Initiates(WakeUp(agent),Awake(agent),time).

 /*  [] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          initiates(wakeUp(Agent), awake(Agent), Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/Example1a.e',24).
axiom(initiates(wakeUp(Agent), awake(Agent), Time),
    []).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/Example1a.e:26
% agent James
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/Example1a.e',26).
==> t(agent,james).
%;; axiom(initially(neg(awake(nathan))),[]). 


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/Example1a.e:28
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/Example1a.e',29).
% !HoldsAt(Awake(James),0).

 /*  [] ->
       ta(Ta_Param,
          tvs1=[start],
          tvs2=[start],
          holds_at(neg(awake(james)), start)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/Example1a.e',29).
axiom(holds_at(neg(awake(james)), start),
    []).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/Example1a.e:30
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/Example1a.e',31).
% Delta: 
next_axiom_uses(delta).
 


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/Example1a.e:30
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/Example1a.e',31).
% Happens(WakeUp(James),0).

 /*  [axiom_uses(delta, Axiom_uses_Ret)] ->
       ta(Ta_Param, tvs1=[start], tvs2=[start], happens(wakeUp(james), start)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/Example1a.e',31).
axiom(happens(wakeUp(james), start),
    [axiom_uses(delta, Axiom_uses_Ret)]).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/Example1a.e:32
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/Example1a.e',33).
% completion Delta Happens
==> completion(delta).
==> completion(happens).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/Example1a.e:34
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/Example1a.e',35).
% range time 0 1
==> range(time,0,1).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/Example1a.e:35
% range offset 1 1
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/Example1a.e',35).
==> range(offset,1,1).
%;;   axiom(terminates(fall_asleep(X),awake(Y),T),[]). 
%;;  
%;;   abducible(dummy).
%;; executable(fall_asleep(_X)).
