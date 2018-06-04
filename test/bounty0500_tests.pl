/*

root@gitlab:/opt/logicmoo_workspace/packs_sys/pddl_valoptic_api# cd t
root@gitlab:/opt/logicmoo_workspace/packs_sys/pddl_valoptic_api/t# swipl bounty0500_tests.pl

*/
% DEFINITION OF DELIVERY ROBOT WORLD IN STRIPS NOTATION
:- use_module(library(pddl_valoptic_api)).

:- optic_workspace(add,delrob_problem).

:- include(prolog_files/delrob/delrob_strips).

:- WS = delrob_problem,
   NewWS = delrob_problem_after_step_1,
   optic_get_plan(WS,at(parcel,o111),[Step1|Rest]),
   optic_debug(plan=[Step1|Rest]),
   optic_apply_step(WS,Step1,NewWS),
   optic_get_plan(NewWS,at(parcel,o111),NewPlan),
   optic_debug(nextplan=NewPlan),
   assertion(Rest = NewPlan).

:- optic_workspace(add,delrob_repeat).

:- include(prolog_files/delrob/delrob_strips).

:- WS = delrob_repeat,
   NewWS = delrob_repeat_after_step_1,
   optic_get_plan(WS,at(parcel,o111),[Step1|Rest]),
   optic_debug(plan=[Step1|Rest]),
   optic_apply_step(WS,Step1,NewWS),
   optic_get_plan(NewWS,at(parcel,o111),NewPlan),
   optic_debug(nextplan=NewPlan),
   assertion(Rest = NewPlan).

