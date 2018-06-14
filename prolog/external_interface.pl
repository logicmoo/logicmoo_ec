/*  Part of Optic Planner interface for SWI-Prolog

    Author:        Andrew Dougherty, Douglas Miles
    E-mail:        andrewdo@frdcsa.org, logicmoo@gmail.com
    WWW:           https://github.com/TeamSPoon/planner_external_api
    Copyright (C): 2017, Process Design Center, Breda, The Netherlands.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

/*
":typing" | ":strips" | ":equality" | ":fluents" | ":durative-actions" | ":duration-inequalities" | ":numeric-fluents" | ":action-costs" | ":adl" | ":negative-preconditions" | ":disjunctive-preconditions" | ":existential-preconditions" | "universal-preconditions" | "quantified-preconditions" | ":conditional-effects" | ":timed-initial-literals" | ":preferences" | ":constraints"

":domain-axioms" | ":derived-predicates" ":action-expansions" | ":foreach-expansions" | ":dag-expansions" | ":subgoal-through-axioms" | ":safety-constraints" | ":expression-evaluation" | ":open-world" | ":true-negation" | ":ucpop"

*/

:- module(planner_external_interface, [
   planner_program/2,   % planner_program(Opr,Program)
   planner_workspace/2,   % planner_workspace(Opr,W)
   planner_workspace_program/3, % planner_workspace_program(Opr,W,Program)
   planner_requirement/3,   % planner_requirement(Opr,W,Require)
   planner_init/3, % planner_init(Opr,W,Fact)
   planner_predicate/3, % planner_predicate(Opr,W,Predicate)
   planner_function/3, % planner_function(Opr,W,Function)
   planner_type/3, % planner_type(Opr,W,Sort)
   planner_object/3, % planner_object(Opr,W,Object)
   planner_derived/4, % planner_derived(Opr,W,Fact,Condition) 
   planner_axiom/3, % planner_axiom(Opr,W,Axiom)
   planner_action/4, % planner_action(Opr,W,Action,Info)
   planner_copy_workspace/2, % planner_copy_workspace(+W,?NewWorkspace)
   planner_load_file/2, % planner_load_file(+W,+FileName)

   planner_get_plan/3, % planner_get_plan(+W,+Goal,-Plan)
   planner_get_plan/4, % planner_get_plan(+W,+Planner,+Goal,-Plan)
   planner_apply_step/3, % planner_apply_step(+W,+Step,-NewWorkspace)
   planner_apply_step/4, % planner_apply_step(+W,+Planner,+Step,-NewWorkspace)
   ensure_external_planners/0,
   planner_debug/1

  ]).


planner_copy_workspace(W,NewWorkspace):- planner_missing(planner_copy_workspace(W,NewWorkspace)).

planner_load_file(W,FileName):- planner_missing(planner_load_file(W,FileName)).

%! planner_workspace(+add,+Workspace) is det.
%! planner_workspace(+rem,+Workspace) is semidet.
%! planner_workspace(+current,?Workspace) is nondet.
%
% add = Adds a workspace name
% rem = Deletes workspace freeing up resources
% current = Enumerates workspace names
%
planner_workspace(Opr,Workspace):- call_settings_data(Opr,current_planner_workspace(Workspace)).


%! planner_program(+add,+Program) is det.
%! planner_program(+rem,+Program) is semidet.
%! planner_program(+current,?Program) is nondet.
%
% add = Adds a planner program name
% rem = Deletes program freeing up resources
% current = Enumerates planner program names
%
planner_program(Opr,Program):- call_settings_data(Opr,current_planner_program(Program)).

pre_existing_clause(MData,R):- strip_module(MData,M,Data),
  clause(M:Data,true,R),clause(MCData,true,R),strip_moidule(MCData,_,CData),Data=@=CData,!.

to_mdata(Data,mdata:BData):- strip_module(Data,_,BData).

call_settings_data(Opr,Data):- to_mdata(Data,MData), call_settings_mdata(Opr,Data).

call_settings_mdata(current,MData):- !, call(MData).
call_settings_mdata(add,MData):-  !, (pre_existing_clause(MData,_R)->true;asserta(MData)).
call_settings_mdata(rem,MData):- ignore(call(MData)),retractall(MData).

call_data_mockup(Goal):- 
  Goal=..[Type,Opr,W,Data],
  check_opr(W,Opr),
  atom_concat(pd_,Type,Pred),
  DataPred=..[Pred,W,Data],
  call_settings_data(Opr,DataPred).


% Manipulate PDDL Workspace Dfault Planner Program
planner_workspace_program(Opr,W,Program):- 
  (call_settings_data(Opr,current_planner_workspace_program(W,Program))
   *->true;
   call_settings_data(Opr,current_planner_program(Program))).
  

% Manipulate PDDL Workspace Problem/Domains (:Requirements ...)
planner_requirement(Opr,W,Require):- call_data_mockup(requirement(Opr,W,Require)).

% Manipulate PDDL Workspace Problem/Domains (:Init ...)
planner_init(Opr,W,Fact):- 
  glean_objs(Opr,W,Fact),
  call_data_mockup(init(Opr,W,Fact)).

% Manipulate PDDL Workspace Problem/Domains (:Predicates ...)
planner_predicate(Opr,W,Predicate):- 
  glean_types(Opr,W,Predicate),
  call_data_mockup(predicate(Opr,W,Predicate)).

% Manipulate PDDL Workspace Problem/Domains (:Functions ...)
planner_function(Opr,W,Function):- 
  glean_types(Opr,W,Function),
  call_data_mockup(function(Opr,W,Function)).

% Manipulate PDDL Workspace Problem/Domains (:TYPE ...)
planner_type(Opr,W,Type):-
  glean_types(Opr,W,Type),
  call_data_mockup(type(Opr,W,Type)).

% Manipulate PDDL Workspace Problem/Domains (:OBJECTS ...)
planner_object(Opr,W,Object):- 
  glean_types(Opr,W,Object),
  call_data_mockup(object(Opr,W,Object)).

% Manipulate a PDDL Workspace Problem/Domains (:derived-predicates ...)
planner_derived(Opr,W,Fact,Cond) :- Cond==[], !, planner_init(Opr,W,Fact).
planner_derived(Opr,W,Fact,Condition) :- 
  call_data_mockup(derived(Opr,W,(Fact:-Condition))).


% Manipulate a PDDL Workspace Problem/Domains (:axiom ...)
planner_axiom(Opr,W,Axiom):- call_data_mockup(axiom(Opr,W,Axiom)).

% Manipulate a PDDL Workspace Problem/Domains (:action Action (...Info...))
planner_action(Opr,W,Action,Info):- check_opr(W,Opr), 
  glean_types(Opr,W,Action),
  call_data_mockup(action(Opr,W,act_inf(Action,Info))).

%% planner_get_plan(+W,+Goal,-Plan) is nondet.
planner_get_plan(W,Goal,Plan):-
  planner_workspace_program(current,W,Planner),
  planner_get_plan(Planner,W,Goal,Plan).

%% planner_get_plan(+Planner,+W,+Goal,-Plan) is nondet.
planner_get_plan(Planner,W,Goal,Plan):-   
  check_workspace(W),
  ignore(Plan=[s1,s2,s3]),
  planner_missing(planner_get_plan(W,Goal,Plan)).

%% planner_apply_step(+W,+Step,-NewWorkspace) is det.
planner_apply_step(W,Step,NewWorkspace):-
  planner_program(current,W,Planner),
  planner_apply_step(Planner,W,Step,NewWorkspace).

%% planner_apply_step(+Planner,+W,+Step,-NewWorkspace) is det.
planner_apply_step(Planner,W,Step,NewWorkspace):-
 check_workspace(W),
 (var(NewWorkspace)->gensym(W,NewWorkspace);true),
 check_workspace(NewWorkspace),
 planner_missing(planner_apply_step(W,Step,NewWorkspace)).


ensure_external_planners.

glean_types(Opr,W,Any):- Opr=add,!,
  check_opr(W,Opr),
  forall((sub_term(Sub,Any),
  compound(Sub),member(Sub,[_-Type, Type:_])),
  planner_type(Opr,W,Type)).
glean_types(_,_,_).   


glean_objs(Opr,W,Any):- Opr=add,!,
  forall((sub_term(Sub,Any),
   compound(Sub),member(Sub,[Obj-_])),
   planner_object(Opr,W,Obj)),
  forall((sub_term(Obj,Any),
  atom(Obj)),planner_object(Opr,W,Obj)).
glean_objs(_,_,_).   

check_opr(W,add):- check_workspace(W).
check_opr(W,del):- check_workspace(W).
check_opr(W,current):- check_workspace(W).
check_opr(_Workspace,Opr):- throw(opr_missing(Opr)).

:- dynamic(current_workspace/1).

check_workspace(W):- current_workspace(W),!.
check_workspace(W):- asserta(current_workspace(W)).

planner_debug(Info):- format('~N% ~q.~n',[Info]).

planner_missing(Goal):- !,planner_debug(g(Goal)).
planner_missing(Goal):- throw(planner_missing(Goal)).



%e_member([L|ST],E):- nonvar(L),!,member(E,[L|ST]).
%e_member(E,E).

end_of_file.

/*
(:constraints (and (always-until (charged ?r) (at ?r rechargepoint)) 
  (always-within 10 (< (charge ?r) 5) (at ?r rechargingpoint))))

(:constraints
    (and (preference
         (always (forall (?b1 ?b2 - block ?c1 ?c2 - color)
                         (implies (and (on ?b1 ?b2)
                                       (color ?b1 ?c1)
                                       (color ?b2 ?c2))
                                       (= ?c1 ?c2))))))
)

(:constraints
    (and (always (forall (?b1 ?b2 - block)
                 (implies (and (fragile ?b1) (on ?b2 ?b1))
                               (clear ?b2)))))
)

*/




