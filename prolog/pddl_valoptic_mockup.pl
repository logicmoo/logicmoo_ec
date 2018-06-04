/*  Part of Optic Planner interface for SWI-Prolog

    Author:        Andrew Dougherty, Douglas Miles
    E-mail:        andrewdo@frdcsa.org, logicmoo@gmail.com
    WWW:           https://github.com/TeamSPoon/pddl_valoptic_api
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

:- module(pddl_valoptic_interface, [
   optic_workspace/2,   % optic_workspace(Opr,W)
   optic_requirement/3,   % optic_requirement(Opr,W,Require)          
   optic_init/3, % optic_init(Opr,W,Fact)
   optic_predicate/3, % optic_predicate(Opr,W,Predicate)
   optic_function/3, % optic_function(Opr,W,Function)
   optic_type/3, % optic_type(Opr,W,Sort)
   optic_object/3, % optic_object(Opr,W,Object)
   optic_derived/4, % optic_derived(Opr,W,Fact,Condition) 
   optic_axiom/3, % optic_axiom(Opr,W,Axiom)
   optic_action/4, % optic_action(Opr,W,Action,Info)
   optic_copy_workspace/2, % optic_copy_workspace(+W,?NewWorkspace)
   optic_load_pddl_file/2, % optic_load_pddl_file(+W,+FileName)

   optic_get_plan/3, % optic_get_plan(+W,+Goal,-Plan)
   optic_apply_step/3, % optic_apply_step(+W,+Step,-NewWorkspace)
   ensure_optic/0,
   optic_debug/1

  ]).


optic_copy_workspace(W,NewWorkspace):- optic_missing(optic_copy_workspace(W,NewWorkspace)).

optic_load_pddl_file(W,FileName):- optic_missing(optic_load_pddl_file(W,FileName)).

%% optic_workspace(+current,?W) is nondet.
%
% Enumerates Planner workspace names
%

%% optic_workspace(+add,+W) is det.
%
% Adds a workspace name
%

%% optic_workspace(+rem,+W) is semidet.
%
% Deletes workspace freeing up resources
%
optic_workspace(Opr,W):- call_mockup(optic_workspace(Opr,W)).

optic_requirement(Opr,W,Require):- call_mockup(optic_requirement(Opr,W,Require)).

optic_init(Opr,W,Fact):- 
  glean_objs(Opr,W,Fact),
  call_mockup(optic_init(Opr,W,Fact)).

optic_predicate(Opr,W,Predicate):- 
  glean_types(Opr,W,Predicate),
  call_mockup(optic_predicate(Opr,W,Predicate)).

optic_function(Opr,W,Function):- 
  glean_types(Opr,W,Function),
  call_mockup(optic_function(Opr,W,Function)).

optic_type(Opr,W,Type):-
  glean_types(Opr,W,Type),
  call_mockup(optic_type(Opr,W,Type)).

optic_object(Opr,W,Object):- 
  glean_types(Opr,W,Object),
  call_mockup(optic_object(Opr,W,Object)).

optic_derived(Opr,W,Fact,Cond) :- Cond==[], !, optic_init(Opr,W,Fact).
optic_derived(Opr,W,Fact,Condition) :- 
  call_mockup(optic_derived(Opr,W,Fact,Condition)).


optic_axiom(Opr,W,Axiom):- call_mockup(optic_axiom(Opr,W,Axiom)).

optic_action(Opr,W,Action,Info):- check_opr(W,Opr), 
  glean_types(Opr,W,Action),
  call_mockup(optic_action(Opr,W,Action,Info)).


%% optic_get_plan(+W,+Goal,-Plan) is nondet.
optic_get_plan(W,Goal,Plan):-
  check_workspace(W),
  ignore(Plan=[s1,s2,s3]),
  optic_missing(optic_get_plan(W,Goal,Plan)).

%% optic_apply_step(+W,+Step,-NewWorkspace) is det.
optic_apply_step(W,Step,NewWorkspace):-
 check_workspace(W),
 (var(NewWorkspace)->gensym(W,NewWorkspace);true),
 check_workspace(NewWorkspace),
 optic_missing(optic_apply_step(W,Step,NewWorkspace)).


ensure_optic.

glean_types(Opr,W,Any):- Opr=add,!,
  check_opr(W,Opr),
  forall((sub_term(Sub,Any),
  compound(Sub),member(Sub,[_-Type, Type:_])),
  optic_type(Opr,W,Type)).
glean_types(_,_,_).   


glean_objs(Opr,W,Any):- Opr=add,!,
  forall((sub_term(Sub,Any),
   compound(Sub),member(Sub,[Obj-_])),
   optic_object(Opr,W,Obj)),
  forall((sub_term(Obj,Any),
  atom(Obj)),optic_object(Opr,W,Obj)).
glean_objs(_,_,_).   

check_opr(W,add):- check_workspace(W).
check_opr(W,del):- check_workspace(W).
check_opr(W,current):- check_workspace(W).
check_opr(_Workspace,Opr):- throw(opr_missing(Opr)).

:- dynamic(current_workspace/1).

check_workspace(W):- current_workspace(W),!.
check_workspace(W):- asserta(current_workspace(W)).

call_mockup(Goal):- 
  arg(1,Goal,Opr),
  arg(2,Goal,W),
  check_opr(W,Opr),
  optic_missing(Goal).

optic_debug(Info):- format('~N% ~q.~n',[Info]).

optic_missing(Goal):- !,optic_debug(g(Goal)).
optic_missing(Goal):- throw(optic_missing(Goal)).



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




