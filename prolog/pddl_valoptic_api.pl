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

:- module(pddl_valoptic_api, [
   optic_workspace/2,   % optic_workspace(Opr,Workspace)
   optic_requirements/3,   % optic_requirements(Opr,Workspace,Requires)          
   optic_inits/3, % optic_inits(Opr,Workspace,Facts)
   optic_predicates/3, % optic_predicates(Opr,Workspace,Predicates)
   optic_functions/3, % optic_functions(Opr,Workspace,Functions)
   optic_types/3, % optic_types(Opr,Workspace,Sorts)
   optic_objects/3, % optic_objects(Opr,Workspace,Objects)
   optic_derived/3, % optic_derived(Opr,Workspace,Fact,Conditions) 
   optic_axioms/3, % optic_axioms(Opr,Workspace,Axioms)
   optic_action/3, % optic_action(Opr,Workspace,Action)
   optic_action_property/5, % optic_action_property(Opr,Workspace,Action,Prop,Value)
   optic_copy_workspace/2, % optic_copy_workspace(+Workspace,?NewWorkspace)
   optic_load_pddl_file/2, % optic_load_pddl_file(+Workspace,+FileName)

   optic_get_plan/3, % optic_get_plan(+Workspace,+Goal,-Plan)
   optic_apply_step/3, % optic_apply_step(+Workspace,+Step,-NewWorkspace)
   ensure_optic/0
  ]).


optic_opr_missing(Goal):- arg(1,Goal,Opr),arg(2,Goal,Workspace),check_opr(Workpsace,Opr),
  optic_missing(Goal).

optic_missing(Goal):- throw(optic_missing(Goal)).


optic_workspace(Opr,Workspace):- optic_opr_missing(optic_workspace(Opr,Workspace)).

optic_requirements(Opr,Workspace,Requires):- optic_opr_missing(optic_requirements(Opr,Workspace,Requires)).
   
optic_inits(Opr,Workspace,Facts):- optic_opr_missing(optic_inits(Opr,Workspace,Facts)).

optic_predicates(Opr,Workspace,Predicates):- optic_opr_missing(optic_predicates(Opr,Workspace,Predicates)).

optic_functions(Opr,Workspace,Functions):-optic_opr_missing(optic_functions(Opr,Workspace,Functions)).

optic_types(Opr,Workspace,Sorts):- optic_opr_missing(optic_types(Opr,Workspace,Sorts)).

optic_objects(Opr,Workspace,Objects):- optic_opr_missing(optic_objects(Opr,Workspace,Objects)).

optic_derived(Opr,Workspace,Fact,Conditions) :- optic_opr_missing(optic_derived(Opr,Workspace,Fact,Conditions)).

optic_axioms(Opr,Workspace,Axioms):- optic_opr_missing(optic_axioms(Opr,Workspace,Axioms)).

optic_action(Opr,Workspace,Action):- optic_opr_missing(optic_action(Opr,Workspace,Action)).

optic_action_property(Opr,Workspace,Action,Prop,Value):- optic_opr_missing(optic_action_property(Opr,Workspace,Action,Prop,Value)).

optic_copy_workspace(Workspace,NewWorkspace):- optic_missing(optic_copy_workspace(Workspace,NewWorkspace)).

optic_load_pddl_file(Workspace,FileName):- optic_missing(optic_load_pddl_file(Workspace,FileName)).

%% optic_get_plan(+Workspace,+Goal,-Plan) is nondet.
optic_get_plan(Workspace,Goal,Plan):- optic_missing(optic_get_plan(Workspace,Goal,Plan)).

%% optic_apply_step(+Workspace,+Step,-NewWorkspace) is det.
optic_apply_step(Workspace,Step,NewWorkspace):- optic_missing(optic_apply_step(Workspace,Step,NewWorkspace)).


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

