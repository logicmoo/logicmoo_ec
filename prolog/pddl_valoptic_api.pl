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
  ]).

:- reexport(pddl_valoptic_interface).

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

