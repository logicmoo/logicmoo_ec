:- include('../ec_test_incl').

        /*

   Formulae for the mail delivery domain.

   Example queries:

   :- abdemo([holds_at(inRoom(p1,r3),T)],R).

   :- abdemo([holds_at(inRoom(p1,r3),T),holds_at(neg(hasGot(Agnt,p1)),T)],R).

*/

do_ec_test(mail1)   :- abdemo_special(loops,[holds_at(inRoom(p1,r2),t)],R).
do_ec_test(mail2)   :- abdemo_special(loops,[holds_at(inRoom(p1,r3),t)],R).
do_ec_test(mail2T)   :- abdemo_special(loops,[holds_at(inRoom(p1,r3),T)],R).
do_ec_test(mail3)   :- abdemo_special(loops,[holds_at(inRoom(p1,r3),T),holds_at(neg(hasGot(agent(1),p1)),T)],R).


/* There should probably be some releases clauses for compound actions */

/* Compound actions */


axiom(happens(do(Agnt,shift_pack(P,R)),T1,T4),
     [happens(do(Agnt,retrieve_pack(P)),T1,T2),
      before(T2,T3), before(T3,T4),
      happens(do(Agnt,deliver_pack(P,R)),T3,T4),
     not(clipped(T2,hasGot(Agnt,P),T3))]).

axiom(initiates(do(Agnt,shift_pack(P,R)),inRoom(P,R),T),[]).


axiom(happens(do(Agnt,retrieve_pack(P)),T1,T2),
     [holds_at(inRoom(P,R),T1), happens(do(Agnt,go_to_room(R)),T1),
      happens(do(Agnt,pickup(P)),T2), before(T1,T2),
      not(clipped(T1,inRoom(Agnt,R),T2))]).

axiom(initiates(do(Agnt,retrieve_pack(P)),hasGot(Agnt,P),T),[]).


axiom(happens(do(Agnt,deliver_pack(P,R)),T1,T2),
     [happens(do(Agnt,go_to_room(R)),T1),
      happens(do(Agnt,putdown(P)),T2), before(T1,T2),
      not(clipped(T1,inRoom(Agnt,R),T2))]).

axiom(initiates(do(Agnt,deliver_pack(P,R)),inRoom(P,R),T),[holds_at(hasGot(Agnt,P),T)]).



/* Primitive actions */


axiom(initiates(do(Agnt,pickup(P)),hasGot(Agnt,P),T),
     [diff(P,Agnt), holds_at(inRoom(P,R),T), holds_at(inRoom(Agnt,R),T)]).

axiom(releases(do(Agnt,pickup(P)),inRoom(P,R),T),
     [diff(P,Agnt), holds_at(inRoom(P,R),T), holds_at(inRoom(Agnt,R),T)]).


axiom(initiates(do(Agnt,putdown(P)),inRoom(P,R),T),
     [diff(P,Agnt), holds_at(hasGot(Agnt,P),T), holds_at(inRoom(Agnt,R),T)]).

axiom(terminates(do(Agnt,putdown(P)),hasGot(Agnt,P),T),[]).

axiom(initiates(do(Agnt,go_to_room(R)),inRoom(Agnt,R),T),[]).

axiom(terminates(do(Agnt,go_to_room(R1)),inRoom(Agnt,R2),T),[diff(R1,R2)]).



/* Domain constraints */


axiom(holds_at(inRoom(P,R),T),
     [diff(P,Agnt), holds_at(hasGot(Agnt,P),T), holds_at(inRoom(Agnt,R),T)]).



/* Narrative */


initially(inRoom(Agnt,r1)).

initially(neg(inRoom(Agnt,r2))).

initially(neg(inRoom(Agnt,r3))).

initially(inRoom(p1,r2)).

initially(neg(inRoom(p1,r1))).

initially(neg(inRoom(p1,r3))).



/* Abduction policy */


abducible(dummy).

/*
executable(do(Agnt,pickup(P))).

executable(do(Agnt,putdown(P))).

executable(do(Agnt,go_to_room(R))).
*/
executable(do(Agn,Act)).

:- run_ec_tests.

:- halt.

% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/logicmoo_ec/test/ec_planner/ectest/test_mail_7.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.ec.ec_planner.ectest/TEST_MAIL_7/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3ATEST_MAIL_7 
% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/ 

