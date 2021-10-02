:- include('../ec_test_incl').
/*

   Test A

*/

do_ec_test(stdtest+3) :-
     testing_msg('Test 3'),
     abdemo_special(easy,[holds_at(have(o1),t), holds_at(have(o2),t),
          holds_at(have(o3),t)], R).

do_ec_test(stdtest+4) :-
     testing_msg('Test 4'),
     abdemo_special(easy,[holds_at(have(o1),t), holds_at(have(o2),t),
          holds_at(have(o3),t), holds_at(have(o4),t)], R).

do_ec_test(stdtest+5) :-
     abdemo_special(easy,[holds_at(have(o1),t), holds_at(have(o2),t),
          holds_at(have(o3),t), holds_at(have(o4),t),
          holds_at(have(o5),t)], R).

do_ec_test(stdtest+6) :-
     testing_msg('Test 6'),
     abdemo_special(easy,[holds_at(have(o1),t), holds_at(have(o2),t),
          holds_at(have(o3),t), holds_at(have(o4),t),
          holds_at(have(o5),t), holds_at(have(o6),t)], R).

do_ec_test(stdtest+7) :-
     abdemo_special(easy,[holds_at(have(o1),t), holds_at(have(o2),t),
          holds_at(have(o3),t), holds_at(have(o4),t),
          holds_at(have(o5),t), holds_at(have(o6),t),
          holds_at(have(o7),t)], R).



do_ec_test(stdtest+8) :-
     testing_msg('Test 8 - 111 sicstus'),
     abdemo_special(easy,[holds_at(have(o1),t), holds_at(have(o2),t),
          holds_at(have(o3),t), holds_at(have(o4),t),
          holds_at(have(o5),t), holds_at(have(o6),t),
          holds_at(have(o7),t), holds_at(have(o8),t)], R).

do_ec_test(benchtest+9) :-
     abdemo_special(long,[holds_at(have(o1),t), holds_at(have(o2),t),
          holds_at(have(o3),t), holds_at(have(o4),t),
          holds_at(have(o5),t), holds_at(have(o6),t),
          holds_at(have(o7),t), holds_at(have(o8),t),
          holds_at(have(o9),t)], R).

do_ec_test(benchtest+10+long) :-
     testing_msg('Test 10'),
     abdemo_special(long,[holds_at(have(o1),t), holds_at(have(o2),t),
          holds_at(have(o3),t), holds_at(have(o4),t),
          holds_at(have(o5),t), holds_at(have(o6),t),
          holds_at(have(o7),t), holds_at(have(o8),t),
          holds_at(have(o9),t), holds_at(have(o10),t)], R).

do_ec_test(benchtest+12+long) :-
     testing_msg('Test 12'),
     abdemo_special(long,[holds_at(have(o1),t), holds_at(have(o2),t),
          holds_at(have(o3),t), holds_at(have(o4),t),
          holds_at(have(o5),t), holds_at(have(o6),t),
          holds_at(have(o7),t), holds_at(have(o8),t),
          holds_at(have(o9),t), holds_at(have(o10),t),
          holds_at(have(o11),t), holds_at(have(o12),t)], R).

do_ec_test(benchtest+14+long) :-
     testing_msg('Test 14'),
     abdemo_special(long,[holds_at(have(o1),t), holds_at(have(o2),t),
          holds_at(have(o3),t), holds_at(have(o4),t),
          holds_at(have(o5),t), holds_at(have(o6),t),
          holds_at(have(o7),t), holds_at(have(o8),t),
          holds_at(have(o9),t), holds_at(have(o10),t),
          holds_at(have(o11),t), holds_at(have(o12),t),
          holds_at(have(o13),t), holds_at(have(o14),t)], R).

do_ec_test(benchtest+16+long) :-
     testing_msg('Test 16'),
     abdemo_special(long,[
          holds_at(have(o1),t), 
          holds_at(have(o2),t),
          holds_at(have(o3),t), holds_at(have(o4),t),
          holds_at(have(o5),t), holds_at(have(o6),t),
          holds_at(have(o7),t), holds_at(have(o8),t),
          holds_at(have(o9),t), holds_at(have(o10),t),
          holds_at(have(o11),t), holds_at(have(o12),t),
          holds_at(have(o13),t), holds_at(have(o14),t),
          holds_at(have(o15),t), holds_at(have(o16),t)], R).


make_test_n(Max,Haves):-
  findall(holds_at(have(O),t),
    (between(1,Max,N),atom_concat(o,N,O)),Haves).
   
do_ec_test(benchtest+19+long) :-
     testing_msg('Test 19'),
     make_test_n(19, Haves),
     dbginfo(haves=Haves),!,
     abdemo_special(long, Haves, R).

do_ec_test(benchtest+20+long) :-
     testing_msg('Test 20'),
     make_test_n(20, Haves),
     dbginfo(haves=Haves),!,
     abdemo_special(long, Haves, R).



:- use_module(library(ec_planner/ec_loader)).

% axiom(initiates(go(X),at(X),T),[]).
% axiom(terminates(go(X),at(Y),T),[diff(X,Y)]).
event(go(store)).
fluent(at(store)).
%:- rtrace.
initiates(go(X),at(X)).
%:- break.
terminates(go(X), at(Y)) <- X \= Y.

% axiom(initiates(buy(X),have(X),T),[sells(Y,X), holds_at(at(Y),T)]).
event(buy(object)).
fluent(at(store)).
predicate(sells(store,object)).
fluent(have(object)).
initiates(buy(X), have(X)) <- sells(Y, X), at(Y).


:- 
  forall((between(1, 64, N), 
        atom_concat(s,N,S),atom_concat(o,N,O)),
    process_ec(sells(S,O))).

/*
axiom(sells(s1,o1),[]).
axiom(sells(s2,o2),[]).
axiom(sells(s3,o3),[]).
axiom(sells(s4,o4),[]).
axiom(sells(s5,o5),[]).
axiom(sells(s6,o6),[]).
axiom(sells(s7,o7),[]).
axiom(sells(s8,o8),[]).
axiom(sells(s9,o9),[]).
axiom(sells(s10,o10),[]).
axiom(sells(s11,o11),[]).
axiom(sells(s12,o12),[]).
axiom(sells(s13,o13),[]).
axiom(sells(s14,o14),[]).
axiom(sells(s15,o15),[]).
axiom(sells(s16,o16),[]).
*/





/* Abduction policy */

%abducible(dummy).
%executable(go(X)).
%executable(buy(X)).


:- listing([ec_current_domain_db, axiom]).

