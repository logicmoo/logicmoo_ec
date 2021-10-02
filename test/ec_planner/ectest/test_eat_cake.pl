
:- include('../ec_test_incl').

/*
   Test queries:

*/

% end_of_file.

do_ec_test(num_cakes(0)).
do_ec_test(num_cakes(1)).

do_ec_test(G) :- G= neg(num_cakes(0)), ec_prove(G).
do_ec_test(G) :- G= neg(num_cakes(1)), ec_prove(G).

do_ec_test(G) :- G= eat_cakes(0), ec_prove(G).
do_ec_test(G) :- G= eat_cakes(1), ec_prove(G).

%do_ec_test(G) :- G= [happens(eat_cakes(1),now),holds_at(eat_cakes(0),now)], fail_solve_goal(G,R).
%do_ec_test(G) :- G= [happens(eat_cakes(1),now),holds_at(eat_cakes(1),now)], ec_prove(G).

do_ec_test(G) :- G= {eat_cakes(1),num_cakes(0)}, ec_prove(G).

do_ec_test(G) :- G= {happens(eat_cakes(1),now),holds_at(num_cakes(1),start)}, ec_prove(G).

do_ec_test(G) :- G= {happens(eat_cakes(1),now),holds_at(num_cakes(1),now-1)}, ec_prove(G).

do_ec_test(G) :- G= {happens(eat_cakes(1),now),holds_at(num_cakes(0),aft)}, ec_prove(G).

% rus out of stack but should just fail
% do_ec_test(G) :- G= {happens(eat_cakes(1),start),holds_at(num_cakes(1),aft)}, ec_prove(G).
fluent(num_cakes(integer)).


axiom(initially(hypothesizing(num_cakes(1))),[]).
axiom(initially(hypothesizing(num_cakes(0))),[]).

axiom(initially(num_cakes(1))).

axiom(initiates(eat_cakes(1),num_cakes(0),T), [holds_at(num_cakes(1),T)]).
axiom(terminates(eat_cakes(1),num_cakes(N),T), [holds_at(num_cakes(N),T)]).

%axiom(initiates(eat_cakes(0),num_cakes(0),T), [holds_at(num_cakes(0),T)]).
%axiom(initiates(eat_cakes(0),num_cakes(1),T), [holds_at(num_cakes(1),T)]).

%axiom(initiates(imagine_initiates(Holds),Holds,T), [holds_at(neg(Holds),T),holds_at(hypothesizing(Holds),T)]).
%axiom(terminates(imagine_terminates(Holds),Holds,T), [holds_at(Holds,T),holds_at(hypothesizing(Holds),T)]).

axiom(initiates(imagine_initiates(Propostion),Propostion,T), [holds_at(hypothesizingAboutTruth(Propostion),T)]).
axiom(initiates(imagine_terminates(Propostion),neg(Propostion),T), [holds_at(hypothesizingAboutTruth(Propostion),T)]).
axiom(initiates(imagine_initiates(neg(Propostion)),neg(Propostion),T), [holds_at(hypothesizingAboutTruth(Propostion),T)]).
axiom(initiates(imagine_terminates(neg(Propostion)),Propostion,T), [holds_at(hypothesizingAboutTruth(Propostion),T)]).

axiom(terminates(imagine_initiates(Propostion),neg(Propostion),T), [holds_at(hypothesizingAboutTruth(Propostion),T)]).
axiom(terminates(imagine_terminates(Propostion),Propostion,T), [holds_at(hypothesizingAboutTruth(Propostion),T)]).
axiom(terminates(imagine_initiates(neg(Propostion)),Propostion,T), [holds_at(hypothesizingAboutTruth(Propostion),T)]).
axiom(terminates(imagine_terminates(neg(Propostion)),neg(Propostion),T), [holds_at(hypothesizingAboutTruth(Propostion),T)]).

/*
*/
%axiom(initiates(immagine_initiates(Holds),Holds,T), [holds_at(neg(Holds),T)]).
%axiom(terminates(immagine_terminates(Holds),Holds,T), [holds_at(Holds,T)]).
%axiom(releases(immagine_releases(Holds),Holds,T), [holds_at(Holds,T)]).

axiom(holds_at(num_cakes(0),T),
     [holds_at(neg(num_cakes(1)),T)]).

axiom(holds_at(neg(num_cakes(0)),T),
     [holds_at(num_cakes(1),T)]).
/*
axiom(holds_at(num_cakes(1),T),
     [holds_at(neg(num_cakes(0)),T)]).

% Why causes loops?
axiom(holds_at(neg(num_cakes(1)),T),
     [holds_at(num_cakes(0),T)]):- fail.
*/
/* Abduction policy */

abducible(dummy).

executable(imagine_terminates(_)).
executable(imagine_initiates(_)).
%executable(make_cake(_)).
executable(eat_cakes(_)).
%executable(ignore_cakes(_)).


