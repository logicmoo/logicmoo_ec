:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(pfc).
% Wed, 01 Apr 2020 20:05:30 GMT
% From ../ectest/ec_reader_test_foundations.e.pl:4
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_examples.e',9038).

 /*  loading(load_e_pl,
   	'/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e').
 */
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: foundations/Root.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:17
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',18).
% sort boolean
==> sort(boolean).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:18
% sort integer
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',18).
==> sort(integer).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:19
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',20).
% reified sort predicate
reified_sort(predicate).
==> mpred_prop(predicate,reified_sort).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:20
% reified sort function
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',20).
reified_sort(function).
==> mpred_prop(function,reified_sort).
%; End of file.
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: foundations/EC.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
%; Event Calculus (EC)
%;
%; @incollection{MillerShanahan:2002,
%;   author = "Rob Miller and Murray Shanahan",
%;   year = "2002",
%;   title = "Some alternative formulations of the event calculus",
%;   editor = "Antonis C. Kakas and Fariba Sadri",
%;   booktitle = "Computational Logic: Logic Programming and Beyond: Essays in Honour of \uppercase{R}obert \uppercase{A}. \uppercase{K}owalski, Part \uppercase{II}",
%;   series = "Lecture Notes in Computer Science",
%;   volume = "2408",
%;   pages = "452--490",
%;   address = "Berlin",
%;   publisher = "Springer",
%; }
%;

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:56
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',57).
% sort time: integer
==> subsort(time,integer).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:57
% sort offset: integer
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',57).
==> subsort(offset,integer).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:59
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',60).
% reified sort fluent
reified_sort(fluent).
==> mpred_prop(fluent,reified_sort).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:60
% reified sort event
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',60).
reified_sort(event).
==> mpred_prop(event,reified_sort).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:62
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',63).
% predicate Happens(event,time)
predicate(happens(event,time)).
==> mpred_prop(happens(event,time),predicate).
==> meta_argtypes(happens(event,time)).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:63
% predicate HoldsAt(fluent,time)
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',63).
predicate(holds_at(fluent,time)).
==> mpred_prop(holds_at(fluent,time),predicate).
==> meta_argtypes(holds_at(fluent,time)).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:64
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',65).
% predicate ReleasedAt(fluent,time)
predicate(releasedAt(fluent,time)).
==> mpred_prop(releasedAt(fluent,time),predicate).
==> meta_argtypes(releasedAt(fluent,time)).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:65
% predicate Initiates(event,fluent,time)
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',65).
predicate(initiates(event,fluent,time)).
==> mpred_prop(initiates(event,fluent,time),predicate).
==> meta_argtypes(initiates(event,fluent,time)).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:66
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',67).
% predicate Terminates(event,fluent,time)
predicate(terminates(event,fluent,time)).
==> mpred_prop(terminates(event,fluent,time),predicate).
==> meta_argtypes(terminates(event,fluent,time)).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:67
% predicate Releases(event,fluent,time)
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',67).
predicate(releases(event,fluent,time)).
==> mpred_prop(releases(event,fluent,time),predicate).
==> meta_argtypes(releases(event,fluent,time)).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:68
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',69).
% predicate Trajectory(fluent,time,fluent,offset)
predicate(trajectory(fluent,time,fluent,offset)).
==> mpred_prop(trajectory(fluent,time,fluent,offset),predicate).
==> meta_argtypes(trajectory(fluent,time,fluent,offset)).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:70
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',71).
%; End of file.
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: foundations/DEC.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
%; Discrete Event Calculus (DEC)
%;
%; @article{Mueller:2004a,
%;   author = "Erik T. Mueller",
%;   year = "2004",
%;   title = "Event calculus reasoning through satisfiability",
%;   journal = "Journal of Logic and Computation",
%;   volume = "14",
%;   number = "5",
%;   pages = "703--730",
%; }
%;

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:101
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',102).
% sort time: integer
==> subsort(time,integer).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:102
% sort offset: integer
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',102).
==> subsort(offset,integer).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:104
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',105).
% reified sort fluent
reified_sort(fluent).
==> mpred_prop(fluent,reified_sort).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:105
% reified sort event
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',105).
reified_sort(event).
==> mpred_prop(event,reified_sort).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:107
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',108).
% predicate Happens(event,time)
predicate(happens(event,time)).
==> mpred_prop(happens(event,time),predicate).
==> meta_argtypes(happens(event,time)).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:108
% predicate HoldsAt(fluent,time)
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',108).
predicate(holds_at(fluent,time)).
==> mpred_prop(holds_at(fluent,time),predicate).
==> meta_argtypes(holds_at(fluent,time)).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:109
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',110).
% predicate ReleasedAt(fluent,time)
predicate(releasedAt(fluent,time)).
==> mpred_prop(releasedAt(fluent,time),predicate).
==> meta_argtypes(releasedAt(fluent,time)).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:111
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',112).
% predicate Initiates(event,fluent,time)
predicate(initiates(event,fluent,time)).
==> mpred_prop(initiates(event,fluent,time),predicate).
==> meta_argtypes(initiates(event,fluent,time)).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:112
% predicate Terminates(event,fluent,time)
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',112).
predicate(terminates(event,fluent,time)).
==> mpred_prop(terminates(event,fluent,time),predicate).
==> meta_argtypes(terminates(event,fluent,time)).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:113
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',114).
% predicate Releases(event,fluent,time)
predicate(releases(event,fluent,time)).
==> mpred_prop(releases(event,fluent,time),predicate).
==> meta_argtypes(releases(event,fluent,time)).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:115
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',116).
% [fluent,time]
% (HoldsAt(fluent,time) &
%  !ReleasedAt(fluent,time+1) &
%  !({event} Happens(event,time) & Terminates(event,fluent,time))) ->
% HoldsAt(fluent,time+1).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:119
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',116).

 /*  holds_at(Fluent, Time), not(releasedAt(Fluent, Time+1)), not(exists([Event],  (happens(Event, Time), terminates(Event, Fluent, Time)))) ->
       holds_at(Fluent, Time+1).
 */

 /*  holds_at(Fluent, Time+1) :-
       holds_at(Fluent, Time),
       not(releasedAt(Fluent, Time+1)),
       (   not(happens(Event, Time))
       ;   not(terminates(Event, Fluent, Time))
       ).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',116).

 /*  [holds_at(Fluent, Time), not(releasedAt(Fluent, Time+1)),  (not(happens(Event, Time));not(terminates(Event, Fluent, Time))), b(Time, Time3), ignore(Time+1==Time3)] ->
       ta(Time,
          tvs1=[Time, Time+1],
          tvs2=[Time, Time3],
          holds_at(Fluent, Time3)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',116).
axiom(holds_at(Fluent, Time3),
   
    [ holds_at(Fluent, Time),
      not(releasedAt(Fluent, Time3)),
       (not(happens(Event, Time));not(terminates(Event, Fluent, Time))),
      b(Time, Time3)
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',116).

 /*  not(holds_at(Fluent, Time)) :-
       ( not(releasedAt(Fluent, Time+1)),
         (   not(happens(Event, Time))
         ;   not(terminates(Event, Fluent, Time))
         )
       ),
       not(holds_at(Fluent, Time+1)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',116).

 /*  [not(releasedAt(Fluent, Time+1)),  (not(happens(Event, Time));not(terminates(Event, Fluent, Time))), holds_at(neg(Fluent), Time4), b(Time, Time4), ignore(Time+1==Time4)] ->
       ta(Time,
          tvs1=[Time, Time+1],
          tvs2=[Time, Time4],
          holds_at(neg(Fluent), Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',116).
axiom(holds_at(neg(Fluent), Time),
   
    [ not(releasedAt(Fluent, Time4)),
       (not(happens(Event, Time));not(terminates(Event, Fluent, Time))),
      holds_at(neg(Fluent), Time4),
      b(Time, Time4)
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',116).

 /*  releasedAt(Fluent, Time+1) :-
       (   not(happens(Event, Time))
       ;   not(terminates(Event, Fluent, Time))
       ),
       holds_at(Fluent, Time),
       not(holds_at(Fluent, Time+1)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',116).

 /*  [(not(happens(Event, Time));not(terminates(Event, Fluent, Time))), holds_at(Fluent, Time), holds_at(neg(Fluent), Time5), b(Time, Time5), ignore(Time+1==Time5)] ->
       ta(Time,
          tvs1=[Time, Time+1],
          tvs2=[Time, Time5],
          releasedAt(Fluent, Time+1)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',116).
axiom(releasedAt(Fluent, Time5),
   
    [  (not(happens(Event, Time));not(terminates(Event, Fluent, Time))),
      holds_at(Fluent, Time),
      holds_at(neg(Fluent), Time5),
      b(Time, Time5)
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',116).

 /*  happens(Event, Time) :-
       not(releasedAt(Fluent, Time+1)),
       holds_at(Fluent, Time),
       not(holds_at(Fluent, Time+1)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',116).

 /*  [not(releasedAt(Fluent, Time+1)), holds_at(Fluent, Time), holds_at(neg(Fluent), Time6), b(Time, Time6), ignore(Time+1==Time6)] ->
       ta(Time,
          tvs1=[Time, Time+1],
          tvs2=[Time, Time6],
          happens(Event, Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',116).
axiom(happens(Event, Time),
   
    [ not(releasedAt(Fluent, Time6)),
      holds_at(Fluent, Time),
      holds_at(neg(Fluent), Time6),
      b(Time, Time6)
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',116).

 /*  terminates(Event, Fluent, Time) :-
       not(releasedAt(Fluent, Time+1)),
       holds_at(Fluent, Time),
       not(holds_at(Fluent, Time+1)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',116).

 /*  [not(releasedAt(Fluent, Time+1)), holds_at(Fluent, Time), holds_at(neg(Fluent), Time7), b(Time, Time7), ignore(Time+1==Time7)] ->
       ta(Time,
          tvs1=[Time, Time+1],
          tvs2=[Time, Time7],
          terminates(Event, Fluent, Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',116).
axiom(terminates(Event, Fluent, Time),
   
    [ not(releasedAt(Fluent, Time7)),
      holds_at(Fluent, Time),
      holds_at(neg(Fluent), Time7),
      b(Time, Time7)
    ]).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:121
% [fluent,time]
% (!HoldsAt(fluent,time) &
%  !ReleasedAt(fluent,time+1) &
%  !({event} Happens(event,time) & Initiates(event,fluent,time))) ->
% !HoldsAt(fluent,time+1).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:125
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',123).

 /*  holds_at(neg(Fluent), Time), not(releasedAt(Fluent, Time+1)), not(exists([Event],  (happens(Event, Time), initiates(Event, Fluent, Time)))) ->
       holds_at(neg(Fluent), Time+1).
 */

 /*  holds_at(neg(Fluent), Time+1) :-
       holds_at(neg(Fluent), Time),
       not(releasedAt(Fluent, Time+1)),
       (   not(happens(Event, Time))
       ;   not(initiates(Event, Fluent, Time))
       ).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',123).

 /*  [holds_at(neg(Fluent), Time), not(releasedAt(Fluent, Time+1)),  (not(happens(Event, Time));not(initiates(Event, Fluent, Time))), b(Time, Time3), ignore(Time+1==Time3)] ->
       ta(Time,
          tvs1=[Time, Time+1],
          tvs2=[Time, Time3],
          holds_at(neg(Fluent), Time3)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',123).
axiom(holds_at(neg(Fluent), Time3),
   
    [ holds_at(neg(Fluent), Time),
      not(releasedAt(Fluent, Time3)),
       (not(happens(Event, Time));not(initiates(Event, Fluent, Time))),
      b(Time, Time3)
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',123).

 /*  not(holds_at(neg(Fluent), Time)) :-
       ( not(releasedAt(Fluent, Time+1)),
         (   not(happens(Event, Time))
         ;   not(initiates(Event, Fluent, Time))
         )
       ),
       not(holds_at(neg(Fluent), Time+1)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',123).

 /*  [not(releasedAt(Fluent, Time+1)),  (not(happens(Event, Time));not(initiates(Event, Fluent, Time))), holds_at(Fluent, Time4), b(Time, Time4), ignore(Time+1==Time4)] ->
       ta(Time,
          tvs1=[Time, Time+1],
          tvs2=[Time, Time4],
          holds_at(Fluent, Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',123).
axiom(holds_at(Fluent, Time),
   
    [ not(releasedAt(Fluent, Time4)),
       (not(happens(Event, Time));not(initiates(Event, Fluent, Time))),
      holds_at(Fluent, Time4),
      b(Time, Time4)
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',123).

 /*  releasedAt(Fluent, Time+1) :-
       (   not(happens(Event, Time))
       ;   not(initiates(Event, Fluent, Time))
       ),
       holds_at(neg(Fluent), Time),
       not(holds_at(neg(Fluent), Time+1)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',123).

 /*  [(not(happens(Event, Time));not(initiates(Event, Fluent, Time))), holds_at(neg(Fluent), Time), holds_at(Fluent, Time5), b(Time, Time5), ignore(Time+1==Time5)] ->
       ta(Time,
          tvs1=[Time, Time+1],
          tvs2=[Time, Time5],
          releasedAt(Fluent, Time+1)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',123).
axiom(releasedAt(Fluent, Time5),
   
    [  (not(happens(Event, Time));not(initiates(Event, Fluent, Time))),
      holds_at(neg(Fluent), Time),
      holds_at(Fluent, Time5),
      b(Time, Time5)
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',123).

 /*  happens(Event, Time) :-
       not(releasedAt(Fluent, Time+1)),
       holds_at(neg(Fluent), Time),
       not(holds_at(neg(Fluent), Time+1)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',123).

 /*  [not(releasedAt(Fluent, Time+1)), holds_at(neg(Fluent), Time), holds_at(Fluent, Time6), b(Time, Time6), ignore(Time+1==Time6)] ->
       ta(Time,
          tvs1=[Time, Time+1],
          tvs2=[Time, Time6],
          happens(Event, Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',123).
axiom(happens(Event, Time),
   
    [ not(releasedAt(Fluent, Time6)),
      holds_at(neg(Fluent), Time),
      holds_at(Fluent, Time6),
      b(Time, Time6)
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',123).

 /*  initiates(Event, Fluent, Time) :-
       not(releasedAt(Fluent, Time+1)),
       holds_at(neg(Fluent), Time),
       not(holds_at(neg(Fluent), Time+1)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',123).

 /*  [not(releasedAt(Fluent, Time+1)), holds_at(neg(Fluent), Time), holds_at(Fluent, Time7), b(Time, Time7), ignore(Time+1==Time7)] ->
       ta(Time,
          tvs1=[Time, Time+1],
          tvs2=[Time, Time7],
          initiates(Event, Fluent, Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',123).
axiom(initiates(Event, Fluent, Time),
   
    [ not(releasedAt(Fluent, Time7)),
      holds_at(neg(Fluent), Time),
      holds_at(Fluent, Time7),
      b(Time, Time7)
    ]).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:127
% [fluent,time]
% (!ReleasedAt(fluent,time) &
%  !({event} Happens(event,time) & Releases(event,fluent,time))) ->
% !ReleasedAt(fluent,time+1).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:130
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',129).

 /*  not(releasedAt(Fluent, Time)), not(exists([Event],  (happens(Event, Time), releases(Event, Fluent, Time)))) ->
       not(releasedAt(Fluent, Time+1)).
 */

 /*  not(releasedAt(Fluent, Time+1)) :-
       not(releasedAt(Fluent, Time)),
       (   not(happens(Event, Time))
       ;   not(releases(Event, Fluent, Time))
       ).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',129).

 /*  [ignore(Time+1==Time3), b(Time, Time3), not(releasedAt(Fluent, Time)),  (not(happens(Event, Time));not(releases(Event, Fluent, Time)))] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time, Time3],
          not(releasedAt(Fluent, Time3))).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',129).
axiom(not(releasedAt(Fluent, Time3)),
   
    [ b(Time, Time3),
      not(releasedAt(Fluent, Time)),
       (not(happens(Event, Time));not(releases(Event, Fluent, Time)))
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',129).

 /*  releasedAt(Fluent, Time) :-
       (   not(happens(Event, Time))
       ;   not(releases(Event, Fluent, Time))
       ),
       releasedAt(Fluent, Time+1).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',129).

 /*  [ignore(Time+1==Time4), b(Time, Time4),  (not(happens(Event, Time));not(releases(Event, Fluent, Time))), releasedAt(Fluent, Time4)] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time, Time4],
          releasedAt(Fluent, Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',129).
axiom(releasedAt(Fluent, Time),
   
    [ b(Time, Time4),
       (not(happens(Event, Time));not(releases(Event, Fluent, Time))),
      releasedAt(Fluent, Time4)
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',129).

 /*  happens(Event, Time) :-
       not(releasedAt(Fluent, Time)),
       releasedAt(Fluent, Time+1).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',129).

 /*  [ignore(Time+1==Time5), b(Time, Time5), not(releasedAt(Fluent, Time)), releasedAt(Fluent, Time5)] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time, Time5],
          happens(Event, Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',129).
axiom(happens(Event, Time),
   
    [ b(Time, Time5),
      not(releasedAt(Fluent, Time)),
      releasedAt(Fluent, Time5)
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',129).

 /*  releases(Event, Fluent, Time) :-
       not(releasedAt(Fluent, Time)),
       releasedAt(Fluent, Time+1).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',129).

 /*  [ignore(Time+1==Time6), b(Time, Time6), not(releasedAt(Fluent, Time)), releasedAt(Fluent, Time6)] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time, Time6],
          releases(Event, Fluent, Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',129).
axiom(releases(Event, Fluent, Time),
   
    [ b(Time, Time6),
      not(releasedAt(Fluent, Time)),
      releasedAt(Fluent, Time6)
    ]).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:132
% [fluent,time]
% (ReleasedAt(fluent,time) &
%  !({event} Happens(event,time) &
%    (Initiates(event,fluent,time) |
%     Terminates(event,fluent,time)))) ->
% ReleasedAt(fluent,time+1).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:137
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',134).

 /*  releasedAt(Fluent, Time), not(exists([Event],  (happens(Event, Time), (initiates(Event, Fluent, Time);terminates(Event, Fluent, Time))))) ->
       releasedAt(Fluent, Time+1).
 */

 /*  releasedAt(Fluent, Time+1) :-
       releasedAt(Fluent, Time),
       (   not(happens(Event, Time))
       ;   not(initiates(Event, Fluent, Time)),
           not(terminates(Event, Fluent, Time))
       ).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',134).

 /*  [ignore(Time+1==Time3), b(Time, Time3), releasedAt(Fluent, Time),  (not(happens(Event, Time));not(initiates(Event, Fluent, Time)), not(terminates(Event, Fluent, Time)))] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time, Time3],
          releasedAt(Fluent, Time3)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',134).
axiom(releasedAt(Fluent, Time3),
   
    [ b(Time, Time3),
      releasedAt(Fluent, Time),
       (not(happens(Event, Time));not(initiates(Event, Fluent, Time)), not(terminates(Event, Fluent, Time)))
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',134).

 /*  not(releasedAt(Fluent, Time)) :-
       (   not(happens(Event, Time))
       ;   not(initiates(Event, Fluent, Time)),
           not(terminates(Event, Fluent, Time))
       ),
       not(releasedAt(Fluent, Time+1)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',134).

 /*  [ignore(Time+1==Time4), b(Time, Time4),  (not(happens(Event, Time));not(initiates(Event, Fluent, Time)), not(terminates(Event, Fluent, Time))), not(releasedAt(Fluent, Time4))] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time, Time4],
          not(releasedAt(Fluent, Time))).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',134).
axiom(not(releasedAt(Fluent, Time)),
   
    [ b(Time, Time4),
       (not(happens(Event, Time));not(initiates(Event, Fluent, Time)), not(terminates(Event, Fluent, Time))),
      not(releasedAt(Fluent, Time4))
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',134).

 /*  happens(Event, Time) :-
       releasedAt(Fluent, Time),
       not(releasedAt(Fluent, Time+1)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',134).

 /*  [ignore(Time+1==Time5), b(Time, Time5), releasedAt(Fluent, Time), not(releasedAt(Fluent, Time5))] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time, Time5],
          happens(Event, Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',134).
axiom(happens(Event, Time),
   
    [ b(Time, Time5),
      releasedAt(Fluent, Time),
      not(releasedAt(Fluent, Time5))
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',134).

 /*  initiates(Event, Fluent, Time) :-
       not(terminates(Event, Fluent, Time)),
       releasedAt(Fluent, Time),
       not(releasedAt(Fluent, Time+1)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',134).

 /*  [ignore(Time+1==Time6), b(Time, Time6), not(terminates(Event, Fluent, Time)), releasedAt(Fluent, Time), not(releasedAt(Fluent, Time6))] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time, Time6],
          initiates(Event, Fluent, Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',134).
axiom(initiates(Event, Fluent, Time),
   
    [ b(Time, Time6),
      not(terminates(Event, Fluent, Time)),
      releasedAt(Fluent, Time),
      not(releasedAt(Fluent, Time6))
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',134).

 /*  terminates(Event, Fluent, Time) :-
       not(initiates(Event, Fluent, Time)),
       releasedAt(Fluent, Time),
       not(releasedAt(Fluent, Time+1)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',134).

 /*  [ignore(Time+1==Time7), b(Time, Time7), not(initiates(Event, Fluent, Time)), releasedAt(Fluent, Time), not(releasedAt(Fluent, Time7))] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time, Time7],
          terminates(Event, Fluent, Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',134).
axiom(terminates(Event, Fluent, Time),
   
    [ b(Time, Time7),
      not(initiates(Event, Fluent, Time)),
      releasedAt(Fluent, Time),
      not(releasedAt(Fluent, Time7))
    ]).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:139
% [event,fluent,time]
% (Happens(event,time) & Initiates(event,fluent,time)) ->
% (HoldsAt(fluent,time+1) & !ReleasedAt(fluent,time+1)).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:141

 /*  happens(Event, Time), initiates(Event, Fluent, Time) ->
       holds_at(Fluent, Time+1),
       not(releasedAt(Fluent, Time+1)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',141).

 /*  not(happens(Event, Time)) :-
       initiates(Event, Fluent, Time),
       (   not(holds_at(Fluent, Time+1))
       ;   releasedAt(Fluent, Time+1)
       ).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',141).

 /*  [initiates(Event, Fluent, Time),  (holds_at(neg(Fluent), Time3);releasedAt(Fluent, Time+1)), b(Time, Time3), ignore(Time+1==Time3)] ->
       ta(Time,
          tvs1=[Time, Time+1],
          tvs2=[Time, Time3],
          not(happens(Event, Time))).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',141).
axiom(not(happens(Event, Time)),
   
    [ initiates(Event, Fluent, Time),
       (holds_at(neg(Fluent), Time3);releasedAt(Fluent, Time3)),
      b(Time, Time3)
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',141).

 /*  not(initiates(Event, Fluent, Time)) :-
       happens(Event, Time),
       (   not(holds_at(Fluent, Time+1))
       ;   releasedAt(Fluent, Time+1)
       ).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',141).

 /*  [happens(Event, Time),  (holds_at(neg(Fluent), Time4);releasedAt(Fluent, Time+1)), b(Time, Time4), ignore(Time+1==Time4)] ->
       ta(Time,
          tvs1=[Time, Time+1],
          tvs2=[Time, Time4],
          not(initiates(Event, Fluent, Time))).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',141).
axiom(not(initiates(Event, Fluent, Time)),
   
    [ happens(Event, Time),
       (holds_at(neg(Fluent), Time4);releasedAt(Fluent, Time4)),
      b(Time, Time4)
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',141).

 /*  holds_at(Fluent, Time+1) :-
       happens(Event, Time),
       initiates(Event, Fluent, Time).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',141).

 /*  [happens(Event, Time), initiates(Event, Fluent, Time), b(Time, Time5), ignore(Time+1==Time5)] ->
       ta(Time,
          tvs1=[Time, Time+1],
          tvs2=[Time, Time5],
          holds_at(Fluent, Time5)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',141).
axiom(holds_at(Fluent, Time5),
   
    [ happens(Event, Time),
      initiates(Event, Fluent, Time),
      b(Time, Time5)
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',141).

 /*  not(releasedAt(Fluent, Time+1)) :-
       happens(Event, Time),
       initiates(Event, Fluent, Time).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',141).

 /*  [ignore(Time+1==Time6), b(Time, Time6), happens(Event, Time), initiates(Event, Fluent, Time)] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time, Time6],
          not(releasedAt(Fluent, Time6))).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',141).
axiom(not(releasedAt(Fluent, Time6)),
   
    [ b(Time, Time6),
      happens(Event, Time),
      initiates(Event, Fluent, Time)
    ]).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:143
% [event,fluent,time]
% (Happens(event,time) & Terminates(event,fluent,time)) ->
% (!HoldsAt(fluent,time+1) & !ReleasedAt(fluent,time+1)).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:145

 /*  happens(Event, Time), terminates(Event, Fluent, Time) ->
       holds_at(neg(Fluent), Time+1),
       not(releasedAt(Fluent, Time+1)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',145).

 /*  not(happens(Event, Time)) :-
       terminates(Event, Fluent, Time),
       (   not(holds_at(neg(Fluent), Time+1))
       ;   releasedAt(Fluent, Time+1)
       ).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',145).

 /*  [terminates(Event, Fluent, Time),  (holds_at(Fluent, Time3);releasedAt(Fluent, Time+1)), b(Time, Time3), ignore(Time+1==Time3)] ->
       ta(Time,
          tvs1=[Time, Time+1],
          tvs2=[Time, Time3],
          not(happens(Event, Time))).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',145).
axiom(not(happens(Event, Time)),
   
    [ terminates(Event, Fluent, Time),
       (holds_at(Fluent, Time3);releasedAt(Fluent, Time3)),
      b(Time, Time3)
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',145).

 /*  not(terminates(Event, Fluent, Time)) :-
       happens(Event, Time),
       (   not(holds_at(neg(Fluent), Time+1))
       ;   releasedAt(Fluent, Time+1)
       ).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',145).

 /*  [happens(Event, Time),  (holds_at(Fluent, Time4);releasedAt(Fluent, Time+1)), b(Time, Time4), ignore(Time+1==Time4)] ->
       ta(Time,
          tvs1=[Time, Time+1],
          tvs2=[Time, Time4],
          not(terminates(Event, Fluent, Time))).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',145).
axiom(not(terminates(Event, Fluent, Time)),
   
    [ happens(Event, Time),
       (holds_at(Fluent, Time4);releasedAt(Fluent, Time4)),
      b(Time, Time4)
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',145).

 /*  holds_at(neg(Fluent), Time+1) :-
       happens(Event, Time),
       terminates(Event, Fluent, Time).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',145).

 /*  [happens(Event, Time), terminates(Event, Fluent, Time), b(Time, Time5), ignore(Time+1==Time5)] ->
       ta(Time,
          tvs1=[Time, Time+1],
          tvs2=[Time, Time5],
          holds_at(neg(Fluent), Time5)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',145).
axiom(holds_at(neg(Fluent), Time5),
   
    [ happens(Event, Time),
      terminates(Event, Fluent, Time),
      b(Time, Time5)
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',145).

 /*  not(releasedAt(Fluent, Time+1)) :-
       happens(Event, Time),
       terminates(Event, Fluent, Time).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',145).

 /*  [ignore(Time+1==Time6), b(Time, Time6), happens(Event, Time), terminates(Event, Fluent, Time)] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time, Time6],
          not(releasedAt(Fluent, Time6))).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',145).
axiom(not(releasedAt(Fluent, Time6)),
   
    [ b(Time, Time6),
      happens(Event, Time),
      terminates(Event, Fluent, Time)
    ]).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:147
% [event,fluent,time]
% (Happens(event,time) & Releases(event,fluent,time)) ->
% ReleasedAt(fluent,time+1).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:149

 /*  happens(Event, Time), releases(Event, Fluent, Time) ->
       releasedAt(Fluent, Time+1).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',149).

 /*  releasedAt(Fluent, Time+1) :-
       happens(Event, Time),
       releases(Event, Fluent, Time).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',149).

 /*  [ignore(Time+1==Time3), b(Time, Time3), happens(Event, Time), releases(Event, Fluent, Time)] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time, Time3],
          releasedAt(Fluent, Time3)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',149).
axiom(releasedAt(Fluent, Time3),
   
    [ b(Time, Time3),
      happens(Event, Time),
      releases(Event, Fluent, Time)
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',149).

 /*  not(happens(Event, Time)) :-
       releases(Event, Fluent, Time),
       not(releasedAt(Fluent, Time+1)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',149).

 /*  [ignore(Time+1==Time4), b(Time, Time4), releases(Event, Fluent, Time), not(releasedAt(Fluent, Time4))] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time, Time4],
          not(happens(Event, Time))).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',149).
axiom(not(happens(Event, Time)),
   
    [ b(Time, Time4),
      releases(Event, Fluent, Time),
      not(releasedAt(Fluent, Time4))
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',149).

 /*  not(releases(Event, Fluent, Time)) :-
       happens(Event, Time),
       not(releasedAt(Fluent, Time+1)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',149).

 /*  [ignore(Time+1==Time5), b(Time, Time5), happens(Event, Time), not(releasedAt(Fluent, Time5))] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time, Time5],
          not(releases(Event, Fluent, Time))).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',149).
axiom(not(releases(Event, Fluent, Time)),
   
    [ b(Time, Time5),
      happens(Event, Time),
      not(releasedAt(Fluent, Time5))
    ]).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:151
%; End of file.
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: foundations/ECCausal.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
%; Causal Constraints
%;
%; @inproceedings{Shanahan:1999a,
%;   author = "Murray Shanahan",
%;   year = "1999",
%;   title = "The ramification problem in the event calculus",
%;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}ixteenth \uppercase{I}nternational \uppercase{J}oint \uppercase{C}onference on \uppercase{A}rtificial \uppercase{I}ntelligence",
%;   pages = "140--146",
%;   address = "San Mateo, CA",
%;   publisher = "Morgan Kaufmann",
%; }
%;

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:182
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',183).
% predicate Started(fluent,time)
predicate(started(fluent,time)).
==> mpred_prop(started(fluent,time),predicate).
==> meta_argtypes(started(fluent,time)).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:183
% predicate Stopped(fluent,time)
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',183).
predicate(stopped(fluent,time)).
==> mpred_prop(stopped(fluent,time),predicate).
==> meta_argtypes(stopped(fluent,time)).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:185
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',186).
% [fluent,time]
% Started(fluent,time) <->
% (HoldsAt(fluent,time) |
%  ({event} Happens(event,time) & Initiates(event,fluent,time))).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:188
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',186).

 /*  started(Fluent, Time) <->
       (   holds_at(Fluent, Time)
       ;   exists([Event],
                   (happens(Event, Time), initiates(Event, Fluent, Time)))
       ).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',186).

 /*  [started] <->
       [;, holds_at, exists, happens, initiates].
 */

 /*  started(Fluent, Time) ->
       (   holds_at(Fluent, Time)
       ;   exists([Event],
                   (happens(Event, Time), initiates(Event, Fluent, Time)))
       ).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',186).

 /*  not(started(Fluent, Time)) :-
       not(holds_at(Fluent, Time)),
       (   not(happens(Event, Time))
       ;   not(initiates(Event, Fluent, Time))
       ).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',186).

 /*  [holds_at(neg(Fluent), Time),  (not(happens(Event, Time));not(initiates(Event, Fluent, Time)))] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          not(started(Fluent, Time))).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',186).
axiom(not(started(Fluent, Time)),
   
    [ holds_at(neg(Fluent), Time),
       (not(happens(Event, Time));not(initiates(Event, Fluent, Time)))
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',186).

 /*  holds_at(Fluent, Time) :-
       (   not(happens(Event, Time))
       ;   not(initiates(Event, Fluent, Time))
       ),
       started(Fluent, Time).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',186).

 /*  [(not(happens(Event, Time));not(initiates(Event, Fluent, Time))), started(Fluent, Time)] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          holds_at(Fluent, Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',186).
axiom(holds_at(Fluent, Time),
   
    [  (not(happens(Event, Time));not(initiates(Event, Fluent, Time))),
      started(Fluent, Time)
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',186).

 /*  happens(Event, Time) :-
       not(holds_at(Fluent, Time)),
       started(Fluent, Time).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',186).

 /*  [holds_at(neg(Fluent), Time), started(Fluent, Time)] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          happens(Event, Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',186).
axiom(happens(Event, Time),
    [holds_at(neg(Fluent), Time), started(Fluent, Time)]).

 /*  initiates(Event, Fluent, Time) :-
       not(holds_at(Fluent, Time)),
       started(Fluent, Time).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',186).

 /*  [holds_at(neg(Fluent), Time), started(Fluent, Time)] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          initiates(Event, Fluent, Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',186).
axiom(initiates(Event, Fluent, Time),
    [holds_at(neg(Fluent), Time), started(Fluent, Time)]).

 /*  holds_at(Fluent, Time);exists([Event],  (happens(Event, Time), initiates(Event, Fluent, Time))) ->
       started(Fluent, Time).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',186).

 /*  started(Fluent, Time) :-
       (   holds_at(Fluent, Time)
       ;   happens(Event, Time),
           initiates(Event, Fluent, Time)
       ).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',186).

 /*  [(holds_at(Fluent, Time);happens(Event, Time), initiates(Event, Fluent, Time))] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          started(Fluent, Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',186).
axiom(started(Fluent, Time),
   
    [  (holds_at(Fluent, Time);happens(Event, Time), initiates(Event, Fluent, Time))
    ]).

 /*  not(holds_at(Fluent, Time)) :-
       not(started(Fluent, Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',186).

 /*  [not(started(Fluent, Time))] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          holds_at(neg(Fluent), Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',186).
axiom(holds_at(neg(Fluent), Time),
    [not(started(Fluent, Time))]).

 /*  not(happens(Event, Time)) :-
       initiates(Event, Fluent, Time),
       not(started(Fluent, Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',186).

 /*  [initiates(Event, Fluent, Time), not(started(Fluent, Time))] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          not(happens(Event, Time))).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',186).
axiom(not(happens(Event, Time)),
   
    [ initiates(Event, Fluent, Time),
      not(started(Fluent, Time))
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',186).

 /*  not(initiates(Event, Fluent, Time)) :-
       happens(Event, Time),
       not(started(Fluent, Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',186).

 /*  [happens(Event, Time), not(started(Fluent, Time))] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          not(initiates(Event, Fluent, Time))).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',186).
axiom(not(initiates(Event, Fluent, Time)),
    [happens(Event, Time), not(started(Fluent, Time))]).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:190
% [fluent,time]
% Stopped(fluent,time) <->
% (!HoldsAt(fluent,time) |
%  ({event} Happens(event,time) & Terminates(event,fluent,time))).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:193
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',192).

 /*  stopped(Fluent, Time) <->
       (   holds_at(neg(Fluent), Time)
       ;   exists([Event],
                   (happens(Event, Time), terminates(Event, Fluent, Time)))
       ).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',192).

 /*  [stopped] <->
       [;, holds_at, neg, exists, happens, terminates].
 */

 /*  stopped(Fluent, Time) ->
       (   holds_at(neg(Fluent), Time)
       ;   exists([Event],
                   (happens(Event, Time), terminates(Event, Fluent, Time)))
       ).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',192).

 /*  not(stopped(Fluent, Time)) :-
       not(holds_at(neg(Fluent), Time)),
       (   not(happens(Event, Time))
       ;   not(terminates(Event, Fluent, Time))
       ).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',192).

 /*  [holds_at(Fluent, Time),  (not(happens(Event, Time));not(terminates(Event, Fluent, Time)))] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          not(stopped(Fluent, Time))).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',192).
axiom(not(stopped(Fluent, Time)),
   
    [ holds_at(Fluent, Time),
       (not(happens(Event, Time));not(terminates(Event, Fluent, Time)))
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',192).

 /*  holds_at(neg(Fluent), Time) :-
       (   not(happens(Event, Time))
       ;   not(terminates(Event, Fluent, Time))
       ),
       stopped(Fluent, Time).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',192).

 /*  [(not(happens(Event, Time));not(terminates(Event, Fluent, Time))), stopped(Fluent, Time)] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          holds_at(neg(Fluent), Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',192).
axiom(holds_at(neg(Fluent), Time),
   
    [  (not(happens(Event, Time));not(terminates(Event, Fluent, Time))),
      stopped(Fluent, Time)
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',192).

 /*  happens(Event, Time) :-
       not(holds_at(neg(Fluent), Time)),
       stopped(Fluent, Time).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',192).

 /*  [holds_at(Fluent, Time), stopped(Fluent, Time)] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          happens(Event, Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',192).
axiom(happens(Event, Time),
    [holds_at(Fluent, Time), stopped(Fluent, Time)]).

 /*  terminates(Event, Fluent, Time) :-
       not(holds_at(neg(Fluent), Time)),
       stopped(Fluent, Time).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',192).

 /*  [holds_at(Fluent, Time), stopped(Fluent, Time)] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          terminates(Event, Fluent, Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',192).
axiom(terminates(Event, Fluent, Time),
    [holds_at(Fluent, Time), stopped(Fluent, Time)]).

 /*  holds_at(neg(Fluent), Time);exists([Event],  (happens(Event, Time), terminates(Event, Fluent, Time))) ->
       stopped(Fluent, Time).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',192).

 /*  stopped(Fluent, Time) :-
       (   holds_at(neg(Fluent), Time)
       ;   happens(Event, Time),
           terminates(Event, Fluent, Time)
       ).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',192).

 /*  [(holds_at(neg(Fluent), Time);happens(Event, Time), terminates(Event, Fluent, Time))] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          stopped(Fluent, Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',192).
axiom(stopped(Fluent, Time),
   
    [  (holds_at(neg(Fluent), Time);happens(Event, Time), terminates(Event, Fluent, Time))
    ]).

 /*  not(holds_at(neg(Fluent), Time)) :-
       not(stopped(Fluent, Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',192).

 /*  [not(stopped(Fluent, Time))] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          holds_at(Fluent, Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',192).
axiom(holds_at(Fluent, Time),
    [not(stopped(Fluent, Time))]).

 /*  not(happens(Event, Time)) :-
       terminates(Event, Fluent, Time),
       not(stopped(Fluent, Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',192).

 /*  [terminates(Event, Fluent, Time), not(stopped(Fluent, Time))] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          not(happens(Event, Time))).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',192).
axiom(not(happens(Event, Time)),
   
    [ terminates(Event, Fluent, Time),
      not(stopped(Fluent, Time))
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',192).

 /*  not(terminates(Event, Fluent, Time)) :-
       happens(Event, Time),
       not(stopped(Fluent, Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',192).

 /*  [happens(Event, Time), not(stopped(Fluent, Time))] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          not(terminates(Event, Fluent, Time))).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',192).
axiom(not(terminates(Event, Fluent, Time)),
    [happens(Event, Time), not(stopped(Fluent, Time))]).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:195
% predicate Initiated(fluent,time)
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',195).
predicate(initiated(fluent,time)).
==> mpred_prop(initiated(fluent,time),predicate).
==> meta_argtypes(initiated(fluent,time)).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:196
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',197).
% predicate Terminated(fluent,time)
predicate(terminated(fluent,time)).
==> mpred_prop(terminated(fluent,time),predicate).
==> meta_argtypes(terminated(fluent,time)).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:198
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',199).
% [fluent,time]
% Initiated(fluent,time) <->
% (Started(fluent,time) &
%  !({event} Happens(event,time) & Terminates(event,fluent,time))).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:201
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',199).

 /*  initiated(Fluent, Time) <->
       started(Fluent, Time),
       not(exists([Event],
                   (happens(Event, Time), terminates(Event, Fluent, Time)))).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',199).

 /*  [initiated] <->
       [started, not, exists, happens, terminates].
 */

 /*  initiated(Fluent, Time) ->
       started(Fluent, Time),
       not(exists([Event],
                   (happens(Event, Time), terminates(Event, Fluent, Time)))).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',199).

 /*  not(initiated(Fluent, Time)) :-
       (   not(started(Fluent, Time))
       ;   happens(Event, Time),
           terminates(Event, Fluent, Time)
       ).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',199).

 /*  [(not(started(Fluent, Time));happens(Event, Time), terminates(Event, Fluent, Time))] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          not(initiated(Fluent, Time))).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',199).
axiom(not(initiated(Fluent, Time)),
   
    [  (not(started(Fluent, Time));happens(Event, Time), terminates(Event, Fluent, Time))
    ]).

 /*  started(Fluent, Time) :-
       initiated(Fluent, Time).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',199).

 /*  [initiated(Fluent, Time)] ->
       ta(Ta_Param, tvs1=[], tvs2=[], started(Fluent, Time)).
 */
axiom(started(Fluent, Time),
    [initiated(Fluent, Time)]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',199).

 /*  not(happens(Event, Time)) :-
       terminates(Event, Fluent, Time),
       initiated(Fluent, Time).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',199).

 /*  [terminates(Event, Fluent, Time), initiated(Fluent, Time)] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          not(happens(Event, Time))).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',199).
axiom(not(happens(Event, Time)),
   
    [ terminates(Event, Fluent, Time),
      initiated(Fluent, Time)
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',199).

 /*  not(terminates(Event, Fluent, Time)) :-
       happens(Event, Time),
       initiated(Fluent, Time).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',199).

 /*  [happens(Event, Time), initiated(Fluent, Time)] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          not(terminates(Event, Fluent, Time))).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',199).
axiom(not(terminates(Event, Fluent, Time)),
    [happens(Event, Time), initiated(Fluent, Time)]).

 /*  started(Fluent, Time), not(exists([Event],  (happens(Event, Time), terminates(Event, Fluent, Time)))) ->
       initiated(Fluent, Time).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',199).

 /*  initiated(Fluent, Time) :-
       started(Fluent, Time),
       (   not(happens(Event, Time))
       ;   not(terminates(Event, Fluent, Time))
       ).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',199).

 /*  [started(Fluent, Time),  (not(happens(Event, Time));not(terminates(Event, Fluent, Time)))] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          initiated(Fluent, Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',199).
axiom(initiated(Fluent, Time),
   
    [ started(Fluent, Time),
       (not(happens(Event, Time));not(terminates(Event, Fluent, Time)))
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',199).

 /*  not(started(Fluent, Time)) :-
       (   not(happens(Event, Time))
       ;   not(terminates(Event, Fluent, Time))
       ),
       not(initiated(Fluent, Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',199).

 /*  [(not(happens(Event, Time));not(terminates(Event, Fluent, Time))), not(initiated(Fluent, Time))] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          not(started(Fluent, Time))).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',199).
axiom(not(started(Fluent, Time)),
   
    [  (not(happens(Event, Time));not(terminates(Event, Fluent, Time))),
      not(initiated(Fluent, Time))
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',199).

 /*  happens(Event, Time) :-
       started(Fluent, Time),
       not(initiated(Fluent, Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',199).

 /*  [started(Fluent, Time), not(initiated(Fluent, Time))] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          happens(Event, Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',199).
axiom(happens(Event, Time),
    [started(Fluent, Time), not(initiated(Fluent, Time))]).

 /*  terminates(Event, Fluent, Time) :-
       started(Fluent, Time),
       not(initiated(Fluent, Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',199).

 /*  [started(Fluent, Time), not(initiated(Fluent, Time))] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          terminates(Event, Fluent, Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',199).
axiom(terminates(Event, Fluent, Time),
    [started(Fluent, Time), not(initiated(Fluent, Time))]).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:203
% [fluent,time]
% Terminated(fluent,time) <->
% (Stopped(fluent,time) &
%  !({event} Happens(event,time) & Initiates(event,fluent,time))).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:206
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',205).

 /*  terminated(Fluent, Time) <->
       stopped(Fluent, Time),
       not(exists([Event],
                   (happens(Event, Time), initiates(Event, Fluent, Time)))).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',205).

 /*  [terminated] <->
       [stopped, not, exists, happens, initiates].
 */

 /*  terminated(Fluent, Time) ->
       stopped(Fluent, Time),
       not(exists([Event],
                   (happens(Event, Time), initiates(Event, Fluent, Time)))).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',205).

 /*  not(terminated(Fluent, Time)) :-
       (   not(stopped(Fluent, Time))
       ;   happens(Event, Time),
           initiates(Event, Fluent, Time)
       ).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',205).

 /*  [(not(stopped(Fluent, Time));happens(Event, Time), initiates(Event, Fluent, Time))] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          not(terminated(Fluent, Time))).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',205).
axiom(not(terminated(Fluent, Time)),
   
    [  (not(stopped(Fluent, Time));happens(Event, Time), initiates(Event, Fluent, Time))
    ]).

 /*  stopped(Fluent, Time) :-
       terminated(Fluent, Time).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',205).

 /*  [terminated(Fluent, Time)] ->
       ta(Ta_Param, tvs1=[], tvs2=[], stopped(Fluent, Time)).
 */
axiom(stopped(Fluent, Time),
    [terminated(Fluent, Time)]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',205).

 /*  not(happens(Event, Time)) :-
       initiates(Event, Fluent, Time),
       terminated(Fluent, Time).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',205).

 /*  [initiates(Event, Fluent, Time), terminated(Fluent, Time)] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          not(happens(Event, Time))).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',205).
axiom(not(happens(Event, Time)),
   
    [ initiates(Event, Fluent, Time),
      terminated(Fluent, Time)
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',205).

 /*  not(initiates(Event, Fluent, Time)) :-
       happens(Event, Time),
       terminated(Fluent, Time).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',205).

 /*  [happens(Event, Time), terminated(Fluent, Time)] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          not(initiates(Event, Fluent, Time))).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',205).
axiom(not(initiates(Event, Fluent, Time)),
    [happens(Event, Time), terminated(Fluent, Time)]).

 /*  stopped(Fluent, Time), not(exists([Event],  (happens(Event, Time), initiates(Event, Fluent, Time)))) ->
       terminated(Fluent, Time).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',205).

 /*  terminated(Fluent, Time) :-
       stopped(Fluent, Time),
       (   not(happens(Event, Time))
       ;   not(initiates(Event, Fluent, Time))
       ).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',205).

 /*  [stopped(Fluent, Time),  (not(happens(Event, Time));not(initiates(Event, Fluent, Time)))] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          terminated(Fluent, Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',205).
axiom(terminated(Fluent, Time),
   
    [ stopped(Fluent, Time),
       (not(happens(Event, Time));not(initiates(Event, Fluent, Time)))
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',205).

 /*  not(stopped(Fluent, Time)) :-
       (   not(happens(Event, Time))
       ;   not(initiates(Event, Fluent, Time))
       ),
       not(terminated(Fluent, Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',205).

 /*  [(not(happens(Event, Time));not(initiates(Event, Fluent, Time))), not(terminated(Fluent, Time))] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          not(stopped(Fluent, Time))).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',205).
axiom(not(stopped(Fluent, Time)),
   
    [  (not(happens(Event, Time));not(initiates(Event, Fluent, Time))),
      not(terminated(Fluent, Time))
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',205).

 /*  happens(Event, Time) :-
       stopped(Fluent, Time),
       not(terminated(Fluent, Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',205).

 /*  [stopped(Fluent, Time), not(terminated(Fluent, Time))] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          happens(Event, Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',205).
axiom(happens(Event, Time),
   
    [ stopped(Fluent, Time),
      not(terminated(Fluent, Time))
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',205).

 /*  initiates(Event, Fluent, Time) :-
       stopped(Fluent, Time),
       not(terminated(Fluent, Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',205).

 /*  [stopped(Fluent, Time), not(terminated(Fluent, Time))] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          initiates(Event, Fluent, Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',205).
axiom(initiates(Event, Fluent, Time),
   
    [ stopped(Fluent, Time),
      not(terminated(Fluent, Time))
    ]).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:208
%; End of file.
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: foundations/ECTraj.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
%; @incollection{MillerShanahan:2002,
%;   author = "Rob Miller and Murray Shanahan",
%;   year = "2002",
%;   title = "Some alternative formulations of the event calculus",
%;   editor = "Antonis C. Kakas and Fariba Sadri",
%;   booktitle = "Computational Logic: Logic Programming and Beyond: Essays in Honour of \uppercase{R}obert \uppercase{A}. \uppercase{K}owalski, Part \uppercase{II}",
%;   series = "Lecture Notes in Computer Science",
%;   volume = "2408",
%;   pages = "452--490",
%;   address = "Berlin",
%;   publisher = "Springer",
%; }
%;

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:240
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',241).
% predicate Clipped(time,fluent,time)
predicate(clipped(time,fluent,time)).
==> mpred_prop(clipped(time,fluent,time),predicate).
==> meta_argtypes(clipped(time,fluent,time)).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:241
% predicate Declipped(time,fluent,time)
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',241).
predicate(declipped(time,fluent,time)).
==> mpred_prop(declipped(time,fluent,time),predicate).
==> meta_argtypes(declipped(time,fluent,time)).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:243
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',244).
% predicate Trajectory(fluent,time,fluent,offset)
predicate(trajectory(fluent,time,fluent,offset)).
==> mpred_prop(trajectory(fluent,time,fluent,offset),predicate).
==> meta_argtypes(trajectory(fluent,time,fluent,offset)).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:244
% predicate AntiTrajectory(fluent,time,fluent,offset)
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',244).
predicate(antiTrajectory(fluent,time,fluent,offset)).
==> mpred_prop(antiTrajectory(fluent,time,fluent,offset),predicate).
==> meta_argtypes(antiTrajectory(fluent,time,fluent,offset)).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:246
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',247).
% [event,fluent,fluent2,offset,time]
% Happens(event,time) &
% Initiates(event,fluent,time) &
% 0 < offset &
% Trajectory(fluent,time,fluent2,offset) &
% !Clipped(time,fluent,time+offset) ->
% HoldsAt(fluent2,time+offset).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:252
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',247).

 /*  happens(Event, Time), initiates(Event, Fluent, Time), 0<Offset, trajectory(Fluent, Time, Fluent2, Offset), not(clipped(Time, Fluent, Time+Offset)) ->
       holds_at(Fluent2, Time+Offset).
 */

 /*  holds_at(Fluent2, Time+Offset) :-
       happens(Event, Time),
       initiates(Event, Fluent, Time),
       comparison(0, Offset, <),
       trajectory(Fluent, Time, Fluent2, Offset),
       not(clipped(Time, Fluent, Time+Offset)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',247).

 /*  [happens(Event, Time), initiates(Event, Fluent, Time), comparison(0, Offset, <), trajectory(Fluent, Time, Fluent2, Offset), not(clipped(Time, Fluent, Time5)), b(Time, Time5), toffset(Time, Offset, Time5), ignore(Time+Offset==Time5)] ->
       ta(Time,
          tvs1=[Time, Time+Offset],
          tvs2=[Time, Time5],
          holds_at(Fluent2, Time5)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',247).
axiom(holds_at(Fluent2, Time5),
   
    [ happens(Event, Time),
      initiates(Event, Fluent, Time),
      comparison(0, Offset, <),
      trajectory(Fluent, Time, Fluent2, Offset),
      not(clipped(Time, Fluent, Time5)),
      b(Time, Time5),
      toffset(Time, Offset, Time5)
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',247).

 /*  not(happens(Event, Time)) :-
       ( initiates(Event, Fluent, Time),
         comparison(0, Offset, <),
         trajectory(Fluent, Time, Fluent2, Offset),
         not(clipped(Time, Fluent, Time+Offset))
       ),
       not(holds_at(Fluent2, Time+Offset)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',247).

 /*  [initiates(Event, Fluent, Time), comparison(0, Offset, <), trajectory(Fluent, Time, Fluent2, Offset), not(clipped(Time, Fluent, Time6)), holds_at(neg(Fluent2), Time6), b(Time, Time6), toffset(Time, Offset, Time6), ignore(Time+Offset==Time6)] ->
       ta(Time,
          tvs1=[Time, Time+Offset],
          tvs2=[Time, Time6],
          not(happens(Event, Time))).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',247).
axiom(not(happens(Event, Time)),
   
    [ initiates(Event, Fluent, Time),
      comparison(0, Offset, <),
      trajectory(Fluent, Time, Fluent2, Offset),
      not(clipped(Time, Fluent, Time6)),
      holds_at(neg(Fluent2), Time6),
      b(Time, Time6),
      toffset(Time, Offset, Time6)
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',247).

 /*  not(initiates(Event, Fluent, Time)) :-
       ( comparison(0, Offset, <),
         trajectory(Fluent, Time, Fluent2, Offset),
         not(clipped(Time, Fluent, Time+Offset))
       ),
       happens(Event, Time),
       not(holds_at(Fluent2, Time+Offset)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',247).

 /*  [comparison(0, Offset, <), trajectory(Fluent, Time, Fluent2, Offset), not(clipped(Time, Fluent, Time7)), happens(Event, Time), holds_at(neg(Fluent2), Time7), b(Time, Time7), toffset(Time, Offset, Time7), ignore(Time+Offset==Time7)] ->
       ta(Time,
          tvs1=[Time, Time+Offset],
          tvs2=[Time, Time7],
          not(initiates(Event, Fluent, Time))).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',247).
axiom(not(initiates(Event, Fluent, Time)),
   
    [ comparison(0, Offset, <),
      trajectory(Fluent, Time, Fluent2, Offset),
      not(clipped(Time, Fluent, Time7)),
      happens(Event, Time),
      holds_at(neg(Fluent2), Time7),
      b(Time, Time7),
      toffset(Time, Offset, Time7)
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',247).

 /*  not(trajectory(Fluent, Time, Fluent2, Offset)) :-
       not(clipped(Time, Fluent, Time+Offset)),
       comparison(0, Offset, <),
       initiates(Event, Fluent, Time),
       happens(Event, Time),
       not(holds_at(Fluent2, Time+Offset)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',247).

 /*  [not(clipped(Time, Fluent, Time8)), comparison(0, Offset, <), initiates(Event, Fluent, Time), happens(Event, Time), holds_at(neg(Fluent2), Time8), b(Time, Time8), toffset(Time, Offset, Time8), ignore(Time+Offset==Time8)] ->
       ta(Time,
          tvs1=[Time, Time+Offset],
          tvs2=[Time, Time8],
          not(trajectory(Fluent, Time, Fluent2, Offset))).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',247).
axiom(not(trajectory(Fluent, Time, Fluent2, Offset)),
   
    [ not(clipped(Time, Fluent, Time8)),
      comparison(0, Offset, <),
      initiates(Event, Fluent, Time),
      happens(Event, Time),
      holds_at(neg(Fluent2), Time8),
      b(Time, Time8),
      toffset(Time, Offset, Time8)
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',247).

 /*  clipped(Time, Fluent, Time+Offset) :-
       trajectory(Fluent, Time, Fluent2, Offset),
       comparison(0, Offset, <),
       initiates(Event, Fluent, Time),
       happens(Event, Time),
       not(holds_at(Fluent2, Time+Offset)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',247).

 /*  [trajectory(Fluent, Time, Fluent2, Offset), comparison(0, Offset, <), initiates(Event, Fluent, Time), happens(Event, Time), holds_at(neg(Fluent2), Time9), b(Time, Time9), toffset(Time, Offset, Time9), ignore(Time+Offset==Time9)] ->
       ta(Time,
          tvs1=[Time, Time+Offset],
          tvs2=[Time, Time9],
          clipped(Time, Fluent, Time9)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',247).
axiom(clipped(Time, Fluent, Time9),
   
    [ trajectory(Fluent, Time, Fluent2, Offset),
      comparison(0, Offset, <),
      initiates(Event, Fluent, Time),
      happens(Event, Time),
      holds_at(neg(Fluent2), Time9),
      b(Time, Time9),
      toffset(Time, Offset, Time9)
    ]).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:254
% [event,fluent,fluent2,offset,time]
% Happens(event,time) &
% Terminates(event,fluent,time) &
% 0 < offset &
% AntiTrajectory(fluent,time,fluent2,offset) &
% !Declipped(time,fluent,time+offset) ->
% HoldsAt(fluent2,time+offset).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:260
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',256).

 /*  happens(Event, Time), terminates(Event, Fluent, Time), 0<Offset, antiTrajectory(Fluent, Time, Fluent2, Offset), not(declipped(Time, Fluent, Time+Offset)) ->
       holds_at(Fluent2, Time+Offset).
 */

 /*  holds_at(Fluent2, Time+Offset) :-
       happens(Event, Time),
       terminates(Event, Fluent, Time),
       comparison(0, Offset, <),
       antiTrajectory(Fluent, Time, Fluent2, Offset),
       not(declipped(Time, Fluent, Time+Offset)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',256).

 /*  [happens(Event, Time), terminates(Event, Fluent, Time), comparison(0, Offset, <), antiTrajectory(Fluent, Time, Fluent2, Offset), not(declipped(Time, Fluent, Time5)), b(Time, Time5), toffset(Time, Offset, Time5), ignore(Time+Offset==Time5)] ->
       ta(Time,
          tvs1=[Time, Time+Offset],
          tvs2=[Time, Time5],
          holds_at(Fluent2, Time5)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',256).
axiom(holds_at(Fluent2, Time5),
   
    [ happens(Event, Time),
      terminates(Event, Fluent, Time),
      comparison(0, Offset, <),
      antiTrajectory(Fluent, Time, Fluent2, Offset),
      not(declipped(Time, Fluent, Time5)),
      b(Time, Time5),
      toffset(Time, Offset, Time5)
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',256).

 /*  not(happens(Event, Time)) :-
       ( terminates(Event, Fluent, Time),
         comparison(0, Offset, <),
         antiTrajectory(Fluent, Time, Fluent2, Offset),
         not(declipped(Time, Fluent, Time+Offset))
       ),
       not(holds_at(Fluent2, Time+Offset)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',256).

 /*  [terminates(Event, Fluent, Time), comparison(0, Offset, <), antiTrajectory(Fluent, Time, Fluent2, Offset), not(declipped(Time, Fluent, Time6)), holds_at(neg(Fluent2), Time6), b(Time, Time6), toffset(Time, Offset, Time6), ignore(Time+Offset==Time6)] ->
       ta(Time,
          tvs1=[Time, Time+Offset],
          tvs2=[Time, Time6],
          not(happens(Event, Time))).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',256).
axiom(not(happens(Event, Time)),
   
    [ terminates(Event, Fluent, Time),
      comparison(0, Offset, <),
      antiTrajectory(Fluent, Time, Fluent2, Offset),
      not(declipped(Time, Fluent, Time6)),
      holds_at(neg(Fluent2), Time6),
      b(Time, Time6),
      toffset(Time, Offset, Time6)
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',256).

 /*  not(terminates(Event, Fluent, Time)) :-
       ( comparison(0, Offset, <),
         antiTrajectory(Fluent, Time, Fluent2, Offset),
         not(declipped(Time, Fluent, Time+Offset))
       ),
       happens(Event, Time),
       not(holds_at(Fluent2, Time+Offset)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',256).

 /*  [comparison(0, Offset, <), antiTrajectory(Fluent, Time, Fluent2, Offset), not(declipped(Time, Fluent, Time7)), happens(Event, Time), holds_at(neg(Fluent2), Time7), b(Time, Time7), toffset(Time, Offset, Time7), ignore(Time+Offset==Time7)] ->
       ta(Time,
          tvs1=[Time, Time+Offset],
          tvs2=[Time, Time7],
          not(terminates(Event, Fluent, Time))).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',256).
axiom(not(terminates(Event, Fluent, Time)),
   
    [ comparison(0, Offset, <),
      antiTrajectory(Fluent, Time, Fluent2, Offset),
      not(declipped(Time, Fluent, Time7)),
      happens(Event, Time),
      holds_at(neg(Fluent2), Time7),
      b(Time, Time7),
      toffset(Time, Offset, Time7)
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',256).

 /*  not(antiTrajectory(Fluent, Time, Fluent2, Offset)) :-
       not(declipped(Time, Fluent, Time+Offset)),
       comparison(0, Offset, <),
       terminates(Event, Fluent, Time),
       happens(Event, Time),
       not(holds_at(Fluent2, Time+Offset)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',256).

 /*  [not(declipped(Time, Fluent, Time8)), comparison(0, Offset, <), terminates(Event, Fluent, Time), happens(Event, Time), holds_at(neg(Fluent2), Time8), b(Time, Time8), toffset(Time, Offset, Time8), ignore(Time+Offset==Time8)] ->
       ta(Time,
          tvs1=[Time, Time+Offset],
          tvs2=[Time, Time8],
          not(antiTrajectory(Fluent, Time, Fluent2, Offset))).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',256).
axiom(not(antiTrajectory(Fluent, Time, Fluent2, Offset)),
   
    [ not(declipped(Time, Fluent, Time8)),
      comparison(0, Offset, <),
      terminates(Event, Fluent, Time),
      happens(Event, Time),
      holds_at(neg(Fluent2), Time8),
      b(Time, Time8),
      toffset(Time, Offset, Time8)
    ]).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',256).

 /*  declipped(Time, Fluent, Time+Offset) :-
       antiTrajectory(Fluent, Time, Fluent2, Offset),
       comparison(0, Offset, <),
       terminates(Event, Fluent, Time),
       happens(Event, Time),
       not(holds_at(Fluent2, Time+Offset)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',256).

 /*  [antiTrajectory(Fluent, Time, Fluent2, Offset), comparison(0, Offset, <), terminates(Event, Fluent, Time), happens(Event, Time), holds_at(neg(Fluent2), Time9), b(Time, Time9), toffset(Time, Offset, Time9), ignore(Time+Offset==Time9)] ->
       ta(Time,
          tvs1=[Time, Time+Offset],
          tvs2=[Time, Time9],
          declipped(Time, Fluent, Time9)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e',256).
axiom(declipped(Time, Fluent, Time9),
   
    [ antiTrajectory(Fluent, Time, Fluent2, Offset),
      comparison(0, Offset, <),
      terminates(Event, Fluent, Time),
      happens(Event, Time),
      holds_at(neg(Fluent2), Time9),
      b(Time, Time9),
      toffset(Time, Offset, Time9)
    ]).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/ec_reader_test_foundations.e:262
%; End of file.
