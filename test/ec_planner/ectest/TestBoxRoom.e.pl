:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(pfc).
% Sat, 04 Apr 2020 13:28:23 GMT
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/test_np_box_agent.e.pl:157
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ec_loader.pl',1433).

 /*  loading(load_e_pl,
   	'/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e').
 */
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: examples/Mueller2006/Chapter10/MovingNewspaperAndBox.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available in
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; @book{Mueller:2006,
%;   author = "Erik T. Mueller",
%;   year = "2006",
%;   title = "Commonsense Reasoning",
%;   address = "San Francisco",
%;   publisher = "Morgan Kaufmann/Elsevier",
%; }
%;

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:23
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',24).
% include foundations/Root.e
:- load_e('foundations/Root.e', include).
:- if(is_e_toplevel).

 /*  loading(include,
   	'/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/foundations/Root.e').
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

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/foundations/Root.e:10
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/foundations/Root.e',11).
% sort boolean
==> sort(boolean).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/foundations/Root.e:11
% sort integer
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/foundations/Root.e',11).
==> sort(integer).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/foundations/Root.e:12
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/foundations/Root.e',13).
% reified sort predicate
reified_sort(predicate).
==> mpred_prop(predicate,reified_sort).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/foundations/Root.e:13
% reified sort function
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/foundations/Root.e',13).
reified_sort(function).
==> mpred_prop(function,reified_sort).
%; End of file.
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/foundations/Root.e',16).
:- endif.

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:24
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',25).
% include foundations/EC.e
:- load_e('foundations/EC.e', include).
:- if(is_e_toplevel).

 /*  loading(include,
   	'/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/foundations/EC.e').
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

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/foundations/EC.e:26
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/foundations/EC.e',27).
% sort time: integer
==> subsort(time,integer).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/foundations/EC.e:27
% sort offset: integer
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/foundations/EC.e',27).
==> subsort(offset,integer).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/foundations/EC.e:29
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/foundations/EC.e',30).
% reified sort fluent
reified_sort(fluent).
==> mpred_prop(fluent,reified_sort).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/foundations/EC.e:30
% reified sort event
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/foundations/EC.e',30).
reified_sort(event).
==> mpred_prop(event,reified_sort).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/foundations/EC.e:32
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/foundations/EC.e',33).
% predicate Happens(event,time)
predicate(happens(event,time)).
==> mpred_prop(happens(event,time),predicate).
==> meta_argtypes(happens(event,time)).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/foundations/EC.e:33
% predicate HoldsAt(fluent,time)
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/foundations/EC.e',33).
predicate(holds_at(fluent,time)).
==> mpred_prop(holds_at(fluent,time),predicate).
==> meta_argtypes(holds_at(fluent,time)).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/foundations/EC.e:34
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/foundations/EC.e',35).
% predicate ReleasedAt(fluent,time)
predicate(releasedAt(fluent,time)).
==> mpred_prop(releasedAt(fluent,time),predicate).
==> meta_argtypes(releasedAt(fluent,time)).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/foundations/EC.e:35
% predicate Initiates(event,fluent,time)
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/foundations/EC.e',35).
predicate(initiates(event,fluent,time)).
==> mpred_prop(initiates(event,fluent,time),predicate).
==> meta_argtypes(initiates(event,fluent,time)).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/foundations/EC.e:36
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/foundations/EC.e',37).
% predicate Terminates(event,fluent,time)
predicate(terminates(event,fluent,time)).
==> mpred_prop(terminates(event,fluent,time),predicate).
==> meta_argtypes(terminates(event,fluent,time)).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/foundations/EC.e:37
% predicate Releases(event,fluent,time)
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/foundations/EC.e',37).
predicate(releases(event,fluent,time)).
==> mpred_prop(releases(event,fluent,time),predicate).
==> meta_argtypes(releases(event,fluent,time)).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/foundations/EC.e:38
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/foundations/EC.e',39).
% predicate Trajectory(fluent,time,fluent,offset)
predicate(trajectory(fluent,time,fluent,offset)).
==> mpred_prop(trajectory(fluent,time,fluent,offset),predicate).
==> meta_argtypes(trajectory(fluent,time,fluent,offset)).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/foundations/EC.e:40
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/foundations/EC.e',41).
%; End of file.
:- endif.

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:26
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',27).
% sort object
==> sort(object).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:27
% sort agent: object
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',27).
==> subsort(agent,object).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:28
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',29).
% sort physobj: object
==> subsort(physobj,object).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:29
% sort room: object
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',29).
==> subsort(room,object).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:31
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',32).
% fluent directlyIn(object,object)
fluent(directlyIn(object,object)).
==> mpred_prop(directlyIn(object,object),fluent).
==> meta_argtypes(directlyIn(object,object)).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:32
% fluent inRoom(object,room)
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',32).
fluent(inRoom(object,room)).
==> mpred_prop(inRoom(object,room),fluent).
==> meta_argtypes(inRoom(object,room)).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:33
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',34).
% noninertial inRoom
==> noninertial(inRoom).
%;; executable(move(agent,object,object,object))

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:37
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',38).
% agent Lisa
==> t(agent,lisa).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:38
% physobj Box, Newspaper
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',38).
==> t(physobj,box).
==> t(physobj,newspaper).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:39
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',40).
% room Kitchen, LivingRoom
==> t(room,kitchen).
==> t(room,livingRoom).
%; Sigma
%; RS10
% [agent,physobj1,physobj2,room,time]
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:45
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',46).
% HoldsAt(directlyIn(agent,room),time) &
% HoldsAt(directlyIn(physobj1,room),time) &
% HoldsAt(inRoom(physobj2,room),time) ->
% Initiates(move(agent,physobj1,room,physobj2),directlyIn(physobj1,physobj2),time).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:48
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',46).

 /*  [holds_at(directlyIn(Agent, Room), Time), holds_at(directlyIn(Physobj1, Room), Time), holds_at(inRoom(Physobj2, Room), Time)] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          initiates(move(Agent, Physobj1, Room, Physobj2),
                    directlyIn(Physobj1, Physobj2),
                    Time)).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:48
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',46).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:48
axiom(initiates(move(Agent, Physobj1, Room, Physobj2), directlyIn(Physobj1, Physobj2), Time),
   
    [ holds_at(directlyIn(Agent, Room), Time),
      holds_at(directlyIn(Physobj1, Room), Time),
      holds_at(inRoom(Physobj2, Room), Time)
    ]).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:50
%; RS11
% [agent,physobj1,physobj2,room,time]
% HoldsAt(directlyIn(agent,room),time) &
% HoldsAt(directlyIn(physobj1,room),time) &
% HoldsAt(inRoom(physobj2,room),time) ->
% Terminates(move(agent,physobj1,room,physobj2),directlyIn(physobj1,room),time).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:55
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',52).

 /*  [holds_at(directlyIn(Agent, Room), Time), holds_at(directlyIn(Physobj1, Room), Time), holds_at(inRoom(Physobj2, Room), Time)] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          terminates(move(Agent, Physobj1, Room, Physobj2),
                     directlyIn(Physobj1, Room),
                     Time)).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:55
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',52).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:55
axiom(terminates(move(Agent, Physobj1, Room, Physobj2), directlyIn(Physobj1, Room), Time),
   
    [ holds_at(directlyIn(Agent, Room), Time),
      holds_at(directlyIn(Physobj1, Room), Time),
      holds_at(inRoom(Physobj2, Room), Time)
    ]).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:57
%; RS12
% [agent,physobj1,physobj2,room,time]
% HoldsAt(directlyIn(agent,room),time) ->
% Initiates(move(agent,physobj1,physobj2,room),directlyIn(physobj1,room),time).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:60
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',59).

 /*  [holds_at(directlyIn(Agent, Room), Time)] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          initiates(move(Agent, Physobj1, Physobj2, Room),
                    directlyIn(Physobj1, Room),
                    Time)).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:60
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',59).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:60
axiom(initiates(move(Agent, Physobj1, Physobj2, Room), directlyIn(Physobj1, Room), Time),
    [holds_at(directlyIn(Agent, Room), Time)]).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:62
%; RS13
% [agent,physobj1,physobj2,room,time]
% HoldsAt(directlyIn(agent,room),time) ->
% Terminates(move(agent,physobj1,physobj2,room),directlyIn(physobj1,physobj2),time).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:65
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',64).

 /*  [holds_at(directlyIn(Agent, Room), Time)] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          terminates(move(Agent, Physobj1, Physobj2, Room),
                     directlyIn(Physobj1, Physobj2),
                     Time)).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:65
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',64).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:65
axiom(terminates(move(Agent, Physobj1, Physobj2, Room), directlyIn(Physobj1, Physobj2), Time),
    [holds_at(directlyIn(Agent, Room), Time)]).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:67
%; RS14
% [agent,room1,room2,time]
% HoldsAt(directlyIn(agent,room1),time) ->
% Initiates(move(agent,agent,room1,room2),directlyIn(agent,room2),time).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:70
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',69).

 /*  [holds_at(directlyIn(Agent, Room1), Time)] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          initiates(move(Agent, Agent, Room1, Room2),
                    directlyIn(Agent, Room2),
                    Time)).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:70
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',69).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:70
axiom(initiates(move(Agent, Agent, Room1, Room2), directlyIn(Agent, Room2), Time),
    [holds_at(directlyIn(Agent, Room1), Time)]).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:72
%; RS15
% [agent,room1,room2,time]
% HoldsAt(directlyIn(agent,room1),time) ->
% Terminates(move(agent,agent,room1,room2),directlyIn(agent,room1),time).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:75
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',74).

 /*  [holds_at(directlyIn(Agent, Room1), Time)] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          terminates(move(Agent, Agent, Room1, Room2),
                     directlyIn(Agent, Room1),
                     Time)).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:75
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',74).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:75
axiom(terminates(move(Agent, Agent, Room1, Room2), directlyIn(Agent, Room1), Time),
    [holds_at(directlyIn(Agent, Room1), Time)]).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:77
%; RS16
% [agent,physobj,room,time]
% HoldsAt(directlyIn(agent,room),time) &
% HoldsAt(directlyIn(physobj,room),time) ->
% Initiates(move(agent,physobj,room,agent),directlyIn(physobj,agent),time).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:81
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',79).

 /*  [holds_at(directlyIn(Agent, Room), Time), holds_at(directlyIn(Physobj, Room), Time)] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          initiates(move(Agent, Physobj, Room, Agent),
                    directlyIn(Physobj, Agent),
                    Time)).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:81
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',79).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:81
axiom(initiates(move(Agent, Physobj, Room, Agent), directlyIn(Physobj, Agent), Time),
   
    [ holds_at(directlyIn(Agent, Room), Time),
      holds_at(directlyIn(Physobj, Room), Time)
    ]).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:83
%; RS17
% [agent,physobj,room,time]
% HoldsAt(directlyIn(agent,room),time) &
% HoldsAt(directlyIn(physobj,room),time) ->
% Terminates(move(agent,physobj,room,agent),directlyIn(physobj,room),time).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:87
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',85).

 /*  [holds_at(directlyIn(Agent, Room), Time), holds_at(directlyIn(Physobj, Room), Time)] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          terminates(move(Agent, Physobj, Room, Agent),
                     directlyIn(Physobj, Room),
                     Time)).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:87
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',85).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:87
axiom(terminates(move(Agent, Physobj, Room, Agent), directlyIn(Physobj, Room), Time),
   
    [ holds_at(directlyIn(Agent, Room), Time),
      holds_at(directlyIn(Physobj, Room), Time)
    ]).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:89
%; RS18
% [agent,physobj,room,time]
% HoldsAt(directlyIn(physobj,agent),time) &
% HoldsAt(directlyIn(agent,room),time) ->
% Initiates(move(agent,physobj,agent,room),directlyIn(physobj,room),time).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:93
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',91).

 /*  [holds_at(directlyIn(Physobj, Agent), Time), holds_at(directlyIn(Agent, Room), Time)] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          initiates(move(Agent, Physobj, Agent, Room),
                    directlyIn(Physobj, Room),
                    Time)).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:93
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',91).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:93
axiom(initiates(move(Agent, Physobj, Agent, Room), directlyIn(Physobj, Room), Time),
   
    [ holds_at(directlyIn(Physobj, Agent), Time),
      holds_at(directlyIn(Agent, Room), Time)
    ]).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:95
%; RS19
% [agent,physobj,room,time]
% HoldsAt(directlyIn(physobj,agent),time) &
% HoldsAt(directlyIn(agent,room),time) ->
% Terminates(move(agent,physobj,agent,room),directlyIn(physobj,agent),time).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:99
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',97).

 /*  [holds_at(directlyIn(Physobj, Agent), Time), holds_at(directlyIn(Agent, Room), Time)] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          terminates(move(Agent, Physobj, Agent, Room),
                     directlyIn(Physobj, Agent),
                     Time)).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:99
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',97).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:99
axiom(terminates(move(Agent, Physobj, Agent, Room), directlyIn(Physobj, Agent), Time),
   
    [ holds_at(directlyIn(Physobj, Agent), Time),
      holds_at(directlyIn(Agent, Room), Time)
    ]).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:101
%; Delta


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:103
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',104).
% Happens(move(Lisa,Newspaper,LivingRoom,Box),0).

 /*  [] ->
       ta(Ta_Param,
          tvs1=[start],
          tvs2=[start],
          happens(move(lisa, newspaper, livingRoom, box), start)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',104).
axiom(happens(move(lisa, newspaper, livingRoom, box), start),
    []).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:104
% Happens(move(Lisa,Box,LivingRoom,Lisa),1).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',104).

 /*  [b(start, Maptime), ignore(start+1==Maptime)] ->
       ta(Ta_Param,
          tvs1=[start+1],
          tvs2=[Maptime, start],
          happens(move(lisa, box, livingRoom, lisa), Maptime)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',104).
axiom(happens(move(lisa, box, livingRoom, lisa), Maptime),
    [b(start, Maptime)]).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:105
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',106).
% Happens(move(Lisa,Lisa,LivingRoom,Kitchen),2).

 /*  [b(start, Start2), toffset(start, 2, Start2), ignore(start+2==Start2)] ->
       ta(Ta_Param,
          tvs1=[start+2],
          tvs2=[Start2, start],
          happens(move(lisa, lisa, livingRoom, kitchen), Start2)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',106).
axiom(happens(move(lisa, lisa, livingRoom, kitchen), Start2),
    [b(start, Start2), toffset(start, 2, Start2)]).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:106
% Happens(move(Lisa,Box,Lisa,Kitchen),3).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',106).

 /*  [b(start, Start3), toffset(start, 3, Start3), ignore(start+3==Start3)] ->
       ta(Ta_Param,
          tvs1=[start+3],
          tvs2=[Start3, start],
          happens(move(lisa, box, lisa, kitchen), Start3)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',106).
axiom(happens(move(lisa, box, lisa, kitchen), Start3),
    [b(start, Start3), toffset(start, 3, Start3)]).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:107
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',108).
% Happens(move(Lisa,Lisa,Kitchen,LivingRoom),4).

 /*  [b(start, Start4), toffset(start, 4, Start4), ignore(start+4==Start4)] ->
       ta(Ta_Param,
          tvs1=[start+4],
          tvs2=[Start4, start],
          happens(move(lisa, lisa, kitchen, livingRoom), Start4)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',108).
axiom(happens(move(lisa, lisa, kitchen, livingRoom), Start4),
    [b(start, Start4), toffset(start, 4, Start4)]).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:109
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',110).
%; Psi
%; RS1
% [object,time]
 
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:112
% !HoldsAt(directlyIn(object,object),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',112).

 /*  [] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          holds_at(neg(directlyIn(Object, Object)), Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',112).
axiom(holds_at(neg(directlyIn(Object, Object)), Time),
    []).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:114
%; RS2
% [object1,object2,time]
% HoldsAt(directlyIn(object1,object2),time) ->
% !HoldsAt(directlyIn(object2,object1),time).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:117
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',116).

 /*  holds_at(directlyIn(Object1, Object2), Time) ->
       holds_at(neg(directlyIn(Object2, Object1)), Time).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:117
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',116).

 /*  holds_at(neg(directlyIn(Object2, Object1)), Time) :-
       holds_at(directlyIn(Object1, Object2), Time).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:117
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',116).

 /*  [holds_at(directlyIn(Object1, Object2), Time)] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          holds_at(neg(directlyIn(Object2, Object1)), Time)).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:117
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',116).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:117
axiom(holds_at(neg(directlyIn(Object2, Object1)), Time),
    [holds_at(directlyIn(Object1, Object2), Time)]).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:117

 /*  not(holds_at(directlyIn(Object1, Object2), Time)) :-
       not(holds_at(neg(directlyIn(Object2, Object1)), Time)).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:117
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',116).

 /*  [holds_at(directlyIn(Object2, Object1), Time)] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          holds_at(neg(directlyIn(Object1, Object2)), Time)).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:117
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',116).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:117
axiom(holds_at(neg(directlyIn(Object1, Object2)), Time),
    [holds_at(directlyIn(Object2, Object1), Time)]).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:119
%; RS3
% [object1,object2,object3,time]
% HoldsAt(directlyIn(object1,object2),time) &
% HoldsAt(directlyIn(object2,object3),time) ->
% !HoldsAt(directlyIn(object1,object3),time).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:123
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',121).

 /*  holds_at(directlyIn(Object1, Object2), Time), holds_at(directlyIn(Object2, Object3), Time) ->
       holds_at(neg(directlyIn(Object1, Object3)), Time).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:123
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',121).

 /*  holds_at(neg(directlyIn(Object1, Object3)), Time) :-
       holds_at(directlyIn(Object1, Object2), Time),
       holds_at(directlyIn(Object2, Object3), Time).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:123
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',121).

 /*  [holds_at(directlyIn(Object1, Object2), Time), holds_at(directlyIn(Object2, Object3), Time)] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          holds_at(neg(directlyIn(Object1, Object3)), Time)).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:123
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',121).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:123
axiom(holds_at(neg(directlyIn(Object1, Object3)), Time),
   
    [ holds_at(directlyIn(Object1, Object2), Time),
      holds_at(directlyIn(Object2, Object3), Time)
    ]).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:123
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',121).

 /*  not(holds_at(directlyIn(Object1, Object2), Time)) :-
       holds_at(directlyIn(Object2, Object3), Time),
       not(holds_at(neg(directlyIn(Object1, Object3)), Time)).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:123
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',121).

 /*  [holds_at(directlyIn(Object2, Object3), Time), holds_at(directlyIn(Object1, Object3), Time)] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          holds_at(neg(directlyIn(Object1, Object2)), Time)).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:123
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',121).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:123
axiom(holds_at(neg(directlyIn(Object1, Object2)), Time),
   
    [ holds_at(directlyIn(Object2, Object3), Time),
      holds_at(directlyIn(Object1, Object3), Time)
    ]).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:123
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',121).

 /*  not(holds_at(directlyIn(Object2, Object3), Time)) :-
       holds_at(directlyIn(Object1, Object2), Time),
       not(holds_at(neg(directlyIn(Object1, Object3)), Time)).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:123
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',121).

 /*  [holds_at(directlyIn(Object1, Object2), Time), holds_at(directlyIn(Object1, Object3), Time)] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          holds_at(neg(directlyIn(Object2, Object3)), Time)).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:123
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',121).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:123
axiom(holds_at(neg(directlyIn(Object2, Object3)), Time),
   
    [ holds_at(directlyIn(Object1, Object2), Time),
      holds_at(directlyIn(Object1, Object3), Time)
    ]).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:125
%; RS4
% [object,object1,object2,time]
% HoldsAt(directlyIn(object,object1),time) &
% HoldsAt(directlyIn(object,object2),time) ->
% object1=object2.
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:129
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',127).

 /*  holds_at(directlyIn(Object, Object1), Time), holds_at(directlyIn(Object, Object2), Time) ->
       equals(Object1, Object2).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:129
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',127).

 /*  not(holds_at(directlyIn(Object, Object1), Time)) :-
       holds_at(directlyIn(Object, Object2), Time),
       not(equals(Object1, Object2)).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:129
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',127).

 /*  [holds_at(directlyIn(Object, Object2), Time), not(equals(Object1, Object2))] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          holds_at(neg(directlyIn(Object, Object1)), Time)).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:129
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',127).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:129
axiom(holds_at(neg(directlyIn(Object, Object1)), Time),
   
    [ holds_at(directlyIn(Object, Object2), Time),
      not(equals(Object1, Object2))
    ]).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:129
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',127).

 /*  not(holds_at(directlyIn(Object, Object2), Time)) :-
       holds_at(directlyIn(Object, Object1), Time),
       not(equals(Object1, Object2)).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:129
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',127).

 /*  [holds_at(directlyIn(Object, Object1), Time), not(equals(Object1, Object2))] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          holds_at(neg(directlyIn(Object, Object2)), Time)).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:129
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',127).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:129
axiom(holds_at(neg(directlyIn(Object, Object2)), Time),
   
    [ holds_at(directlyIn(Object, Object1), Time),
      not(equals(Object2, Object1))
    ]).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:131
%; RS7
% [object,room,time]
% HoldsAt(directlyIn(object,room),time) ->
% HoldsAt(inRoom(object,room),time).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:134
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',133).

 /*  holds_at(directlyIn(Object, Room), Time) ->
       holds_at(inRoom(Object, Room), Time).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:134
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',133).

 /*  holds_at(inRoom(Object, Room), Time) :-
       holds_at(directlyIn(Object, Room), Time).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:134
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',133).

 /*  [holds_at(directlyIn(Object, Room), Time)] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          holds_at(inRoom(Object, Room), Time)).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:134
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',133).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:134
axiom(holds_at(inRoom(Object, Room), Time),
    [holds_at(directlyIn(Object, Room), Time)]).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:134

 /*  not(holds_at(directlyIn(Object, Room), Time)) :-
       not(holds_at(inRoom(Object, Room), Time)).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:134
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',133).

 /*  [holds_at(neg(inRoom(Object, Room)), Time)] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          holds_at(neg(directlyIn(Object, Room)), Time)).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:134
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',133).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:134
axiom(holds_at(neg(directlyIn(Object, Room)), Time),
    [holds_at(neg(inRoom(Object, Room)), Time)]).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:136
%; RS8
% [object1,object2,room,time]
% HoldsAt(directlyIn(object1,object2),time) &
% HoldsAt(inRoom(object2,room),time) ->
% HoldsAt(inRoom(object1,room),time).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:140
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',138).

 /*  holds_at(directlyIn(Object1, Object2), Time), holds_at(inRoom(Object2, Room), Time) ->
       holds_at(inRoom(Object1, Room), Time).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:140
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',138).

 /*  holds_at(inRoom(Object1, Room), Time) :-
       holds_at(directlyIn(Object1, Object2), Time),
       holds_at(inRoom(Object2, Room), Time).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:140
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',138).

 /*  [holds_at(directlyIn(Object1, Object2), Time), holds_at(inRoom(Object2, Room), Time)] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          holds_at(inRoom(Object1, Room), Time)).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:140
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',138).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:140
axiom(holds_at(inRoom(Object1, Room), Time),
   
    [ holds_at(directlyIn(Object1, Object2), Time),
      holds_at(inRoom(Object2, Room), Time)
    ]).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:140
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',138).

 /*  not(holds_at(directlyIn(Object1, Object2), Time)) :-
       holds_at(inRoom(Object2, Room), Time),
       not(holds_at(inRoom(Object1, Room), Time)).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:140
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',138).

 /*  [holds_at(inRoom(Object2, Room), Time), holds_at(neg(inRoom(Object1, Room)), Time)] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          holds_at(neg(directlyIn(Object1, Object2)), Time)).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:140
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',138).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:140
axiom(holds_at(neg(directlyIn(Object1, Object2)), Time),
   
    [ holds_at(inRoom(Object2, Room), Time),
      holds_at(neg(inRoom(Object1, Room)), Time)
    ]).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:140
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',138).

 /*  not(holds_at(inRoom(Object2, Room), Time)) :-
       holds_at(directlyIn(Object1, Object2), Time),
       not(holds_at(inRoom(Object1, Room), Time)).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:140
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',138).

 /*  [holds_at(directlyIn(Object1, Object2), Time), holds_at(neg(inRoom(Object1, Room)), Time)] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          holds_at(neg(inRoom(Object2, Room)), Time)).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:140
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',138).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:140
axiom(holds_at(neg(inRoom(Object2, Room)), Time),
   
    [ holds_at(directlyIn(Object1, Object2), Time),
      holds_at(neg(inRoom(Object1, Room)), Time)
    ]).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:142
%; RS9
% [object,room1,room2,time]
% HoldsAt(inRoom(object,room1),time) &
% HoldsAt(inRoom(object,room2),time) ->
% room1=room2.
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:146
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',144).

 /*  holds_at(inRoom(Object, Room1), Time), holds_at(inRoom(Object, Room2), Time) ->
       equals(Room1, Room2).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:146
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',144).

 /*  not(holds_at(inRoom(Object, Room1), Time)) :-
       holds_at(inRoom(Object, Room2), Time),
       not(equals(Room1, Room2)).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:146
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',144).

 /*  [holds_at(inRoom(Object, Room2), Time), not(equals(Room1, Room2))] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          holds_at(neg(inRoom(Object, Room1)), Time)).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:146
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',144).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:146
axiom(holds_at(neg(inRoom(Object, Room1)), Time),
   
    [ holds_at(inRoom(Object, Room2), Time),
      not(equals(Room1, Room2))
    ]).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:146
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',144).

 /*  not(holds_at(inRoom(Object, Room2), Time)) :-
       holds_at(inRoom(Object, Room1), Time),
       not(equals(Room1, Room2)).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:146
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',144).

 /*  [holds_at(inRoom(Object, Room1), Time), not(equals(Room1, Room2))] ->
       ta(Time,
          tvs1=[Time],
          tvs2=[Time],
          holds_at(neg(inRoom(Object, Room2)), Time)).
 */
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:146
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',144).
% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:146
axiom(holds_at(neg(inRoom(Object, Room2)), Time),
   
    [ holds_at(inRoom(Object, Room1), Time),
      not(equals(Room2, Room1))
    ]).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:148
%; Gamma


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:150
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',151).
% HoldsAt(directlyIn(Lisa,LivingRoom),0).

 /*  [] ->
       ta(Ta_Param,
          tvs1=[start],
          tvs2=[start],
          holds_at(directlyIn(lisa, livingRoom), start)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',151).
axiom(holds_at(directlyIn(lisa, livingRoom), start),
    []).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:151
% HoldsAt(directlyIn(Newspaper,LivingRoom),0).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',151).

 /*  [] ->
       ta(Ta_Param,
          tvs1=[start],
          tvs2=[start],
          holds_at(directlyIn(newspaper, livingRoom), start)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',151).
axiom(holds_at(directlyIn(newspaper, livingRoom), start),
    []).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:152
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',153).
% HoldsAt(directlyIn(Box,LivingRoom),0).

 /*  [] ->
       ta(Ta_Param,
          tvs1=[start],
          tvs2=[start],
          holds_at(directlyIn(box, livingRoom), start)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',153).
axiom(holds_at(directlyIn(box, livingRoom), start),
    []).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:154
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',155).
%; added:                                                 
%; DMILES REMOVED [room1,room2,time] !HoldsAt(inRoom(room1,room2),time).
%; DMILES REMOVED [room,object,time] !HoldsAt(directlyIn(room,object),time).
%; entailed:


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:159
% HoldsAt(directlyIn(Lisa,LivingRoom),5).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',159).

 /*  [b(start, Start5), toffset(start, 5, Start5), ignore(start+5==Start5)] ->
       ta(Ta_Param,
          tvs1=[start+5],
          tvs2=[Start5, start],
          holds_at(directlyIn(lisa, livingRoom), Start5)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',159).
axiom(holds_at(directlyIn(lisa, livingRoom), Start5),
    [b(start, Start5), toffset(start, 5, Start5)]).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:160
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',161).
% HoldsAt(directlyIn(Box,Kitchen),5).

 /*  [b(start, Start5), toffset(start, 5, Start5), ignore(start+5==Start5)] ->
       ta(Ta_Param,
          tvs1=[start+5],
          tvs2=[Start5, start],
          holds_at(directlyIn(box, kitchen), Start5)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',161).
axiom(holds_at(directlyIn(box, kitchen), Start5),
    [b(start, Start5), toffset(start, 5, Start5)]).


% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:161
% HoldsAt(inRoom(Newspaper,Kitchen),5).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',161).

 /*  [b(start, Start5), toffset(start, 5, Start5), ignore(start+5==Start5)] ->
       ta(Ta_Param,
          tvs1=[start+5],
          tvs2=[Start5, start],
          holds_at(inRoom(newspaper, kitchen), Start5)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',161).
axiom(holds_at(inRoom(newspaper, kitchen), Start5),
    [b(start, Start5), toffset(start, 5, Start5)]).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:163
% completion Happens
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',163).
==> completion(happens).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:165
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',166).
% range time 0 5
==> range(time,0,5).

% From /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e:166
% range offset 1 1
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/ec_planner/ectest/TestBoxRoom.e',166).
==> range(offset,1,1).
%; End of file.
