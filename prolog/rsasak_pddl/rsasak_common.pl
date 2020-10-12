

:- break.

:- debug,(must(test_blocks)).


:- solve_files(pddl('benchmarks/mystery/domain.pddl'),pddl('benchmarks/mystery/prob01.pddl')).
:- test_domain(pddl('benchmarks/driverlog/domain.pddl'),4).
:- solve_files(pddl('hsp2_1_0_pddl/parcprinter-strips/p01-domain-woac.pddl'),
               pddl('hsp2_1_0_pddl/parcprinter-strips/p01-woac.pddl')).


% :-doall(on_call_decl_hyhtn).

:- if(gethostname(c3po);gethostname(ubuntu);gethostname(titan)).

:- test_domain(pddl('../domains_ocl/chameleonWorld/domain*')).
:- test_all(5). % should be 7

/*
:- if(current_predicate(pce_show_profile/0)).
:- pce_show_profile.
:- endif.
*/

:- retractall(t_l:loading_files).
:- endif.

twhy
  :- show_call(record_time(forall(between(1,1000000,_),forall(get_action_bb(_),true)),_Time1)),
   show_call(record_time(forall(between(1,1000000,_),forall(actn(_,_),true)),_Time2)).

% :- twhy.

% BAD :- test_domain(pddl('elearning/domain.pddl')).
% :- test_all.

% 
% :- solve_files(pddl('benchmarks/nomystery-sat11-strips/domain.pddl'),pddl('benchmarks/nomystery-sat11-strips/p01.pddl')).
% :- test_domain(pddl('benchmarks/nomystery-sat11-strips/domain.pddl').



:- if(gethostname(c3po);gethostname(ubuntu);gethostname(titan)).


/*

:- test_domain(pddl('../domains_ocl/toasterWorldv2/domain*')).

:- solve_files(pddl('regression-tests/issue58-domain.pddl'),pddl('regression-tests/issue58-problem.pddl')).
:- forall(must_filematch(pddl('hsp-planners-master/?*?/pddl/?*?/?*domain*.*'),E),once(test_domain(E,4))).
:- forall(must_filematch(pddl('hsp-planners-master/?*?/examples/?*?/?*domain*.*'),E),once(test_domain(E,5))).

:- test_domain(pddl('benchmarks/nomystery-sat11-strips/domain.pddl')).

test_blocks:- fail, test_domain(pddl('benchmarks/nomystery-sat11-strips/domain.pddl',RList),reverse(RList,List),
  forall(member(E,List),once(test_domain(E))).

% :-asserta(t_l:loading_files).

:- forall(must_filematch(pddl('rover/?*?/?*domain*.*'),E),once(load_domain(E))).
:- forall(must_filematch(pddl('hsp-planners-master/?*?/pddl/?*?/?*domain*.*'),E),once(load_domain(E))).
:- forall(must_filematch(pddl('hsp-planners-master/?*?/examples/?*?/?*domain*.*'),E),once(load_domain(E))).
:- forall(must_filematch(pddl('hsp-planners-master/?*?/examples/?*?/?*domain*.*'),E),once(load_domain(E))).
:- forall(must_filematch(pddl('primaryobjects_strips/?*?/?*domain*.*'),E),once(test_domain(E))).
:- solve_files(pddl('hakank-pddl/monkey-domain.pddl'),pddl('hakank-pddl/monkey-prob01.pddl')).

*/ 

:- endif.

% :- test_all(7).

:- show_call(flag(time_used_other,W,W)).
:- show_call(flag(time_used,W,W)).

:- fixup_exports.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Handling mutexes

 make_mutex(M):-
		bagof(R1, forbiden_pair(R1), MA),
		bagof(R2, forbiden_pair(MA, R2), MB),
%		writel(MA),nl,
%		writel(MB),nl,
		union(MA, MB, M0),
%		list_to_set(M0_, M0),
%		write('Cistim:'),nl,	
		clear_mutex1(M0, M1),
		clear_mutex2(M1, M2),
		clear_duplicates(M2, M).
		%write('Ocistene:'),nl,writel(M),nl, length(M, L), write('Pocet: '), write(L),nl.

clear_duplicates([], []).
clear_duplicates([H|T], R):-
    member(M, T),
    identical_but_for_variables(H, M),
    !,
    clear_duplicates(T, R).
clear_duplicates([H|T], [H|R]):-
    clear_duplicates(T, R).

forbiden_pair(R):-
		get_action(A),
		get_positiv_effect(A, PE),
		get_negativ_effect(A, NE),
		member(P, PE),
		member(Q, NE),
		copy_term_spec(P-Q, R).
forbiden_pair(MA, NR):-
		member(P-Q, MA),
		get_action(A),
		get_precondition(A, Precond),
		get_positiv_effect(A, PE),
		member(R, Precond),
		member(P, PE),
		copy_term_spec(R-Q, NR).

clear_mutex1([], []):-!.
clear_mutex1([PP-QQ|T], M):-
		(P-Q = PP-QQ ; P-Q = QQ-PP),
		get_init(I),
		select_20_faster(P, I, R),
		member(Q, R),
%		write('Rule1: '), write(PP-QQ),nl,
		clear_mutex1(T, M), !.
clear_mutex1([P-Q|R], [P-Q|M]):-
		clear_mutex1(R, M).

clear_mutex2(M0, M):-
		(select_20_faster(P-Q, M0, R) ; select_20_faster(Q-P, M0, R)),
		get_action(A, _Def), get_precondition(A, Precond), get_positiv_effect(A, PE), get_negativ_effect(A, NE),
		select_20_faster(P, PE, RPE),
		\+ member(Q, NE),
		(
			member(Q, RPE)%, write('prva cast')
			;
			all_not_in(Precond, P, Q, M0)%, write('druha cast')
		),
%		write('Rule2: '), write(P-Q-_Def),nl,

		clear_mutex2(R, M), !.
clear_mutex2(M0, M0).

all_not_in([], _, _, _).
all_not_in([P|T], P, Q, M):-
	all_not_in(T, P, Q, M).
all_not_in([R|T], P, Q, M):-
		\+ (member(R-Q, M) ; member(Q-R, M)),
		%write(precon-R),nl,
		all_not_in(T, P, Q, M).



%check_mutex(+State).
check_mutex(S):-
		bb_get(mutex, M),
		pairfrom(S, P, Q, _),
		(member(P-Q, M) ; member(Q-P, M)),
%		write('Mutex pair.'), write(P-Q), nl,
		!, fail.
check_mutex(_).


identical_but_for_variables(X, Y) :-
		\+ \+ (
			copy_term(X, Z),
			numbervars(Z, 0, N),
			numbervars(Y, 0, N),
			Z = Y
		).% Dostupne veci:
		



