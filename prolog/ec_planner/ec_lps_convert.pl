% =========================================
% Goal/Plan translating
% =========================================
:- module(ec_lps_convert,[%load_e/1, needs_proccess/3,process_ec/2,fix_time_args/3,fix_goal/3, 
  %brk_on_bind/1,assert_axiom_2/2,
   test_lps_ereader/0,
   is_e_toplevel/0]).


:- use_module(library(logicmoo_common)).

is_e_toplevel :- prolog_load_context(source,File),prolog_load_context(file,File).

ec_current_domain(X):- wdmsg(ec_current_domain(X)),fail.
export_transparent(P):-
  export(P),
  module_transparent(P).

:- user:use_module(library(logicmoo_lps)).
%:- user:use_module(library('ec_planner/ec_planner_dmiles')).
:- user:use_module(library(ec_planner/ec_reader)).

:- use_module(library(lps_corner)).

:- set_prolog_flag(lps_translation_only_HIDE,false).
:- set_prolog_flag(lps_translation_only,false).



assert_ep(Mod,load(X)):- assert_lps(Mod,act,load(X)).
assert_ep(Mod,option(N,V)):- assert_lps(Mod,act,option(N,V)).
assert_ep(Mod,Form):- 
  format('~N',[]), display(ef(Mod,Form)),nl,
  must_or_rtrace(ep_to_lps(Form,Lps)),
  must_or_rtrace(assert_lps(Mod,Form,Lps)),!.


assert_lps(Mod,Form,Lps):- is_list(Lps),!, maplist(assert_lps(Mod,Form),Lps).
assert_lps(Mod,t(Type,Inst),_):- atom(Type), M=..[Type,Inst],!,assert_lps(Mod,M,M),!.
assert_lps(Mod,Form,Lps):- 
 locally(current_prolog_flag(lps_translation_only_HIDE,true),
   locally(t_l:lps_program_module(Mod),
    must_or_rtrace(lps_f_term_expansion_now(Mod,Lps,Prolog)))),!,
  ignore(((Form\==Prolog,Lps==Prolog)->pprint_ecp(yellow,Lps))),
  ignore((Lps\==Prolog->pprint_ecp(cyan,Prolog) ;(pprint_ecp(red,Prolog)))),
  must_or_rtrace(assert_prolog(Mod,Prolog)),!.


assert_prolog(Mod,Prolog):- is_list(Prolog),!, maplist(assert_lps(Mod),Prolog).
assert_prolog(_Mod,Prolog):- % get_source_location(File,Line),
   nop(pprint_ecp(yellow,Prolog)).


test_lps_ereader:- 
 convert_e(assert_ep(test_lps_mod),user_error,
  ['../ext/ec_sources/ecnet/Diving.e',
   '../ext/ec_sources/foundations/*.e',
   '../ext/ec_sources/ecnet/*.e',
   '../test/ec_planner/*/*/*/*.e']).

ep_to_lps(Form,Lps):- \+ compound(Form),!,Lps=Form.
ep_to_lps(t(Type,Inst),Lps):- atom(Type), M=..[Type,Inst],!,ep_to_lps(M,Lps).
ep_to_lps(Form,Lps):- ep_to_lps_arg([],Form,Lps),!.

ep_to_lps_arg(_Top,Form,Lps):- \+ compound(Form),!,Lps=Form.
ep_to_lps_arg( Top,Form,Lps):- 
   compound_name_arguments(Form,F,Args),
   maplist(ep_to_lps_arg([F|Top]),Args,ArgsO),
   compound_name_arguments_maybe_zero(LpsM,F,ArgsO),
   over_pass(Top,LpsM,Lps),!.

compound_name_arguments_maybe_zero(F,F,[]):- !.
compound_name_arguments_maybe_zero(LpsM,F,ArgsO):- compound_name_arguments(LpsM,F,ArgsO).

:- use_module(library(lps_syntax)).

over_pass(_Top, X, X):- \+ compound(X),!.
over_pass(_Top, X, X):- functor(X,_,1), arg(1,X,Var), var(Var),!.
over_pass(_Top,at(X,Y),loc_at(X,Y)).
over_pass(_Top,holds_at(Fluent, Time),initially(Fluent)):- Time==0, !.
over_pass(_Top,holds_at(Fluent, Time),at(Fluent, Time)):- !.
over_pass(_Top,happens(Event,Time),(observe Event at Time)):- !.
over_pass(  [],initiates(Event,Fluent,Time),initiates(Event,Fluent)):- var(Time), !.
over_pass(  [],terminates(Event,Fluent,Time),terminates(Event,Fluent)):- var(Time), !.

over_pass(_Top, not(exists(_,X)), not(X)):-!.
over_pass(_Top, not(initially(X)),(initially not X)):-!.
over_pass(_Top, not(holds_at(X,T)),holds(not(X),T)).
over_pass(_Top, not(at(X,T)),holds(not(X),T)).



over_pass(_Top,initiates(Event,Fluent,Time),(Event initiates Fluent at Time)):- !.
over_pass(_Top,terminates(Event,Fluent,Time),(Event terminates Fluent at Time)):- !.
%over_pass(_Top,happensAt(Event,Time),at(observe(Event),Time)):- !.
over_pass(_Top,event(X),events(Lps)):- protify(X,Lps),!.
over_pass(_Top,fluent(X),fluents(Lps)):- protify(X,Lps),!.
over_pass(_Top,X=Y,Lps):- callable(X),append_term(X,Y,Lps).
over_pass(Top,'<->'(X1,X2),[Lps1,Lps2]):- over_pass(Top,'->'(X1,X2),Lps1),over_pass(Top,'->'(X2,X1),Lps2).
over_pass(_Top,'->'(X1,X2),(if X1 then X2)):-!.
over_pass(_Top,X,X).  
                                       
protify(Form,Lps):- \+ callable(Form),!,Lps=Form.
protify(Form,Lps):- \+ compound(Form),!,Lps=(Form/0).
protify((X1,X2),(Lps1,Lps2)):- !, protify(X1,Lps1),protify(X2,Lps2).
%protify(X,Lps):- cfunctor(X,F,A),Lps=(F/A).
protify(X,Lps):- cfunctor(X,F,A),cfunctor(Lps,F,A),!.



