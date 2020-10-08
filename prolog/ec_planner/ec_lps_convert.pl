% =========================================
% Goal/Plan translating
% =========================================
:- module(ec_lps_convert,[%load_e/1, needs_proccess/3,process_ec/2,fix_time_args/3,fix_goal/3, 
  %brk_on_bind/1,assert_axiom_2/2,
   assert_lps/1,
   test_logicmoo_ec_lps_reader/0,
   test_lps_ereader/0,
   test_logicmoo_ec_reader_2/0,test_logicmoo_ec_lps_reader/1]).


:- use_module(library(logicmoo_common)).

:- use_module(library(ec_planner/ec_loader)).

ec_current_domain(X):- wdmsg(ec_current_domain(X)),fail.
/*export_transparent(P):-
  export(P),
  module_transparent(P).
*/
:- use_module(library(logicmoo_lps)).
%:- user:use_module(library('ec_planner/ec_planner_dmiles')).
:- use_module(library(ec_planner/ec_reader)).

:- use_module(library(lps_corner)).

:- set_prolog_flag(lps_translation_only_HIDE,false).
:- set_prolog_flag(lps_translation_only,false).


assert_lps(user:ec_current_domain_db(Stuff,_)):-!,  assert_lps(Stuff).
assert_lps(axiom(Stuff,Nil)):- Nil==[],!,assert_lps(Stuff).
assert_lps(axiom(Stuff,List)):- !,must_or_rtrace(list_to_conjuncts(List,Body)),!,assert_lps(->(Body,Stuff)).
assert_lps(Stuff):- assert_ep(lps_test_mod,Stuff).

assert_ep(Mod,option(N,V)):- assert_lps(Mod,option(N,V),option(N,V)).
assert_ep(Mod,Form):- 
  format('~N',[]),
  must_or_rtrace(ep_to_lps(Form,Lps)),
  ignore((Form\==Lps->pprint_ecp_cmt(hfg(green),Form))),
  must_or_rtrace(assert_lps(Mod,Form,Lps)),!.

assert_lps(Mod,_,include(F)):- include_e_lps_file_now(Mod:F). % with_e_file(assert_ep(Mod),current_output, [F]). 
assert_lps(Mod,_,load(F)):- include_e_lps_file_now(Mod:F). % with_e_file(assert_ep(Mod),current_output, [F]). 
%assert_lps(Mod,_,include(F)):- !, with_e_file(assert_ep(Mod),current_output, [ec(F)]). 
%assert_lps(Mod,_,load(X)):- nop(assert_ep(Mod,include(X))),!.
assert_lps(Mod,Form,Lps):- nortrace, is_list(Lps),!, maplist(assert_lps(Mod,Form),Lps).
assert_lps(Mod,t(Type,Inst),_):- atom(Type), M=..[Type,Inst],!,assert_lps(Mod,M,M),!.
assert_lps(Mod,_Form,Lps):- 
 locally(current_prolog_flag(lps_translation_only_HIDE,true),
   locally(t_l:lps_program_module(Mod),
    must_or_rtrace(lps_f_term_expansion_now(Mod,Lps,Prolog)))),!,
  must_or_rtrace((Lps\==Prolog->(ignore(( /*(Form\==Prolog,Lps==Prolog)-> */
    with_lps_operators2(user,ec_lps_convert:with_lps_operators2(pretty_clauses,pretty_clauses:clause_to_string(Lps,S))),
    real_ansi_format(hfg(yellow), '~N~s.~N', [S]),
     nop(pprint_ecp(yellow,Lps)))),
    pprint_ecp_cmt(cyan,Prolog))
   ;pprint_ecp(red,Prolog))),
  must_or_rtrace(assert_prolog(Mod,Prolog)),!.


:- export_transparent(with_lps_operators2/2).
with_lps_operators2(M,Goal):- 
   setup_call_cleanup(push_operators(M:[op(900, fy, M:not),  op(1200, xfx, M:then), op(1185, fx, M:if), op(1190, xfx, M:if), op(1100, xfy, M:else), op(1050, xfx, M:terminates), op(1050, xfx, M:initiates), op(1050, xfx, M:updates), op(1050, fx, M:observe), op(1050, fx, M:false), op(1050, fx, M:initially), op(1050, fx, M:fluents), op(1050, fx, M:events), op(1050, fx, M:prolog_events), op(1050, fx, M:actions), op(1050, fx, M:unserializable), op(999, fx, M:update), op(999, fx, M:initiate), op(999, fx, M:terminate), op(997, xfx, M:in), op(995, xfx, M:at), op(995, xfx, M:during), op(995, xfx, M:from), op(994, xfx, M:to), op(1050, xfy, M: ::), op(1200, xfx, M:(<-)), op(1050, fx, M:(<-)), op(700, xfx, M: <=)],Undo),
     M:call(Goal),pop_operators(Undo)).

:- export_transparent(with_lps_operators/1).
with_lps_operators(MGoal):- 
  strip_module(MGoal,M,Goal),
  with_lps_operators2(user,ec_lps_convert:with_lps_operators2(M,M:Goal)).


assert_prolog(Mod,Prolog):- is_list(Prolog),!, maplist(assert_prolog(Mod),Prolog).
assert_prolog(_Mod,Prolog):- % get_source_location(File,Line),
   pprint_ecp_pl(yellow,Prolog).

include_e_lps_file_now(MFile):- strip_module(MFile,M,File), include_e_lps_file_now(M,File).
include_e_lps_file_now(M,File):- absolute_file_name(File,AbsFile),File\==AbsFile,!,include_e_lps_file_now(M,AbsFile).

include_e_lps_file_now(Mod,File):- 
   atom_concat(File,'.lps',LPSFile),
   translate_e_to_filetype(Mod:'lps',File,LPSFile).

load_e_lps_file(File):- retractall(etmp:ec_option(load(_), _)), include_e_lps_file(File).
  
include_e_lps_file(File):- is_list(File), !, maplist(include_e_lps_file,File).
include_e_lps_file(File):- wdmsg(include_e_lps_file(File)),fail.
include_e_lps_file(File):- needs_resolve_local_files(File,Resolved),!,include_e_lps_file(Resolved).
include_e_lps_file(File):- absolute_file_name(File,DB), exists_file(DB),!, 
  update_changed_files,   
  strip_module(_,M,_), prolog_statistics:time(M:include_e_lps_file_now(File)),!.
include_e_lps_file(File):- throw(with_abs_paths(include_e_lps_file,File)).


test_logicmoo_ec_lps_reader(File):- load_e_lps_file(File).

test_logicmoo_ec_lps_reader:- 
 test_logicmoo_ec_lps_reader([ec('ecnet/Diving.e'), ec('foundations/*.e'), ec('ecnet/*.e')]).

test_logicmoo_ec_reader_2:- 
 test_logicmoo_ec_lps_reader(library('../test/ec_planner/*/*/*/*.e')).


test_lps_ereader:- 
 convert_e(assert_ep(test_lps_mod),user_error,
  ['../ext/ec_sources/ecnet/Diving.e',
   '../ext/ec_sources/foundations/*.e',
   '../ext/ec_sources/ecnet/*.e',
   '../test/ec_planner/*/*/*/*.e']).



ep_to_lps(Form,Lps):- \+ compound(Form),!,Lps=Form.
ep_to_lps(t(Type,Inst),Lps):- atom(Type), M=..[Type,Inst],!,ep_to_lps(M,Lps).
ep_to_lps(Form,LpsO):- ep_to_lps_arg([],Form,Lps),
  must_or_rtrace((fix_axiom_head(_Global,Lps,LpsM),over_pass([],LpsM,LpsO))).

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
over_pass(_Top, X, X):- functor(X,_,1), arg(1,X,Var), is_ftVar(Var),!.
over_pass(_Top,at(X,Y),loc_at(X,Y)).
over_pass(Top,neg(X),Rest):- ep_to_lps_arg(Top,not(X),Rest).
over_pass(_Top,holds_at(Fluent, Time),initially(Fluent)):- Time==start, !.
over_pass(_Top,holds_at(Fluent, Time),initially(Fluent)):- Time==0, !.
over_pass(_Top,holds_at(Fluent, Time),at(Fluent, Time)):- !.
over_pass(_Top,happens(Event,Time),(observe Event at Time)):- nonvar(Time), !.
% observe(from(muestra_del_general('+86-555000001'),to(2,3)))
over_pass(  [],initiates(Event,Fluent,Time),initiates(Event,Fluent)):- is_ftVar(Time), !.
over_pass(  [],terminates(Event,Fluent,Time),terminates(Event,Fluent)):- is_ftVar(Time), !.

over_pass(_Top, not(exists(_,X)), not(X)):-!.
over_pass(_Top, not(initially(X)),(initially not X)):-!.
over_pass(_Top, not(holds_at(X,T)),at(not(X),T)).
over_pass(_Top, not(at(X,T)),at(not(X),T)).



over_pass(_Top,initiates(Event,Fluent,Time),(Event initiates Fluent at Time)):- !.
over_pass(_Top,terminates(Event,Fluent,Time),(Event terminates Fluent at Time)):- !.
%over_pass(_Top,happensAt(Event,Time),at(observe(Event),Time)):- !.
over_pass(_Top,Form,LpsO):- Form=..[EFP,X], argtype_pred(EFP,_), protify(EFP,X,Lps),!,flatten([Lps],LpsO).
over_pass(_Top,X=Y,Lps):- callable(X),append_term(X,Y,Lps).
over_pass(Top,'<->'(X1,X2),[Lps1,Lps2]):- over_pass(Top,'->'(X1,X2),Lps1),over_pass(Top,'->'(X2,X1),Lps2).
over_pass(_Top,'->'(X1,X2),(if X1 then X2)):-!.
over_pass(_Top,X,X).  

argtype_pred(event,events).
argtype_pred(fluent,fluents).
argtype_pred(action,actions).
argtype_pred(predicate,predicates).
argtype_pred(Action,Actions):- arg_info(domain,Action,arginfo),atom_concat(Action,"s",Actions).

protify(Type,Form,Lps):- is_list(Form),!,maplist(protify(Type),Form,Lps).
protify(Type,Form,Lps):- argtype_pred(Type,LPSType), \+ callable(Form),!,Lps=..[LPSType,[Form]].
protify(Type,Form,Lps):- argtype_pred(Type,LPSType), \+ compound(Form),!,Lps=..[LPSType,[Form/0]].
protify(Type,(X1,X2),[Lps1,Lps2]):- !, protify(Type,X1,Lps1),protify(Type,X2,Lps2).
%protify(Type,X,Lps):- cfunctor(X,F,A),Lps=(F/A).
protify(Event,X,LPS):- ((event) == Event), compound(X), arg(1,X,Agent),agent==Agent,!,protify(action,X,LPS).
protify(Type,X,LPS):- argtype_pred(Type,LPSType), cfunctor(X,F,A),cfunctor(_Lps,F,A),!,Pred=..[LPSType,[F/A]],LPS=[Pred/*,mpred_prop(X,Type)*/].


