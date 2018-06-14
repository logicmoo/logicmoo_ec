/** <module> logicmoo_hyhtn
% Provides a prolog database *env*
%
%  
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Denton, TX 2005, 2010, 2014 
% Dec 13, 2035
%
*/
:-module(logicmoo_hyhtn,[]).

:- use_module(library(prolog_pack)).
:- if( \+ prolog_pack:current_pack(logicmoo_planners)).
:- dynamic   user:file_search_path/2.
:- multifile user:file_search_path/2.
:- prolog_load_context(directory,Dir),
   DirFor = planner,
   (( \+ user:file_search_path(DirFor,Dir)) ->asserta(user:file_search_path(DirFor,Dir));true),
   absolute_file_name('../../../../',Y,[relative_to(Dir),file_type(directory)]),
   (( \+ user:file_search_path(pack,Y)) ->asserta(user:file_search_path(pack,Y));true).
:- initialization(attach_packs,now).
:- endif.
% [Required] Load the Logicmoo Library Utils
:- ensure_loaded(library(logicmoo_utils)).

do_ss(A,B):-do_ss,!, must(do_ss0(A,B)).
do_ss(A,A).

do_ss0(A,B):- \+ compound(A), !, B=A.
do_ss0(is_of_sort(I,C),isa(I,C)).
do_ss0(ss(C,I,G),GG):-do_ss0(ss([is_of_sort(I,C)|G]),GG).
do_ss0(se(C,I,G),GG):-do_ss0(se([is_of_sort(I,C)|G]),GG).
do_ss0(sc(C,I,=>(L,R)),GG):-do_ss0(sc(=>([is_of_sort(I,C)|L],[is_of_sort(I,C)|R])),GG).
%do_ss(sc(C,I,G),sc(G)):-!.
do_ss0(A,B):- A=..[F|AA],must_maplist(do_ss0,AA,BB),B=..[F|BB].

:-dynamic(do_ss/0).
:-dynamic(do_ss_in_file/0).
system:term_expansion(A,B):- do_ss, loop_check(do_ss(A,B)).
system:goal_expansion(A,B):- do_ss, loop_check(do_ss(A,B)).

:- ensure_loaded(library(logicmoo_util_structs)).
:- ensure_loaded(library(logicmoo_util_bb_env)).
%:-asserta(do_ss).
%do_ss_in_file.
do_non_ss_in_file.
term_expansion(A,B):-env_term_expansion(A,B).


:- ensure_loaded(logicmoo_hyhtn_code).
:- ensure_loaded(library(logicmoo_ocl_and_pddl)).
% :-include(logicmoo_hyhtn_works).

:- fixup_exports.
