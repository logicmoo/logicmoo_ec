:- module(logicmoo_planner,[load_planner_api/0]).

:- style_check(-singleton).

:- use_module(library(prolog_pack)).
:- if( ( \+ prolog_pack:current_pack(planner_api) , false) ).
:- dynamic   user:file_search_path/2.
:- multifile user:file_search_path/2.
:- prolog_load_context(directory,Dir),
   DirFor = planner,
   (( \+ user:file_search_path(DirFor,Dir)) ->asserta(user:file_search_path(DirFor,Dir));true),
   absolute_file_name('../..',Y,[relative_to(Dir),file_type(directory)]),
   (( \+ user:file_search_path(pack,Y)) ->asserta(user:file_search_path(pack,Y));true).
:- attach_packs.
:- initialization(attach_packs).
:- endif.


:- if( \+  user:file_search_path(pddl,_) ).
:- prolog_load_context(directory,Dir),
   must((absolute_file_name('../pddl',Y,[relative_to(Dir),file_type(directory)]),
   asserta(user:file_search_path(pddl,Y)))).
:- endif.


% [Required] Load the Logicmoo Library Utils
% :- ensure_loaded(library(logicmoo_hyhtn)).
% 
load_planner_api:- ensure_loaded(library(rsasak_forward_wa_star_h_add)).
%:- initialization(load_planner, program).
:- fixup_exports.




