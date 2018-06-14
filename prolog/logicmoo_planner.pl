

:- module(logicmoo_planner,[]).

:- use_module(library(prolog_pack)).
:- if( \+ prolog_pack:current_pack(logicmoo_planners)).
:- dynamic   user:file_search_path/2.
:- multifile user:file_search_path/2.
:- prolog_load_context(directory,Dir),
   %Dir = (DirThis/planner),
   DirFor = planner,
   (( \+ user:file_search_path(DirFor,Dir)) ->asserta(user:file_search_path(DirFor,Dir));true),
   absolute_file_name('../../../',Y,[relative_to(Dir),file_type(directory)]),
   (( \+ user:file_search_path(pack,Y)) ->asserta(user:file_search_path(pack,Y));true).
:- attach_packs.
:- initialization(attach_packs).
:- endif.

% [Required] Load the Logicmoo Library Utils
% :- ensure_loaded(library(logicmoo_hyhtn)).
:- ensure_loaded(library(rsasak_forward_wa_star_h_add)).
:- fixup_exports.




