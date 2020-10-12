:- module(logicmoo_planner,[load_planner_api/0]).


:- style_check(-singleton).

:- use_module(library(prolog_pack)).

:- if( \+ prolog_pack:current_pack(logicmoo_ec)).
:- add_absolute_search_folder(pack,'../../').
:- attach_packs.
:- initialization(attach_packs).
:- endif.

%:-  add_absolute_search_folder(ext,('../ext/')).
:-  add_absolute_search_folder(ext,library('../ext/')).
%:-  add_absolute_search_folder(pack,library('../prolog/../..')).

:- if( \+  user:file_search_path(pddl,_) ).

:-  add_absolute_search_folder(pddl,library('../test/pddl_tests/')),
    add_absolute_search_folder(pddl,library('../test/uw-yale-pddl/strict-domains/')),
    add_absolute_search_folder(pddl,library('../test/uw-yale-pddl/domains/')),
    !.
         
:- endif.

% [Required] Load the Logicmoo Library Utils
% :- ensure_loaded(library(logicmoo_hyhtn)).
% 
% load_planner_api:- ensure_loaded(library(rsasak_forward_wa_star_h_add)).
%:- initialization(load_planner, program).
load_planner_api.

:- fixup_exports.




