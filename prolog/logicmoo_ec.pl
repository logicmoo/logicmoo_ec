:- module(logicmoo_ec,[]).

% [Required] Load the Logicmoo Library Utils
:- ensure_loaded(library(logicmoo_utils)).

:- reexport(logicmoo_planner).

:- use_module(logicmoo_lps).


/*
:- add_absolute_search_folder(pack,'../ext/').
:- initialization(attach_packs).
:- initialization(attach_packs,now).

:- if( \+ exists_source(library(lps_corner))).
:- initialization(attach_packs).
:- initialization(attach_packs,now).
:- add_absolute_search_folder(pack,'../../logicmoo_webui/').
:- endif.

:- initialization(attach_packs).
:- initialization(attach_packs,now).
*/

e_lps_reader_test(Files):- with_abs_paths(convert_e(user_output),Files).


%:- use_module(library(lps_corner)).
:- user:use_module(library(ec_planner/ec_lps_convert)).
:- user:reexport(library(ec_planner/ec_lps_convert)).

:- use_module(library(eggdrop)).
:- egg_go.

