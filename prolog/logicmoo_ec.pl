:- module(logicmoo_ec,[test_logicmoo_ec/0]).

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

test_logicmoo_ec:- 
   test_logicmoo_ec_sanity,
   test_logicmoo_ec_lps_reader.


%:- use_module(library(lps_corner)).
:- user:use_module(library(ec_planner/ec_lps_convert)).
:- user:reexport(library(ec_planner/ec_lps_convert)).

:- use_module(library(eggdrop)).
:- egg_go.

