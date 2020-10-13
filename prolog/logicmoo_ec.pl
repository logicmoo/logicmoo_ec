:- module(logicmoo_ec,[test_logicmoo_ec/0]).

% [Required] Load the Logicmoo Library Utils
:- ensure_loaded(library(logicmoo_common)).

:- reexport(logicmoo_planner).

:- use_module(library(logicmoo_lps)).
:- use_module(library(logicmoo_dec)).
:- use_module(library(logicmoo_icl)).
:- use_module(library(logicmoo_rsasak)).
:- use_module(library(logicmoo_ocl)).

test_logicmoo_ec:- run_tests.


