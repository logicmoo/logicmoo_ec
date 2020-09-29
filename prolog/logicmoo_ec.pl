:- module(logicmoo_ec,[]).

:- reexport(logicmoo_planner).
:- use_module(library(lps_corner)).
:- reexport(library(ec_planner/ec_lps_convert)).
:- reexport(library(ec_planner/ec_reader)).

:- use_module(library(eggdrop)).
:- egg_go.

