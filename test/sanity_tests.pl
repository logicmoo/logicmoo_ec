
:- use_module(library(logicmoo_common)).

:- multifile(prolog_trace_interception/4).
:- dynamic(prolog_trace_interception/4).
:- multifile(user:prolog_trace_interception/4).
:- dynamic(user:prolog_trace_interception/4).

:- use_module(library(plunit)).
:- use_module(library(test_cover)).
:- use_module(library(listing)).

:- set_prolog_flag(must_saftey,3).
:- set_prolog_flag(must_debug,0).
:- set_prolog_flag(must_speed,0).
:- set_prolog_flag(must_type,keep_going).

:- listing(prolog_trace_interception/4).
:- listing(user:prolog_trace_interception/4).

:- prolog_load_context(source,Loader),
  ( \+ prolog_load_context(file,Loader) -> assert(t_l:santiy_tests_includer(Loader)) ; true).

:- current_prolog_flag(toplevel_goal,default) -> set_prolog_flag(toplevel_goal,user:sanity_test_default); true.

user:sanity_test_default:- halt(0).

term_expansion(EOF,S,Out,S):- nonvar(S),
  EOF == end_of_file,
  prolog_load_context(file,Loader),
  retract(t_l:santiy_tests_includer(Loader)),
  Out = 
  [(:- set_test_options([silent(false)])),
   (:-  set_test_options([load(never)])),
   (:- use_module(library(test_wizard))),
   (:- set_prolog_flag(log_query_file, '/tmp/Queries.pl')),
 % (:- run_ec_tests(sanity_tests)),
   (:- show_coverage(run_ec_tests)),
   end_of_file],!.

:-  Unit = sanity_tests, prolog_load_context(source,File), plunit:make_unit_module(Unit, Name), 
    plunit:begin_tests(Unit, Name, File:3, []).



