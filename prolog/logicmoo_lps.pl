:- module(logicmoo_lps,[]).

% [Required] Load the Logicmoo Library Utils
:- ensure_loaded(library(logicmoo_utils)).

:- reexport(logicmoo_planner).

:- if(\+ exists_source(swish(lib/render))).
:- add_absolute_search_folder(swish,'../../logicmoo_webui/swish').
:- endif.


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


swish:is_really_module.

run_lps_corner1(File):- with_abs_paths(run_lps_corner_file1,File).

:- user:use_module(library(listing)).
:- prolog_listing:use_module(library(lists)).

system:run_lps_corner_file1(File):- wdmsg(run_lps_corner_file1(File)),fail.
system:run_lps_corner_file1(File):-
   absolute_file_name(File,DB),!,
   DB:use_module(library(lps_corner)), 
   %listing(db:actions/1),
   %listing(interpreter:actions/1),
   interpreter:check_lps_program_module(DB),
   interpreter:must_lps_program_module(DB),
   DB:consult(DB),
      interpreter:must_lps_program_module(DB),
   write('% '), writeq(:-listing(DB:_)),writeln('.\n'),
   elsewhere:listing(DB:_),!,
   DB:golps(X),
   %listing(interpreter:lps_program_module/1),
   wdmsg(dB(DB,X)).

%load_lps_corner:-!.
user:test_lps_corner:- 
  debug(lps(term_expand)),
  run_lps_corner1(library('../examples/binaryChop2.pl')),
  %test_lps_corner('../test/lps_user_examples/*.lps'),
  test_lps_corner('../test/lps_user_examples/{s,S}*.pl'),
  test_lps_corner('../test/lps_user_examples/*.pl'),!,
  nodebug(lps(term_expand)),!.



test_lps_corner(Mask):-  expand_file_name(Mask,Files),!,run_lps_corner1(Files).
     

