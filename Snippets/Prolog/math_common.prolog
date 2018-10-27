:- module(math_common,
          [powerset/2]).

:- use_module(library(oset)).

%% Generates the  powerset of a list  L. P is a  single member (single
%% answer/solution) of  the set of  solutions. Note that  this removes
%% the  empty list  from the  powerset solution,  so this  is isn't  a
%% strict   implementation  of   powerset.  If   you  need   a  strict
%% implementation   of   powerset,    then   the   oset_power/2   from
%% library(oset) should do the job.
powerset(L, P) :- oset_power(L, X), delete(X, [], Y), member(P, Y).
