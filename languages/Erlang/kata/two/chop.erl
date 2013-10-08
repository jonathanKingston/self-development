-module(chop).
-export([chop/2, chop/3]).
-include_lib("eunit/include/eunit.hrl").

%In this code, I approached the algorithm by passing an Index
%This algorithm only checks each side in the hope that it will cope with each finding the first index of multiple in the same list

chop(Search_number, List) ->
  chop(Search_number, List, 0).

chop(Search_number, [], Index_start) ->
  -1;
chop(Search_number, List, Index_start) ->
  Middle = length(List) div 2,
  {First, Last} = lists:split(Middle, List),
  Lf = lists:last(First),
  Fl = hd(Last),
  if
      length(List) =:= 2 ->
        if
          Lf =:= Search_number ->
            Index_start;
          Fl =:= Search_number ->
            Index_start + 1;
          true ->
            -1
        end;
      Fl =< Search_number ->
        chop(Search_number, Last, Index_start + Middle);
      Lf >= Search_number ->
        chop(Search_number, First, Index_start);
      true ->
        -1
  end.

chop_test() ->
  [
    ?_assert(-1 =:= chop(3, [])),
    ?_assert(-1 =:= chop(3, [1])),
    ?_assert(0 =:= chop(1, [1])),
    %%%
    ?_assert(0 =:= chop(1, [1, 3, 5])),
    ?_assert(1 =:= chop(3, [1, 3, 5])),
    ?_assert(2 =:= chop(5, [1, 3, 5])),
    ?_assert(-1 =:= chop(0, [1, 3, 5])),
    ?_assert(-1 =:= chop(2, [1, 3, 5])),
    ?_assert(-1 =:= chop(4, [1, 3, 5])),
    ?_assert(-1 =:= chop(6, [1, 3, 5])),
    %%%
    ?_assert(0 =:= chop(1, [1, 3, 5, 7])),
    ?_assert(1 =:= chop(3, [1, 3, 5, 7])),
    ?_assert(2 =:= chop(5, [1, 3, 5, 7])),
    ?_assert(3 =:= chop(7, [1, 3, 5, 7])),
    ?_assert(-1 =:= chop(0, [1, 3, 5, 7])),
    ?_assert(-1 =:= chop(2, [1, 3, 5, 7])),
    ?_assert(-1 =:= chop(4, [1, 3, 5, 7])),
    ?_assert(-1 =:= chop(6, [1, 3, 5, 7])),
    ?_assert(-1 =:= chop(8, [1, 3, 5, 7]))
  ].