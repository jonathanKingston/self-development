-module(chop2).
-export([chop/2, chop/3]).
-include_lib("eunit/include/eunit.hrl").

%This algorithm is implemented using passing an index
%It compares each value smaller and larger with the search number

chop(Search_number, List) ->
  chop(Search_number, List, 0).

chop(_, [], _) ->
  -1;
chop(Search_number, List, Index_start) ->
  Length = length(List),
  Middle = Length div 2,
  if
    (Length =:= 1) and (Search_number =:= hd(List)) ->
      Index_start;
    true ->
      Middle_value = lists:nth(Middle, List),
      if
        Middle_value =:= Search_number ->
          Index_start + Middle;
        Middle_value >= Search_number ->
          {First, _} = lists:split(Middle, List),
          chop(Search_number, First, Index_start);
        Middle_value =< Search_number ->
          {_, Last} = lists:split(Middle, List),
          chop(Search_number, Last, Index_start + Middle);
        true ->
          -1
      end
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