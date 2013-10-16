-module(weather).
-export([output/0]).

%TODO - this isn't even close to being complete, So far it finds the position of the data to be parsed.

%part one
%In weather.dat you’ll find daily weather data for Morristown, NJ for June 2002.
%Download this text file, then write a program to output the day number (column one) with the smallest temperature spread
%(the maximum temperature is the second column, the minimum the third column).

output() ->
  {ok, Result} = file:read_file('weather.dat'),
  {Start, _} = binary:match(Result, <<"pre>">>, []),
  {End, _} = binary:match(Result, <<"</pre>">>, []),
  binary:bin_to_list(binary:part(Result, Start, End - Start)).