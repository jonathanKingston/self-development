-module(proc1).
-export([mouth/1, person/0, start/0]).

%%% In this I setup a process registered as person
%%% Person creates a mouth process which it sends messages to
%%% Mouth continues to call itself until it receives a bye command (In this case this is after one call)

mouth(String) ->
  io:format("~w: Starting mouth ~w ~n", [self(), String]),
  receive
    bye ->
      io:format("~w: and goodbye all. ~n", [self()]);
    Msg ->
      io:format("~w: Mouth ~w ~w From: ~w ~n", [self(), String, Msg, person]),
      person ! {self(), rec},
      mouth(String)
  end.

person() ->
  io:format("~w: Starting person ~n", [self()]),
  Mouth_Pid = spawn(proc1, mouth, ['Ello']),
  Mouth_Pid ! 'World',
  receive
    {Mouth_Pid, rec} ->
      io:format("~w: Person received mouth command From: ~w ~n", [self(), Mouth_Pid]),
      Mouth_Pid ! bye
  end.

start() ->
  io:format("~w: Starting people ~n", [self()]),
  register(person, spawn(proc1, person, [])),
  done.
