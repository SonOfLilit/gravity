-module(gravity).
-author('sonoflilit@gmail.com').

-behaviour(application).

-include("sdl.hrl").
-include("sdl_events.hrl").
-include("sdl_video.hrl").
-include("sdl_keyboard.hrl").
-include("sdl_gfx.hrl").

-export([start/0,
         start/2,
         stop/1,
         go/0,
         go/1,
         ship_controller/4]).

-define(TICK_LENGTH, 0.1).

start() ->
    start(normal, []).

start(normal, Args) ->
    go().

stop(_) ->
    sdl:quit().

go() ->
    go([]).
go(Config) ->
    gravity_display:start_link(640),

    Ship1 = spawn(?MODULE, ship_controller, [{0.5, 0.5}, {0.1, 0.1}, 1.3, 0.01]),

    loop([Ship1]),

    sdl:quit().

loop(Objects) ->
    timer:sleep(20),
    Self = self(),
    Graphics = lists:map(
      fun(E) ->
              E ! {tick, Self},
              receive
                  List
                  when list(List) ->
                      List
              end
      end,
      Objects),

    Vectors = lists:flatten(Graphics),
    gravity_display:draw_frame(Vectors),
    loop(Objects).

ship_controller(Pos = {X, Y}, V = {Vx, Vy}, Angle, Va) ->
    receive
        {tick, Pid} ->
            Pid ! vector_ship(X, Y, Angle),
            ship_controller({X + ?TICK_LENGTH*Vx, Y + ?TICK_LENGTH*Vy},
                            {Vx, Vy}, Angle + ?TICK_LENGTH*Va, Va);
        stop ->
            ok
    end.

vector_ship(X, Y, Angle) ->
    [{X, Y, X + 0.02*math:cos(Angle), Y + 0.02*math:sin(Angle)}].
