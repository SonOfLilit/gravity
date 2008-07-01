-module(gr_ship).
-author(sonoflilit@gmail.com).

-include("sdl.hrl").
-include("sdl_events.hrl").
-include("sdl_keyboard.hrl").
-include("sdl_gfx.hrl").

-include("gravity.hrl").

-export([start_link/5]).

-record(keys, {l, r, u, d}).

start_link(InitialState, LKey, RKey, UKey, DKey) ->
    spawn_link(
      fun() ->
              loop(InitialState, #keys{l=LKey, r=RKey, u=UKey, d=DKey})
      end).

loop(#obj_state{pos={X, Y}, vel=Vel={Vx, Vy}, angle=Angle, ang_vel=Va}, Keys) ->
    receive
        {tick} ->
            gr_display:draw_me_as(render_ship(X, Y, Angle)),
            loop(#obj_state{pos={X + ?TICK_LENGTH*Vx, Y + ?TICK_LENGTH*Vy},
                            vel=Vel,
                            angle=Angle + ?TICK_LENGTH*Va,
                            ang_vel=Va},
                 Keys);
        {stop} ->
            gr_display:erase_me(),
            ok
    end.

render_ship(X, Y, Angle) ->
    C = math:cos(Angle),
    S = math:sin(Angle),
    [{X - 0.004*S, X + 0.004*C, X + 0.004*S, X - 0.004*C},
     {X, Y, X + 0.02*C, Y + 0.02*S}].
