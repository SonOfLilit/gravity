-module(gravity).
-author('sonoflilit@gmail.com').

-compile(export_all).
-behaviour(application).

-include("sdl.hrl").
-include("sdl_events.hrl").
-include("sdl_video.hrl").
-include("sdl_keyboard.hrl").
-include("sdl_gfx.hrl").

-define(NUM_SPRITES, 100).
-define(MAX_SPEED,   1).

go() ->
    go([]).
go(Config) ->
    Server = sdl:init(?SDL_INIT_VIDEO),
    sdl_util:debug(1),

    Flags = 
	case lists:member(fullscreen, Config) of 
	    true ->
		?SDL_ANYFORMAT bor ?SDL_FULLSCREEN bor ?SDL_RESIZABLE;
	    _ -> 
		?SDL_ANYFORMAT bor ?SDL_RESIZABLE
	end,
    ScreenRef = sdl_video:setVideoMode(640, 480, 32, Flags),
    Screen = sdl_video:getSurface(ScreenRef),

    Background = sdl_video:mapRGB(Screen, 0, 0, 255),
    
    true = sdl_video:fillRect(Screen, null, Background),
    
    sdl_gfx:line(Screen, 0, 100, 640, 480, 255, 0, 0, 0),

    sdl_video:flip(Screen),
    sdl_video:updateRect(Screen, 0, 0, 0, 0),
    sdl_video:flip(Screen),
    sdl_video:updateRect(Screen, 0, 0, 0, 0),
    sdl_video:flip(Screen),
    sdl_video:updateRect(Screen, 0, 0, 0, 0),

    timer:sleep(5000),
    sdl:quit(),
    Background.
