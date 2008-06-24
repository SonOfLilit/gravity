-module(gravity).
-author('sonoflilit@gmail.com').

-compile(export_all).
-behaviour(application).

-include("sdl.hrl").
-include("sdl_events.hrl").
-include("sdl_video.hrl").
-include("sdl_keyboard.hrl").

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
    ScreenRef = sdl_video:setVideoMode(640, 480, 8, Flags),
    io:format("Video Driver Name: ~s\n", [sdl_video:videoDriverName()]),
    Screen = sdl_video:getSurface(ScreenRef),
    SpriteRef =  sdl_video:createRGBsurface(?SDL_ANYFORMAT, 32, 32, 16, 16#0f00, 16#00f0, 16#000f, 16#f000),
    sdl_video:fillRect(SpriteRef, null, sdl_video:mapRGB(Screen, 255, 255, 255)),
    Sprite    = sdl_video:getSurface(SpriteRef),
    {R1, R2, R3} = erlang:now(),
    random:seed(R1,R2,R3),
    Rects = create_rects(?NUM_SPRITES, 
			 Sprite#sdl_surface.w, 
			 Sprite#sdl_surface.h, 
			 Screen#sdl_surface.w, 
			 Screen#sdl_surface.h),    

    Background = sdl_video:mapRGB(Screen, 255, 0, 0),
    
    testblit(Screen, Sprite, Background),
    timer:sleep(500),

    sdl:quit(),
    Background.

testblit(Screen, Sprite, BG) ->
    Rect = #sdl_rect{x = 0, y = 0, 
		     w = Sprite#sdl_surface.w, 
		     h = Sprite#sdl_surface.h},

    true = sdl_video:fillRect(Screen, null, BG),
    
    CR = 
	case sdl_video:blitSurface(Sprite, null, Screen, Rect) of
	    {null, ClippedRect} -> io:format("Blit successfull~n"),
				   ClippedRect;
	    _  -> io:format("Blit failed~n"),
		  ?printError(),
		  exit({error, bad_blit})
	end,
    sdl_video:updateRects(Screen, [CR]),
    %% It migth have changed
    Sprite1 = sdl_video:getSurface(Sprite),
    SpriteFlags = Sprite1#sdl_surface.flags,    
    case ( SpriteFlags band ?SDL_HWACCEL) > 0 of 
	true ->
	    io:format("Sprite blit uses hardware acc~n");
	false ->
	    io:format("Sprite blit don't use HW acceleration~n")
    end,
    case ( SpriteFlags band ?SDL_RLEACCEL) > 0 of 
	true ->
	    io:format("Sprite blit uses RLE acc~n");
	false ->
	    io:format("Sprite blit don't use RLE acceleration~n")
    end,
    Sprite1.

create_rects(0, SpriteW, SpriteH, WinW, WinH) ->
    [];

create_rects(N, SpriteW, SpriteH, WinW, WinH) ->
    R = #sdl_rect{x = random:uniform(WinW),
		  y = random:uniform(WinH),
		  w = SpriteW, h = SpriteH},
    [R | create_rects(N-1, SpriteW, SpriteH, WinW, WinH)].

print_info(Screen, Sprite) ->
    ScreenFlags = Screen#sdl_surface.flags,    
    SpriteFlags = Sprite#sdl_surface.flags,    
    case ( ScreenFlags band ?SDL_HWSURFACE) > 0 of 
	true ->
	    io:format("Screen is in video memory~n");
	false ->
	    io:format("Screen is in system memory~n")
    end,
    case ( ScreenFlags band ?SDL_DOUBLEBUF) > 0 of 
	true ->
	    io:format("Screen is doubled buffered~n");
	_  ->
	    ignore
    end,
    case (SpriteFlags band ?SDL_HWSURFACE) > 0 of 
	true ->
	    io:format("Sprite is in video memory~n");
	false ->
	    io:format("Sprite is in system memory~n")
    end.
