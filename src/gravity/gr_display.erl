%%%-------------------------------------------------------------------
%%% File    : gr_display.erl
%%% Author  :  <son@moaddib>
%%% Description : Vector display
%%%
%%% Created : 26 Jun 2008 by  <son@moaddib>
%%%-------------------------------------------------------------------
-module(gr_display).
-author('sonoflilit@gmail.com').

-behaviour(gen_server).

-include("sdl.hrl").
-include("sdl_events.hrl").
-include("sdl_video.hrl").
-include("sdl_keyboard.hrl").
-include("sdl_gfx.hrl").

-record(state, {screen, objects}).

%% API
-export([start/1,
         start/2,
         stop/0,
         start_link/1,
         start_link/2,
         draw_frame/1,
         draw_me_as/1,
         erase_me/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start(Size (, Config)) -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start(Size) ->
    gen_server:start({local, ?MODULE}, ?MODULE, [Size], []).

start(Size, Config) ->
    gen_server:start({local, ?MODULE}, ?MODULE, [Size, Config], []).

%%--------------------------------------------------------------------
%% Function: start_link(Size (, Config)) -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server under supervision
%%--------------------------------------------------------------------
start_link(Size) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Size], []).

start_link(Size, Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Size, Config], []).

%%--------------------------------------------------------------------
%% Function: stop() -> ok
%% Description: Stops the server
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast(?MODULE, stop).

%%--------------------------------------------------------------------
%% Func: draw_frame([{X1, Y1, X2, Y2}, ...]) -> ok
%% Description: Clear display, draw the segments in the list
%% plus process representations and update display
%%--------------------------------------------------------------------
draw_frame(List) ->
    gen_server:cast(?MODULE, {draw_frame, List}).

%%--------------------------------------------------------------------
%% Func: draw_me_as([{X1, Y1, X2, Y2}, ...]) -> ok
%% Description: Set process to be drawn as given list of segments
%%--------------------------------------------------------------------
draw_me_as(List) ->
    gen_server:cast(?MODULE, {draw_me_as, List, self()}).

%%--------------------------------------------------------------------
%% Func: erase_me() -> ok
%% Description: Set process to not be drawn
%%--------------------------------------------------------------------
erase_me() ->
    gen_server:cast(?MODULE, {erase_me, self()}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init([Size | Config]) -> {ok, State} |
%%                                    {ok, State, Timeout} |
%%                                    ignore               |
%%                                    {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Size | Config]) ->
    _Server = sdl:init(?SDL_INIT_VIDEO),
%    sdl_util:debug(1),

    Flags = 
	case lists:member(fullscreen, Config) of 
	    true ->
		?SDL_ANYFORMAT bor ?SDL_FULLSCREEN bor ?SDL_RESIZABLE;
	    _ -> 
		?SDL_ANYFORMAT bor ?SDL_RESIZABLE
	end,
    ScreenRef = sdl_video:setVideoMode(Size, Size, 32, Flags),
    Screen = sdl_video:getSurface(ScreenRef),
    {ok, #state{screen=Screen, objects=dict:new()}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(Request, From, State) ->
    error_logger:error_msg("gr_display:handle_call(~w, ~w, ~w)~n",
                           [Request, From, State]).

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({draw_frame, List}, State = #state{screen=Screen, objects=Objects})
  when list(List) ->
    do_draw_frame(Screen, Objects, List),
    {noreply, State};
handle_cast({draw_me_as, List, From}, State) ->
    ConvertedList = convert_lines((State#state.screen)#sdl_surface.w, List),
    Objects = dict:store(From, ConvertedList, State#state.objects),
    {noreply, State#state{objects=Objects}};
handle_cast({erase_me, From}, State) ->
    Objects = dict:erase(From, State#state.objects),
    {noreply, State#state{objects=Objects}};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Msg, State) ->
    error_logger:error_msg("gr_display:handle_cast(~w, ~w)~n",
                           [Msg, State]).

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    error_logger:info_msg("gr_display:handle_info(~w, ~w)~n", [Info, State]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    sdl:quit(),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    sdl:quit(),
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Func: do_draw_frame(Screen, Objects, [{X1, Y1, X2, Y2}, ...]) -> ok
%% Description: Clear display, draw the segments in the list
%% plus process representations and update display
%%--------------------------------------------------------------------
do_draw_frame(Screen, Objects, Lines)
  when list(Lines) ->
    Background = sdl_video:mapRGB(Screen, 0, 0, 0),
    true = sdl_video:fillRect(Screen, null, Background),
    
    IntLines = convert_lines(Screen#sdl_surface.w, Lines),
    draw_lines(Screen, IntLines),

    dict:fold(
      fun(_Key, Val, _Acc) ->
              draw_lines(Screen, Val)
      end,
      ok,
      Objects),

    sdl_video:updateRect(Screen, 0, 0, 0, 0),
    sdl_video:flip(Screen),
    ok.

%%--------------------------------------------------------------------
%% Func: convert_lines(Size, [{X1, Y1, X2, Y2}, ...]) -> Lines
%% Description: Convert lines from [0.0...1.0]^2 space to [0...Size-1]^2 space
%%--------------------------------------------------------------------
convert_lines(Size, Lines) ->
    Size,
    lists:map(
      fun({A, B, C, D}) ->
              {round(Size*A), round(Size*B),
               round(Size*C), round(Size*D)}
      end,
      Lines).

%%--------------------------------------------------------------------
%% Func: draw_lines(Screen, [{X1, Y1, X2, Y2}, ...]) -> ok
%% Description: Draw the segments in a list
%%--------------------------------------------------------------------
draw_lines(_Screen, []) ->
    ok;
draw_lines(Screen, _List = [{X1, Y1, X2, Y2} | Rest]) ->
%    error_logger:info_msg("~w:draw_lines(~w, ~p)~n", [?MODULE, Screen, _List]),
    sdl_gfx:line(Screen, X1, Y1, X2, Y2, 0, 200, 0, 255),
    draw_lines(Screen, Rest).
