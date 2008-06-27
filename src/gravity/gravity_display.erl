%%%-------------------------------------------------------------------
%%% File    : gravity_display.erl
%%% Author  :  <son@moaddib>
%%% Description : 
%%%
%%% Created : 26 Jun 2008 by  <son@moaddib>
%%%-------------------------------------------------------------------
-module(gravity_display).
-author('sonoflilit@gmail.com').

-behaviour(gen_server).

-include("sdl.hrl").
-include("sdl_events.hrl").
-include("sdl_video.hrl").
-include("sdl_keyboard.hrl").
-include("sdl_gfx.hrl").

-record(state, {screen}).

%% API
-export([start_link/1,
         start_link/2,
         draw_frame/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link(Size (, Config)) -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Size) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Size], []).

start_link(Size, Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Size, Config], []).

%%--------------------------------------------------------------------
%% Func: draw_frame(Screen, [{X1, Y1, X2, Y2}, ...]) -> ok
%% Description: Clear display, draw the lines in the list and update display
%%--------------------------------------------------------------------
draw_frame(List) ->
    gen_server:call(gravity_display, {draw_frame, List}).

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
    sdl_util:debug(1),

    Flags = 
	case lists:member(fullscreen, Config) of 
	    true ->
		?SDL_ANYFORMAT bor ?SDL_FULLSCREEN bor ?SDL_RESIZABLE;
	    _ -> 
		?SDL_ANYFORMAT bor ?SDL_RESIZABLE
	end,
    ScreenRef = sdl_video:setVideoMode(Size, Size, 32, Flags),
    Screen = sdl_video:getSurface(ScreenRef),
    {ok, #state{screen=Screen}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(Request, _From, State) ->
    case Request of
        {draw_frame, List} when list(List) ->
            Reply = do_draw_frame(State#state.screen, List)
    end,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    case Msg of
        {draw_frame, List} when list(List) ->
            do_draw_frame(State#state.screen, List)
    end,
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    error_logger:info_msg("gravity_display:handle_info(~w, ~w)~n", Info, State),
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
%% Func: do_draw_frame(Screen, [{X1, Y1, X2, Y2}, ...]) -> ok
%% Description: Clear display, draw the lines in the list and update display
%%--------------------------------------------------------------------
do_draw_frame(Screen, Lines)
  when list(Lines) ->
    Background = sdl_video:mapRGB(Screen, 0, 0, 0),
    true = sdl_video:fillRect(Screen, null, Background),
    
    % convert from [0.0 .. 1.0] coordinate space to [0 .. W-1] space
    Size = Screen#sdl_surface.w,
    IntLines = lists:map(
                 fun({A, B, C, D}) ->
                         {round(Size*A), round(Size*B),
                          round(Size*C), round(Size*D)}
                 end,
                 Lines),
    draw_lines(Screen, IntLines),

    sdl_video:updateRect(Screen, 0, 0, 0, 0),
    sdl_video:flip(Screen),
    ok.

%%--------------------------------------------------------------------
%% Func: draw_lines(Screen, [{X1, Y1, X2, Y2}, ...]) -> ok
%% Description: Draw the lines in a list
%%--------------------------------------------------------------------
draw_lines(_Screen, []) ->
    ok;
draw_lines(Screen, List = [{X1, Y1, X2, Y2} | Rest]) ->
    error_logger:info_msg("~w:draw_lines(~w, ~p)~n", [?MODULE, Screen, List]),
    sdl_gfx:line(Screen, X1, Y1, X2, Y2, 0, 255, 0, 255),
    draw_lines(Screen, Rest).
