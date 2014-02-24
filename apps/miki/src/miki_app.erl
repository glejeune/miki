-module(miki_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  Port = case application:get_env(miki, port) of
    {ok, P} -> P;
    _ -> 8080
  end,
  IP = case application:get_env(miki, ip) of
    {ok, I} -> 
      [A, B, C, D] = [list_to_integer(X) || X <- string:tokens(I, ".")],
      {A, B, C, D};
    _ -> {0, 0, 0, 0}
  end,
  MaxConn = case application:get_env(miki, max_conn) of 
    {ok, MC} -> MC;
    _ -> 100
  end,
  Routes      = routes(),
  Dispatch    = cowboy_router:compile(Routes),
  TransOpts   = [{port, Port}, {ip, IP}],
  ProtoOpts   = [ {env, [{dispatch, Dispatch}]} ],
  {ok, _}     = cowboy:start_http(http, MaxConn, TransOpts, ProtoOpts),
  lager:info("miki server started on port ~p (~p)", [Port, code:priv_dir(miki)]),
  miki_sup:start_link().

stop(_State) ->
  ok.

%% ===================================================================
%% Internal functions
%% ===================================================================
routes() ->
  [
    {'_', [
      {"/", miki_handler, []},
      {"/config", miki_config_handler, []},
      {"/pages/[...]", miki_page_handler, []},
      {"/users", miki_user_handler, []},
      {"/users/:token", miki_user_handler, []},
      {"/users/:username/:token", miki_user_handler, []},
      {"/static/[...]", cowboy_static, {priv_dir, miki, "static", [
        {mimetypes, cow_mimetypes, all}
      ]}}
    ]}
  ].

