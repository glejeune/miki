-module(miki_index_handler).
-export([init/3, handle/2, terminate/3]).

init({tcp, http}, Req, _Opts) ->
  {ok, Req, undefined_state}.

handle(Req, State) ->
  Index = case application:get_env(miki, index) of
    {ok, I} -> list_to_binary("/page/" ++ I);
    _ -> <<"/page">>
  end,
  {ok, Reply} = cowboy_req:reply(200, [], Index, Req),
  {ok, Reply, State}.

terminate(_, _, _) ->
  ok.
