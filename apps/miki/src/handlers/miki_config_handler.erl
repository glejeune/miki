-module(miki_config_handler).
-export([init/3, handle/2, terminate/3]).

init({tcp, http}, Req, _Opts) ->
  {ok, Req, undefined_state}.

handle(Req, State) ->
  case cowboy_req:method(Req) of
    {<<"POST">>, _} ->
      case miki_user:count() of
        0 -> 
          {Username, Password} = case cowboy_req:body_qs(Req) of
            {ok, X, _} -> 
              {
                case proplists:get_value(<<"user_login">>, X) of
                  undefined -> <<"">>;
                  Y -> Y
                end,
                case proplists:get_value(<<"user_password">>, X) of
                  undefined -> <<"">>;
                  Y -> Y
                end
              };
            _ -> {<<"">>, <<"">>}
          end,
          if 
            Username =:= <<"">> orelse Password =:= <<"">> -> ok;
            true -> miki_user:add_user(Username, Password)
          end;
        _ -> ok
      end;
    _ -> ok
  end,
  {ok, Reply} = case miki_user:count() of
    N when N > 0 ->
      cowboy_req:reply(
        302,
        [{<<"Location">>, <<"/">>}],
        <<"go home!">>,
        Req
      );
    0 ->
      Index = filename:join([code:priv_dir(miki), "config", "index.html"]),
      Body = list_to_bitstring(miki_utils:readlines(Index)),
      cowboy_req:reply(200, [], Body, Req)
  end,
  {ok, Reply, State}.

terminate(_, _, _) ->
  ok.
