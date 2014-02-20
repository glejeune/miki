-module(miki_user_handler).

-export([
  init/3,
  allowed_methods/2,
  content_types_accepted/2,
  content_types_provided/2,
  do_put_post/2
  ]).
-export([
  do_post/2,
  do_put/2,
  do_get/2,
  delete_resource/2
]).

init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>], Req, State}.

content_types_accepted(Req, State) -> 
  {[
      {{<<"application">>, <<"json">>, '*'}, do_put_post}
  ], Req, State}.

do_put_post(Req, State) ->
  case cowboy_req:method(Req) of
    {<<"POST">>, Req2} -> do_post(Req2, State);
    {<<"PUT">>, Req2} -> do_put(Req2, State)
  end.

content_types_provided(Req, State) ->
  {[
      {{<<"application">>, <<"json">>, '*'}, do_get}
  ], Req, State}.


% POST /users
% INPUT : {"username": Username, "password": Password, "token": MyToken}
% OUTPUT : {"ok": Username}
do_post(Req, State) ->
  case cowboy_req:body(Req) of
    {ok, Data, Req2} -> 
      JsonData = jsx:decode(Data),
      Username = case lists:keyfind(<<"username">>, 1, JsonData) of
        {<<"username">>, U} -> U;
        false -> error
      end,
      Password = case lists:keyfind(<<"password">>, 1, JsonData) of
        {<<"password">>, P} -> P;
        false -> error
      end,
      Token = case lists:keyfind(<<"token">>, 1, JsonData) of
        {<<"token">>, T} -> T;
        false -> error
      end,
      if
        Username =:= error orelse Password =:= error -> 
          {ok, Req3} = cowboy_req:chunked_reply(400, Req2),
          {halt, Req3, State};
        Token =:= error -> 
          {ok, Req4} = cowboy_req:chunked_reply(401, Req2),
          {halt, Req4, State};
        true ->
          case miki_user:is_token_valid(Token) of 
            {ok, _} -> 
              ResponseBody = miki_user:add_user(Username, Password),
              Req5 = cowboy_req:set_resp_body(jsx:encode([ResponseBody]), Req2),
              {true, Req5, State};
            _ -> 
              {ok, Req7} = cowboy_req:chunked_reply(401, Req2),
              {halt, Req7, State}
          end
      end;
    {error, _} ->
      {ok, Req8} = cowboy_req:chunked_reply(400, Req),
      {halt, Req8, State}
  end.

% GET /users/:token
% OUTPUT : {"ok": Username}
% GET /users
% OUTPUT : [User1, User2]
do_get(Req, State) ->
  case cowboy_req:bindings(Req) of
    {[], _Req2} ->
      Users = lists:foldl(fun({Username, _}, Acc) ->
              Acc ++ [Username]
          end, [], miki_user:all_users()),
      {jsx:encode(Users), Req, State};
    {Bindings, Req2} ->
      Token = proplists:get_value(token, Bindings),
      case miki_user:is_token_valid(Token) of
        {error, _} -> 
          {ok, Req3} = cowboy_req:chunked_reply(404, Req2),
          {halt, Req3, State};
        OK -> 
          {jsx:encode([OK]), Req, State}
      end
  end.

% PUT /users
% INPUT : {"username": Username, "password": Password, "action": "new_token"}
% OUTPUT : {"ok": Token}
% INPUT : {"username": Username, "password": Password, "new_password": NewPassword, "action": "new_password"}
% OUTPUT : {"ok": Username}
do_put(Req, State) ->
  case cowboy_req:body(Req) of
    {ok, Data, Req2} -> 
      JsonData = jsx:decode(Data),
      Username = case lists:keyfind(<<"username">>, 1, JsonData) of
        {<<"username">>, U} -> U;
        false -> error
      end,
      Password = case lists:keyfind(<<"password">>, 1, JsonData) of
        {<<"password">>, P} -> P;
        false -> error
      end,
      if
        Username =:= error orelse Password =:= error -> 
          {ok, Req4} = cowboy_req:chunked_reply(401, Req2),
          {halt, Req4, State};
        true ->
          case lists:keyfind(<<"action">>, 1, JsonData) of
            {<<"action">>, <<"new_token">>} -> 
              case miki_user:get_token(Username, Password) of
                {ok, Token} ->
                  Req5 = cowboy_req:set_resp_body(jsx:encode([{ok, Token}]), Req2),
                  {true, Req5, State};
                {error, _} ->
                  {ok, Req11} = cowboy_req:chunked_reply(401, Req2),
                  {halt, Req11, State}
              end;
            {<<"action">>, <<"new_password">>} -> 
              case lists:keyfind(<<"new_password">>, 1, JsonData) of
                {_, NewPassword} -> 
                  case miki_user:update_password(Username, Password, NewPassword) of
                    {ok, Username} ->
                      Req6 = cowboy_req:set_resp_body(jsx:encode([{ok, Username}]), Req2),
                      {true, Req6, State};
                    {error, _} ->
                      {ok, Req7} = cowboy_req:chunked_reply(401, Req2),
                      {halt, Req7, State}
                  end;
                false -> 
                  {ok, Req8} = cowboy_req:chunked_reply(400, Req2),
                  {halt, Req8, State}
              end;
            false -> 
              {ok, Req9} = cowboy_req:chunked_reply(400, Req2),
              {halt, Req9, State}
          end
      end;
    {error, _} ->
      {ok, Req10} = cowboy_req:chunked_reply(400, Req),
      {halt, Req10, State}
  end.

% DELETE /users/:username/:token
delete_resource(Req, State) ->
  case cowboy_req:bindings(Req) of
    {[], Req2} ->
      {ok, Req3} = cowboy_req:chunked_reply(404, Req2),
      {halt, Req3, State};
    {Bindings, Req2} ->
      Username = proplists:get_value(username, Bindings),
      Token = proplists:get_value(token, Bindings),
      case miki_user:is_token_valid(Token) of
        {error, _} -> 
          {ok, Req3} = cowboy_req:chunked_reply(401, Req2),
          {halt, Req3, State};
        {ok, TokenUser} -> 
          if 
            TokenUser =/= Username ->
              case miki_user:delete_user(Username) of
                ok ->
                  {ok, Req3} = cowboy_req:chunked_reply(204, Req2),
                  {halt, Req3, State};
                _ ->
                  {ok, Req3} = cowboy_req:chunked_reply(404, Req2),
                  {halt, Req3, State}
              end;
            true ->
              {ok, Req3} = cowboy_req:chunked_reply(403, Req2),
              {halt, Req3, State}
          end
      end
  end.
