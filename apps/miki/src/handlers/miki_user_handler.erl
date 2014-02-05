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
% OUTPUT : {"ok": Username} | {"error": Message}
do_post(Req, State) ->
  Req3 = case cowboy_req:body(Req) of
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
      ResponseBody = if
        Username =:= error -> {error, username_missing};
        Password =:= error -> {error, password_missing};
        Token =:= error -> {error, token_missing};
        true ->
          case miki_user:is_token_valid(Token) of 
            {ok, _} -> miki_user:add_user(Username, Password);
            E -> E
          end
      end,
      cowboy_req:set_resp_body(jsx:encode([ResponseBody]), Req2);
    {error, Reason} ->
      cowboy_req:set_resp_body(jsx:encode([{error, Reason}]), Req)
  end,
  {true, Req3, State}.

% GET /users/:token
% OUTPUT : {"ok": Username} | {"error": Message}
% GET /users
% OUTPUT : {"count": N}
do_get(Req, State) ->
  case cowboy_req:bindings(Req) of
    {[], _Req2} ->
      {jsx:encode([{error, invalid_token}]), Req, State};
    {Bindings, _Req2} ->
      Token = proplists:get_value(token, Bindings),
      {jsx:encode([miki_user:is_token_valid(Token)]), Req, State}
  end.

% PUT /users
% INPUT : {"username": Username, "password": Password, "action": "new_token"}
% OUTPUT : {"ok": Token} | {"error": Message}
% INPUT : {"username": Username, "password": Password, "new_password": NewPassword, "action": "new_password"}
% OUTPUT : {"ok": Username} | {"error": Message}
do_put(Req, State) ->
  Req3 = case cowboy_req:body(Req) of
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
      ResponseBody = if
        Username =:= error -> {error, username_missing};
        Password =:= error -> {error, password_missing};
        true ->
          case lists:keyfind(<<"action">>, 1, JsonData) of
            {<<"action">>, <<"new_token">>} -> miki_user:get_token(Username, Password);
            {<<"action">>, <<"new_password">>} -> 
              case lists:keyfind(<<"new_password">>, 1, JsonData) of
                {_, NewPassword} -> miki_user:update_password(Username, Password, NewPassword);
                false -> {error, missing_new_password}
              end;
            false -> {error, invalid_action}
          end
      end,
      cowboy_req:set_resp_body(jsx:encode([ResponseBody]), Req2);
    {error, Reason} ->
      cowboy_req:set_resp_body(jsx:encode([{error, Reason}]), Req)
  end,
  {true, Req3, State}.

% DELETE /users/:username/:token
delete_resource(Req, State) ->
  {true, Req, State}.
