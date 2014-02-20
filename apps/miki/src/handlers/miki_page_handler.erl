-module(miki_page_handler).

-export([
  init/3,
  rest_init/2,
  allowed_methods/2,
  content_types_accepted/2,
  accept_event_body/2,
  content_types_provided/2
  ]).
-export([
  do_post/2,
  do_get/2,
  delete_resource/2
]).

init(_, _, _) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
  Page = case cowboy_req:path_info(Req) of
    {[P], _} -> {page, binary_to_list(P)};
    {[P, T], _} -> [{page, binary_to_list(P)}, {token, T}];
    _ -> list
  end,
  {ok, Req, {page, Page}}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>, <<"DELETE">>], Req, State}.

content_types_accepted(Req, State) -> 
  {[
      {{<<"application">>, <<"json">>, '*'}, do_post}
  ], Req, State}.

accept_event_body(Req, State) ->
  {true, Req, State}.

content_types_provided(Req, State) ->
  {[
      {{<<"*">>, <<"*">>, '*'}, do_get}
  ], Req, State}.

% POST /page
% INPUT : {"title": Title, "content": Content, "token": Token}
% OUTPUT : {"ok", Title}
do_post(Req, State) ->
  {ok, Data, _} = cowboy_req:body(Req),
  JSON = jsx:decode(Data),
  {<<"title">>, Filename} = lists:keyfind(<<"title">>, 1, JSON),
  {<<"content">>, Content} = lists:keyfind(<<"content">>, 1, JSON),
  {<<"token">>, Token} = lists:keyfind(<<"token">>, 1, JSON),
  case miki_user:is_token_valid(Token) of
    {ok, _} -> 
      File = filename:join([code:priv_dir(miki), "pages", binary_to_list(Filename)]),
      file:write_file(list_to_binary(File), Content),
      lager:info("Save page to ~p", [File]),
      Req2 = cowboy_req:set_resp_body(jsx:encode([{ok, Filename}]), Req),
      {true, Req2, State};
    {error, _} -> 
      {ok, Req4} = cowboy_req:chunked_reply(401, Req),
      {halt, Req4, State}
  end.

% GET /page/:page
do_get(Req, {page, PageRequest} = State) ->
  case PageRequest of
    {page, Page} ->
      File = filename:join([code:priv_dir(miki), "pages", Page]),
      lager:info("Send page ~p", [File]),
      case miki_utils:readlines(File) of
        error -> 
          {ok, Req2} = cowboy_req:chunked_reply(404, Req),
          {halt, Req2, State};
        Data -> 
          Req3 = cowboy_req:set_resp_header("Content-Type", "text/plain", Req),
          Body = list_to_bitstring(Data),
          {Body, Req3, State}
      end;
    list ->
      Dir = filename:join([code:priv_dir(miki), "pages"]),
      FileList = case file:list_dir(Dir) of
        {ok, Filenames} -> lists:map(fun(E) -> list_to_binary(E) end, Filenames);
        _ -> []
      end,
      Req4 = cowboy_req:set_resp_header("Content-Type", "application/json", Req),
      Body = jsx:encode(FileList),
      {Body, Req4, State}
  end.

% DELETE /pages/:page/:token
delete_resource(Req, {page, PageRequest} = State) ->
  lager:info("PageRequest = ~p", [PageRequest]),
  if 
    is_list(PageRequest) -> 
      {token, Token} = lists:keyfind(token, 1, PageRequest),
      {page, Page} = lists:keyfind(page, 1, PageRequest),
      case miki_user:is_token_valid(Token) of
        {ok, _} -> 
          File = filename:join([code:priv_dir(miki), "pages", Page]),
          lager:info("Delete page ~p", [File]),
          case file:delete(File) of
            ok ->
              {ok, Req2} = cowboy_req:chunked_reply(204, Req),
              {halt, Req2, State};
            {error, enoent} ->
              {ok, Req2} = cowboy_req:chunked_reply(404, Req),
              {halt, Req2, State};
            _ -> 
              {ok, Req2} = cowboy_req:chunked_reply(403, Req),
              {halt, Req2, State}
          end;
        {error, _} -> 
          {ok, Req3} = cowboy_req:chunked_reply(401, Req),
          {halt, Req3, State}
      end;
    is_tuple(PageRequest) ->
      {ok, Req3} = cowboy_req:chunked_reply(401, Req),
      {halt, Req3, State};
    true ->
      {ok, Req4} = cowboy_req:chunked_reply(400, Req),
      {halt, Req4, State}
  end.

