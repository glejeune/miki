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
  from_json/2,
  to_txt/2
]).

init(_, _, _) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
  Page = case cowboy_req:path_info(Req) of
    {[], _} -> "index";
    {[Extra], _} -> binary_to_list(Extra)
  end,
  {ok, Req, {page, Page}}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) -> 
  {[
      {{<<"application">>, <<"json">>, '*'}, from_json}
  ], Req, State}.

accept_event_body(Req, State) ->
  {true, Req, State}.

content_types_provided(Req, State) ->
  {[
      {{<<"*">>, <<"*">>, '*'}, to_txt}
  ], Req, State}.

% POST /page
% INPUT : {"title": Title, "content", Content}
% OUTPUT : {"ok", Title} | {"error": Message}
from_json(Req, State) ->
  {ok, Data, _} = cowboy_req:body(Req),
  JSON = jsx:decode(Data),
  {<<"title">>, Filename} = lists:keyfind(<<"title">>, 1, JSON),
  {<<"content">>, Content} = lists:keyfind(<<"content">>, 1, JSON),
  File = filename:join([code:priv_dir(miki), "pages", binary_to_list(Filename)]),
  file:write_file(list_to_binary(File), Content),
  lager:info("Save page to ~p", [File]),
  Req2 = cowboy_req:set_resp_body(jsx:encode([{ok, Filename}]), Req),
  {true, Req2, State}.

% GET /page/:page
to_txt(Req, {page, Page} = State) ->
  File = filename:join([code:priv_dir(miki), "pages", Page]),
  lager:info("Send page ~p", [File]),
  case miki_utils:readlines(File) of
    error -> 
      {ok, Req2} = cowboy_req:chunked_reply(404, Req),
      {halt, Req2, State};
    Data -> 
      Req2 = cowboy_req:set_resp_header("Content-Type", "text/plain", Req),
      Body = list_to_bitstring(Data),
      {Body, Req2, State}
  end.
