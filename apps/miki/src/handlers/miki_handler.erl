-module(miki_handler).

-export([init/3]).
-export([content_types_provided/2]).
-export([
  miki_html/2,
  miki_json/2
]).

init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
  {[
      {<<"text/html">>, miki_html},
      {<<"application/json">>, miki_json}
  ], Req, State}.

miki_html(Req, State) ->
  Index = filename:join([code:priv_dir(miki), "static", "index.html"]),
  Body = list_to_bitstring(miki_utils:readlines(Index)),
  {Body, Req, State}.

miki_json(Req, State) ->
  Version = case application:get_key(miki, vsn) of
    {ok, Vsn} -> list_to_binary(Vsn);
    _ -> <<"unknow">>
  end,
  Body = jsx:encode([{name, <<"miki">>}, {version, Version}]),
  {Body, Req, State}.

