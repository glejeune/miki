-module(miki_user).
-behaviour(gen_server).
-include("../include/miki.hrl").
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([
  add_user/2,
  update_password/3,
  get_token/2,
  is_token_valid/1,
  count/0,
  all_users/0,
  delete_user/1
]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_user(Username, Password) ->
  gen_server:call(?SERVER, {add_user, Username, Password}).

update_password(Username, OldPassword, NewPassword) ->
  gen_server:call(?SERVER, {update_password, Username, OldPassword, NewPassword}).

get_token(Username, Password) ->
  gen_server:call(?SERVER, {get_token, Username, Password}).

is_token_valid(Token) ->
  gen_server:call(?SERVER, {is_token_valid, Token}).

count() ->
  gen_server:call(?SERVER, {count}).

all_users() ->
  gen_server:call(?SERVER, {all_users}).

delete_user(Username) ->
  gen_server:call(?SERVER, {delete_user, Username}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
  Tokens = ets:new('tokens', []),
  File = filename:join([code:priv_dir(miki), "db", "users.db"]),
  Users = case ets:file2tab(File) of
    {ok, Tab} -> Tab;
    _ -> ets:new('users', [])
  end,
  erlang:send_after(60000, self(), trigger),
  {ok, #miki{tokens = Tokens, users = Users}}.

handle_call({add_user, Username, Password}, _From, #miki{users = Users} = State) ->
  Result = case find_user(Users, Username, Password) of
    user_not_found -> 
      ets:insert(Users, {Username, Password}),
      File = filename:join([code:priv_dir(miki), "db", "users.db"]),
      ets:tab2file(Users, File),
      {ok, Username};
    _ -> 
      {error, user_exist}
  end, 
  {reply, Result, State};
handle_call({update_password, Username, OldPassword, NewPassword}, _From, #miki{users = Users} = State) ->
  Result = case find_user(Users, Username, OldPassword) of
    ok -> 
      ets:delete_object(Users, {Username, OldPassword}),
      ets:insert(Users, {Username, NewPassword}),
      File = filename:join([code:priv_dir(miki), "db", "users.db"]),
      ets:tab2file(Users, File),
      {ok, Username};
    Err -> 
      {error, Err}
  end,
  {reply, Result, State};
handle_call({delete_user, Username}, _From, #miki{users = Users} = State) ->
  Result = case find_user(Users, Username) of
    user_not_found -> user_not_found;
    Password -> 
      ets:delete_object(Users, {Username, Password}),
      File = filename:join([code:priv_dir(miki), "db", "users.db"]),
      ets:tab2file(Users, File),
      ok
  end,
  {reply, Result, State};
handle_call({get_token, Username, Password}, _From, #miki{users = Users, tokens = Tokens} = State) ->
  Result = case find_user(Users, Username, Password) of
    ok -> 
      UUID = uuid:generate(),
      Expire = now_sec() + 86400,
      ets:insert(Tokens, {UUID, Expire, Username}),
      {ok, UUID};
    Err -> 
      {error, Err}
  end,
  {reply, Result, State};
handle_call({is_token_valid, Token}, _From, #miki{tokens = Tokens} = State) ->
  {reply, find_token(Tokens, Token), State};
handle_call({count}, _From, #miki{users = Users} = State) ->
  {reply, ets:info(Users, size), State};
handle_call({all_users}, _From, #miki{users = Users} = State) ->
  {reply, ets:tab2list(Users), State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, #miki{tokens = Tokens} = State) ->
  io:format("remove old tokens!!!~n"),
  Nowsec = now_sec(),
  lists:foreach(fun({_, Expire, _} = E) ->
        if
          Nowsec < Expire -> ok;
          true -> ets:delete_object(Tokens, E)
        end
    end, ets:tab2list(Tokens)),
  % TODO remove "old" tokens
  erlang:send_after(60000, self(), trigger),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

find_user(Users, Username, Password) ->
  case ets:match(Users, {Username, '$1'}) of
    [[Password]] -> ok;
    [_] -> invalid_password;
    [] -> user_not_found
  end.

find_user(Users, Username) ->
  case ets:match(Users, {Username, '$1'}) of
    [[Password]] -> Password;
    [] -> user_not_found
  end.

find_token(Tokens, Token) ->
  case ets:match(Tokens, {Token, '$1', '$2'}) of
    [[Expire, Username]] -> 
      Nowsec = now_sec(),
      if 
        Nowsec < Expire -> {ok, Username};
        true -> {error, expired_token}
      end;
    _ -> 
      {error, invalid_token}
  end.

now_sec() ->
  Now = now(),
  Nowish = calendar:now_to_universal_time(Now),
  calendar:datetime_to_gregorian_seconds(Nowish).

