-module(auth_middleware).
-behaviour(cowboy_middleware).

%% API
-export([
  execute/2
]).

execute(Req0, Env) ->
  case cowboy_req:parse_header(<<"authorization">>, Req0) of
    {digest, A} ->
      %%io:format("KEY ~p~n", [A]),
      Req1 = cowboy_req:delete_resp_header(<<"authorization">>, Req0),
      {ok, Req1, Env};
    _ ->
      Req1 = cowboy_req:reply(401, #{
        <<"www-authenticate">> => <<"Digest realm=\"localhost\", nonce=\"test\", qop=\"auth\"">>
      }, Req0),
      {stop, Req1}
  end.