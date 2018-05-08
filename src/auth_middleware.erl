-module(auth_middleware).
-behaviour(cowboy_middleware).

%% API
-export([
  execute/2
]).

execute(Req0 = #{headers := Headers}, Env) ->
  case maps:get(<<"proxy-authorization">>, Headers, undefined) of
    undefined ->
      unauthorized_reply(Req0);
    Authorization ->
      RequestProperties = parse_unauthorized(Authorization),
      #{<<"response">> := Response} = RequestProperties,
      Password = <<"test">>,
      Method = cowboy_req:method(Req0),
      ServerResponse = auth_response(RequestProperties, Password, Method),
      case ServerResponse of
        Response ->
          io:format("SUCCESS~n"),
          Req1 = cowboy_req:delete_resp_header(<<"proxy-authorization">>, Req0),
          {ok, Req1, Env};
        _ ->
          io:format("FAIL~n"),
          unauthorized_reply(Req0)
      end
  end.

unauthorized_reply(Req0) ->
  Req1 = cowboy_req:reply(407, #{
    <<"proxy-authenticate">> => <<"Digest realm=\"localhost\", nonce=\"test\", qop=\"auth\"">>
  }, Req0),
  {stop, Req1}.

auth_response(RequestProperties, Password, Method) ->
  #{<<"username">>  := Username,
    <<"realm">>     := Realm,
    <<"uri">>       := Route,
    <<"nonce">>     := Nonce,
    <<"nc">>        := NonceCounter,
    <<"cnonce">>    := CNonce,
    <<"qop">>       := Qop} = RequestProperties,
  HA1 = ha1(Username, Realm, Password),
  HA2 = ha2(Method, Route),
  md5(lists:flatten(lists:join(":", [HA1, Nonce, NonceCounter, CNonce, Qop, HA2]))).

ha1(Username, Realm, Password) ->
  md5(<<Username/binary, <<":">>/binary, Realm/binary, <<":">>/binary, Password/binary>>).

ha2(Method, Route) ->
  md5(<<Method/binary, <<":">>/binary , Route/binary>>).

-spec parse_unauthorized(Message) -> Parsed when
  Message :: binary(),
  Parsed  :: map().
parse_unauthorized(<<"Digest ", Message/binary>>) ->
  Splitted = re:split(Message, <<", ">>),
  RawProperties = lists:map(fun(Elem) -> list_to_tuple(re:split(Elem, <<"=">>)) end, Splitted),
  RawMapProperties = maps:from_list(RawProperties),
  maps:map(
    fun(_Key, Value) ->
      binary:replace(Value, <<"\"">>, <<"">>, [global])
    end, RawMapProperties).

md5(S) ->
  list_to_binary(lists:flatten([io_lib:format("~2.16.0b",[N]) || <<N>> <= erlang:md5(S)])).