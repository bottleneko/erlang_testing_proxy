-module(auth_middleware).
-behaviour(cowboy_middleware).

%% API
-export([
  execute/2
]).

-spec execute(Req, Env) ->
  {ok, Req, Env} | {stop, Req} when
  Req :: cowboy_req:req(),
  Env :: cowboy_middleware:env().
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

-spec unauthorized_reply(Req) ->
  {stop, Req} when
  Req :: cowboy_req:req().
unauthorized_reply(Req0) ->
  Req1 = cowboy_req:reply(407, #{
    <<"proxy-authenticate">> => <<"Digest realm=\"localhost\", nonce=\"test\", qop=\"auth\"">>
  }, Req0),
  {stop, Req1}.

-spec auth_response(RequestProperties :: map(), Password :: binary(), Method :: binary()) -> Response :: binary().
auth_response(RequestProperties, Password, Method) ->
  #{<<"username">>  := Username,
    <<"realm">>     := Realm,
    <<"uri">>       := URI,
    <<"nonce">>     := Nonce,
    <<"nc">>        := NonceCounter,
    <<"cnonce">>    := CNonce,
    <<"qop">>       := Qop} = RequestProperties,
  HA1 = ha1(Username, Realm, Password),
  HA2 = ha2(Method, URI),
  md5(list_to_binary(lists:flatten(lists:join(":", [HA1, Nonce, NonceCounter, CNonce, Qop, HA2])))).

-spec ha1(Username :: binary(), Realm :: binary(), Password :: binary()) ->
  HA1 :: binary().
ha1(Username, Realm, Password) ->
  md5(<<Username/binary, <<":">>/binary, Realm/binary, <<":">>/binary, Password/binary>>).

-spec ha2(Method :: binary(), URI :: binary()) ->
  HA2 :: binary().
ha2(Method, URI) ->
  md5(<<Method/binary, <<":">>/binary , URI/binary>>).

-spec parse_unauthorized(Message :: binary()) -> Parsed :: map().
parse_unauthorized(<<"Digest ", Message/binary>>) ->
  Splitted = re:split(Message, <<", ">>),
  RawProperties = lists:map(fun(Elem) -> list_to_tuple(re:split(Elem, <<"=">>)) end, Splitted),
  %% @TODO: uri may be have eq symbol
  RawMapProperties = maps:from_list(RawProperties),
  maps:map(
    fun(_Key, Value) ->
      binary:replace(Value, <<"\"">>, <<"">>, [global])
    end, RawMapProperties).

-spec md5(S :: binary()) -> Hash :: binary().
md5(S) ->
  list_to_binary(lists:flatten([io_lib:format("~2.16.0b",[N]) || <<N>> <= erlang:md5(S)])).