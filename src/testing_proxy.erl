-module(testing_proxy).

-export([
  init/2,
  is_authorized/2,
  resource_exists/2,
  previously_existed/2,
  moved_permanently/2,
  moved_temporarily/2,
  to_html/2
]).

-record(state, {
  response
}).


init(Req0, _State) ->
  Uri = binary_to_list(cowboy_req:host(Req0)),
  Path = cowboy_req:path(Req0),
  Qs = cowboy_req:qs(Req0),
  PathWithQs = <<Path/binary, <<"?">>/binary, Qs/binary>>,
  Headers = lists:keydelete(<<"authorization">>, 1, maps:to_list(cowboy_req:headers(Req0))),
  %%io:format("HEADERS ~p~n", [Headers]),
  Method = cowboy_req:method(Req0),
  {ok, ConnPid} = gun:open(Uri, 80, #{connect_timeout => infinity, retry => 0}),
  StreamRef = gun:request(ConnPid, Method, PathWithQs, Headers),
  {cowboy_rest, Req0, #state{
    response = receive_gun_response(ConnPid, StreamRef)
  }}.

%% TODO: make real authorization
is_authorized(Req0, State) ->
  case cowboy_req:parse_header(<<"authorization">>, Req0) of
    {digest, A} ->
      %%io:format("KEY ~p~n", [A]),
      Req1 = cowboy_req:delete_resp_header(<<"authorization">>, Req0),
      {true, Req1, State};
    _ ->
      {{false, <<"Digest realm=\"localhost\", nonce=\"fucku\", qop=\"auth\"">>}, Req0, State}
  end.

resource_exists(Req0, State) ->
  #state{response = #{status := Status}} = State,
  io:format("RE STATUS ~p~n", [Status]),
  case Status of
    412 ->
      {true, Req0, State};
    404 ->
      {false, Req0, State};
    301 ->
      {false, Req0, State};
    307 ->
      {false, Req0, State};
    _ ->
      {false, Req0, State}
  end.

previously_existed(Req0, State) ->
  #state{response = #{status := Status}} = State,
  case Status of
    404 ->
      {false, Req0, State};
    301 ->
      {true, Req0, State};
    307 ->
      {true, Req0, State};
    _ ->
      {false, Req0, State}
  end.

moved_permanently(Req0, State) ->
  #state{response = #{status := Status, headers := Headers}} = State,
  case Status of
    301 ->
      Location = proplists:get_value(<<"location">>, Headers),
      {{true, Location}, Req0, State};
    307 ->
      {false, Req0, State};
    _ ->
      {false, Req0, State}
  end.

moved_temporarily(Req0, State) ->
  #state{response = #{status := Status, headers := Headers}} = State,
  case Status of
    307 ->
      Location = proplists:get_value(<<"location">>, Headers),
      {{true, Location}, Req0, State};
    _ ->
      {false, Req0, State}
  end.

to_html(Req0, State) ->
  #{headers := ResponseHeaders, content := Received} = State#state.response,
  Req1 = cowboy_req:set_resp_headers(maps:from_list(ResponseHeaders), Req0),
  case Received of
    no_data ->
      {<<"">>, Req1, State};
    Data ->
      {Data, Req1, State}
  end.

receive_gun_response(ConnPid, StreamRef) ->
  receive
    {gun_response, ConnPid, StreamRef, fin, Status, Headers} ->
      #{status => Status, headers => Headers, content => no_data};
    {gun_response, ConnPid, StreamRef, nofin, Status, Headers} ->
      receive_data_loop(ConnPid, StreamRef, #{status => Status, headers => Headers, content => <<"">>});
    {'DOWN', _, process, ConnPid, Reason} ->
      error_logger:error_msg("Oops!"),
      exit(Reason)
  after 50000 ->
    exit(timeout)
  end.

receive_data_loop(Pid, Ref, Response = #{content := Acc}) ->
  receive
    {gun_up, Pid, _} ->
      receive_data_loop(Pid, Ref, Response);
    {gun_data, Pid, Ref, nofin, Data} ->
      receive_data_loop(Pid, Ref, Response#{content => <<Acc/binary, Data/binary>>});
    {gun_data, Pid, Ref, fin, Data} ->
      Response#{content => <<Acc/binary, Data/binary>>};
    A -> error(A)
  after 50000 ->
    error(timeout)
  end.