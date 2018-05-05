-module(testing_proxy).

-export([
  init/2,
  is_authorized/2,
  to_html/2
]).


init(Req0, State) ->
  {cowboy_rest, Req0, State}.

%% TODO: make real authorization
is_authorized(Req, State) ->
  case cowboy_req:parse_header(<<"authorization">>, Req) of
    {digest, A} ->
      %%io:format("KEY ~p~n", [A]),
      {true, Req, State};
    _ ->
      {{false, <<"Digest realm=\"localhost\", nonce=\"fucku\", qop=\"auth\"">>}, Req, State}
  end.

to_html(Req, State) ->
  Uri = binary_to_list(cowboy_req:host(Req)),
  Path = cowboy_req:path(Req),
  Qs = cowboy_req:qs(Req),
  PathWithQs = <<Path/binary, <<"?">>/binary, Qs/binary>>,
  Headers = maps:to_list(cowboy_req:headers(Req)),
  Method = cowboy_req:method(Req),
  {ok, ConnPid} = gun:open(Uri, 80, #{connect_timeout => infinity, retry => 0}),
  StreamRef = gun:request(ConnPid, Method, PathWithQs, Headers),
  #{status := _Status, headers := ResponseHeaders, content := Received} = receive_response(ConnPid, StreamRef),
  Req1 = cowboy_req:set_resp_headers(maps:from_list(ResponseHeaders), Req),
  case Received of
    no_data ->
      {<<"">>, Req1, State};
    Data ->
      {Data, Req1, State}
  end.

receive_response(ConnPid, StreamRef) ->
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