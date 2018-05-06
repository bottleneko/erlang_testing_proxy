-module(testing_proxy).

-export([init/2]).

init(Req0, State) ->
  Req1 = reply(Req0, State),
  {ok, Req1, State}.


reply(Req, _State) ->
  Uri = binary_to_list(cowboy_req:host(Req)),
  Path = cowboy_req:path(Req),
  Qs = cowboy_req:qs(Req),
  PathWithQs = <<Path/binary, <<"?">>/binary, Qs/binary>>,
  Headers = maps:to_list(cowboy_req:headers(Req)),
  Method = cowboy_req:method(Req),
  {ok, ConnPid} = gun:open(Uri, 80, #{connect_timeout => infinity, retry => 0}),
  StreamRef = gun:request(ConnPid, Method, PathWithQs, Headers),
  #{status := Status, headers := ResponseHeaders0, content := Received} = receive_response(ConnPid, StreamRef),
  ResponseHeaders1 = proplists:delete(<<"proxy-connection">>, ResponseHeaders0),
  ResponseHeaders2 = proplists:delete(<<"transfer-encoding">>, ResponseHeaders1),
  case Received of
    no_data ->
      cowboy_req:reply(Status, maps:from_list(ResponseHeaders1), Req);
    <<"">> ->
%%      if
%%        Status == 301 -> io:format("STATUS ~p~n~p~n~p~n", [Status, ResponseHeaders1, Req]);
%%        true -> ok
%%      end,
      cowboy_req:reply(Status, maps:from_list(ResponseHeaders2), Req);
    _ ->
      cowboy_req:reply(Status, maps:from_list(ResponseHeaders1), Received, Req)
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
  after 1000 ->
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
  after 5000 ->
    error(timeout)
  end.