-module(network_maybe).

-export([ main/2
        ]).

main(Port, AuthString) ->
  Bind = monad:bind(error_m:error_monad()),
  Result =
    Bind(get_connection(Port),
         fun(BareSocket) ->
             Bind(authorize_socket(BareSocket, AuthString),
                  fun(BareSocket1) ->
                      Bind(recieve_echo_message(BareSocket1),
                           fun({BareSocket2, BareMessage}) ->
                               write_echo_message(BareSocket2, BareMessage)
                           end
                          )
                  end
                 )
         end
        ),
  io:format("Result: ~p~n", [error_m:show_error(Result)]).

get_connection(Port) ->
  gen_tcp:connect("localhost", Port, [{active, false}]).

authorize_socket(Socket, AuthStr) ->
  Bind = monad:bind(error_m:error_monad()),
  Return = monad:return(error_m:error_monad()),
  CheckString =
    fun(Received) ->
        case Received of
          AuthStr -> Return(Socket);
          _ ->
            ErrorMsg =
              make_formatted_str(
                "Bad Auth String: Expected '~p', received '~p'",
                [AuthStr, Received]),
            error_m:error(ErrorMsg)
        end
    end,
  Bind(gen_tcp:recv(Socket, 0), CheckString).


recieve_echo_message(Socket) ->
  Bind = monad:bind(error_m:error_monad()),
  Return = monad:return(error_m:error_monad()),
  Bind(gen_tcp:recv(Socket, 0),
       fun(Message) ->
           Return({Socket, Message})
       end).

write_echo_message(Socket, Message) ->
  Return = monad:return(error_m:error_monad()),
  case gen_tcp:send(Socket, Message) of
    ok -> Return("Finished Writing Message");
    Otherwise -> Otherwise
  end.

make_formatted_str(FormatString, Args) ->
  binary_to_list(
    list_to_binary(
      io_lib:format(FormatString, Args))).
