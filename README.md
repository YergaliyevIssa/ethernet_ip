ethernet_ip
=====

    Erlang library that provides access to Allen-Bradley PLCs. It is wrapper around [Link](https://github.com/libplctag/libplctag)

API
-----
    First of all start app
    {ok, EIPport} | {error, Error} = ethernet_ip:start_link('$some_random_atom$')
    
    Read operation
    [Value1, Value2] = ethernet_ip:read(EIPport, [{PLCtagName1, Type1}, {PLCtagName2, Type2}], #{<<"gateway">> => IP, <<"path">> => Path, <<"plc">> => PLC})

    Write operation
    ethernet_ip:write(EIPport, [{PLCtagName1, Type1, Value1}, {PLCtagName2, Type2, Value2}], #{<<"gateway">> => IP, <<"path">> => Path, <<"plc">> => PLC})
    


Build
-----

    $ ./rebar3 compile