%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum
%%% @copyright (C) 2018-2019, Jim Rosenblum
%%% @doc Module that manages RESTful interactions for Maps and Map collections).
%%% This is a Cowboy handler handling DELETE, GET, HEAD, PUT, and OPTIONS
%%% verbs for Map x Key X Value JC cache entries.
%%%
%%% Currently, only application/json is the only content-type provided.
%%%
%%% @version {@version}
%%% @end
%%% Created : 20 Oct 2018 by Jim Rosenblum
%%% ----------------------------------------------------------------------------

-module(cb_map_h).

% Cowboy handler required functions
-export([init/2]).

% RESTFUL functions
-export([allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         delete_resource/2,
         resource_exists/2
        ]).

% Callbacks that construct JSON responses to GET and does the actual put.
-export([kv_to_json/2, put_kv/2]).

% Handler state. When resource_exists = true, cache result in value.
-record(cb_map_state, {value :: string()}).



%%% ============================================================================
%%% Module callbacks required for Cowboy handler
%%% ============================================================================


%% -----------------------------------------------------------------------------
%% Initialize state 
%%
-spec init(_,nonempty_maybe_improper_list())-> {'cowboy_rest', Req, State}
                            when Req::cowboy_req:req(),
                                 State::#cb_map_state{}.

init(Req, _Opts) ->
    State = #cb_map_state{},
    {cowboy_rest, Req, State}.



%%% ============================================================================
%%% RESTful callbacks
%%% ============================================================================


%% -----------------------------------------------------------------------------
%% 
-spec allowed_methods (Req, State) -> {Method, Req, State}
                                      when Req::cowboy_req:req(),
                                           State::#cb_map_state{},
                                           Method::nonempty_list().

allowed_methods(Req, State) ->
    Methods = [<<"DELETE">>, <<"GET">>, <<"HEAD">>, <<"OPTIONS">>, <<"PUT">>], 
    {Methods, Req, State}.


%% -----------------------------------------------------------------------------
%% Only provide JSON for now.
%%
-spec content_types_provided(Req, State) -> {Types, Req, State}
                                            when Req::cowboy_req:req(),
                                                 State::#cb_map_state{},
                                                 Types::cowboy:types().

content_types_provided(Req, State) ->
   Types =  [
             {<<"application/json">>, kv_to_json}
            ],
    {Types, Req, State}.


%% -----------------------------------------------------------------------------
%% Only accept urlencode for now.
%%
-spec content_types_accepted(Req, State) -> {Types, Req, State}
                                            when Req::cowboy_req:req(),
                                                 State::#cb_map_state{},
                                                 Types::cowboy:types().

content_types_accepted(Req, State) ->
   Types =  [
             {<<"application/x-www-form-urlencoded">>, put_kv}
            ],
    {Types, Req, State}.


%% -----------------------------------------------------------------------------
%% DELETE the KV resource collection.
%%
-spec delete_resource(Req, State) -> {Result, Req, State}
                                     when Result::boolean(),
                                          Req::cowboy_req:req(),
                                          State::#cb_map_state{}.

delete_resource(Req, State) ->
    MapName = cowboy_req:binding(map, Req),
    KeyName = cowboy_req:binding(key, Req),
    lager:debug("~p: DELETing ~,~.", [MapName, KeyName]),
    try jc:evict(MapName, KeyName) of
        ok -> {true, Req, State};
        false -> {false, Req, State}
    catch
        _:_ -> {false, Req, State}
    end.
            

%% -----------------------------------------------------------------------------
%% Return true if the resource exists, cache the value if the KV does exist.
%%
-spec resource_exists (Req, State) -> {boolean(), Req, State}
                                      when Req::cowboy_req:req(),
                                           State::#cb_map_state{}.

resource_exists(Req, State) ->
    Map = cowboy_req:binding(map, Req),
    Key = cowboy_req:binding(key, Req),
    Result = case jc:get(Map, Key) of
                miss -> miss;
                {ok, Value} -> Value
            end,
    {miss /= Result, Req, State#cb_map_state{value=Result}}.




%%% ============================================================================
%%% Function to package MKV into JSON and function for handling put
%%% ============================================================================


%% -----------------------------------------------------------------------------
%% Convert jc:get(map, key) to JSON (or empty if head).
%%
-spec kv_to_json(Req, State) ->{Value, Req, State}
                                      when Value::nonempty_list(iodata()),
                                           Req::cowboy_req:req(),
                                           State::#cb_map_state{}.

kv_to_json(Req, #cb_map_state{value = Value} = State) -> 
    Map = cowboy_req:binding(map, Req),
    Key = cowboy_req:binding(key, Req),
    lager:debug("~p: GET ~p:~p.",[?MODULE, Map, Key]),

    {kv_to_json(Req, Map, Key, Value), Req, State};

kv_to_json(#{method := <<"HEAD">>} = Req, State) ->
            {<<>>, Req, State}.


%% -----------------------------------------------------------------------------
%% Put the urlencoded value into the map and key indicated by url.
%% Use TTL if ttl=xx is supplied, use jc_s if sequence=XX is supplied.
%%
-spec put_kv(Req, State) ->{true, Req, State}
                                      when Req::cowboy_req:req(),
                                           State::#cb_map_state{}.
put_kv(Req, State) ->
    {ok, Body, Req1} = cowboy_req:read_urlencoded_body(Req),
    case value_to_int(<<"sequence">>, false, Body) of
        false -> put_kv_jc(Req1, Body, State);
        Seq -> put_kv_jc_s(Req1, Body, State, Seq)
    end.
    
put_kv_jc_s(Req, Body, State, Seq) ->
    Map = cowboy_req:binding(map, Req),
    Key = cowboy_req:binding(key, Req),
    Value = proplists:get_value(<<"value">>, Body),
    TTL = value_to_int(<<"ttl">>, 0, Body),
    {SHP, Path} = get_URI(Req),

    lager:debug("~p: jc_s:put(~p, ~p, ~p, ~p, ~p).",[?MODULE, Map, Key, Value, TTL, Seq]),
    case jc_s:put(Map, Key, Value, TTL, Seq) of
        {ok, Key} ->
            Req1 = cowboy_req:set_resp_header(<<"location">>, [SHP, Path], Req),
            {true, Req1, State};
        {error, out_of_seq} ->
            Req1 = cowboy_req:set_resp_header(<<"location">>, [SHP, Path], Req),
            {false, Req1, State}
    end.

put_kv_jc(Req, Body, State) ->
    Map = cowboy_req:binding(map, Req),
    Key = cowboy_req:binding(key, Req),
    Value = proplists:get_value(<<"value">>, Body),
    TTL = value_to_int(<<"ttl">>, 0, Body),

    lager:debug("~p: jc:put(~p, ~p, ~p, ~p).",[?MODULE, Map, Key, Value, TTL]),

    {ok, Key} = jc:put(Map, Key, Value, TTL),

    {SHP, Path} = get_URI(Req),
    Req1 = cowboy_req:set_resp_header(<<"location">>, [SHP, Path], Req),
    {true, Req1, State}.



%%% ============================================================================
%%% Internal functions
%%% ============================================================================


% ------------------------------------------------------------------------------
% Return {Scheme://Host:Port, Path} of the request.

get_URI(Req) ->
    Scheme = cowboy_req:scheme(Req),
    Host = cowboy_req:host(Req),
    Port = list_to_binary(integer_to_list(cowboy_req:port(Req))),
    Path = cowboy_req:path(Req),
    {[Scheme, <<"://">>, Host, <<":">>, Port],Path}.


% ------------------------------------------------------------------------------
% Construct the RESTFul JSON represention for jc:get(map, key).
%
kv_to_json(Req, Map, Key, Value) ->
    {SHP, Path} = get_URI(Req),
    Url = [SHP, Path],
    [<<"{\"map_name\":\"">>, Map, <<"\",">>,
     <<"\"key\": \"">>, Key, <<"\",">>,
     <<"\"value\": \"">>, Value, <<"\",">>,
     <<"\"links\": [{\"rel\":\"self\",\"href\":\"">>,Url,<<"\"},">>,
     <<"{\"rel\":\"parent\",\"href\":\"">>,SHP,<<"/maps/">>,Map,<<"\"}]}">>].


% ------------------------------------------------------------------------------
% Extract Value for Key (or Default, if not present in the list). Convert to 
% integer.
%
value_to_int(Key, Default, Body) ->
    case proplists:get_value(Key, Body, Default) of
        Default -> 
            Default;
        Other -> 
            try binary_to_integer(Other) 
            catch
                error:badarg ->
                    lager:warning("~p: Bad arg: ~p, expecting binary integer. Using ~p instead.", 
                                  [?MODULE, Other, Default]),
                    Default
            end
    end.

                    
                    
