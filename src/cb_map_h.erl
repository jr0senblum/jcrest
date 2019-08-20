%% ----------------------------------------------------------------------------
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



% Handler state. When resource_exists = true, cache results.
-type jctype() :: 'miss' | true | false | null | string().

-record(cb_map_state, {map        = miss  :: jctype(),
                       key        = miss  :: jctype(),
                       orig_value = miss  :: jctype(),
                       new_value  = miss  :: string() | 'miss', 
                       sequence   = false :: string() | 'miss', 
                       ttl        = 0     :: non_neg_integer()}).


%%% ============================================================================
%%% Module callbacks required for Cowboy handler
%%% ============================================================================


%% -----------------------------------------------------------------------------
%% Initialize state 
%%
-spec init(_,nonempty_maybe_improper_list()) -> {'cowboy_rest', Req, State}
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

delete_resource(Req, #cb_map_state{map=Map, key=Key} =State) ->
    lager:debug("~p: DELETE ~p, ~p.", [Map, Key]),
    try jc:evict(Map, Key) of
        ok -> {true, Req, State}
    catch
        _:_ -> {false, Req, State}
    end.
            

%% -----------------------------------------------------------------------------
%% Return true if the resource exists, cache the results if the KV does exist.
%% All actions hit this, so cache results for other REST verbs
%%
-spec resource_exists (Req, State) -> {boolean(), Req, State}
                                      when Req::cowboy_req:req(),
                                           State::#cb_map_state{}.

resource_exists(Req, State) ->
    Map = ensure_valid(url2internal(cowboy_req:binding(map, Req))),
    Key = ensure_valid(url2internal(cowboy_req:binding(key, Req))),
    {V, TTL, Seq, Req1} = get_values(Req),

    VFixed = case V of
                 miss -> miss;
                 V -> ensure_valid(V)
             end,
                   
    Result = case jc:get(Map, Key) of
                miss -> miss;
                {ok, Value} -> Value
            end,
    {miss /= Result, Req1, State#cb_map_state{map = Map,
                                              key = Key,
                                              orig_value = Result,
                                              new_value = VFixed,
                                              ttl = TTL,
                                              sequence = Seq}}.


% pull out the Value, TTL, and sequence in a request.
get_values(Req) ->
    {ok, Body, Req1} = cowboy_req:read_urlencoded_body(Req),

    V = proplists:get_value(<<"value">>, Body, miss),
    T = value_to_int(<<"ttl">>, 0, Body),
    S = value_to_int(<<"sequence">>, false, Body),
                       
    {V, T, S, Req1}.
                                            

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

kv_to_json(Req, #cb_map_state{map=Map, key=Key, orig_value = Value} = State) -> 
    lager:debug("~p: GET ~p:~p.",[?MODULE, Map, Key]),

    {kv_to_json(Req, Map, Key, Value), Req, State}.


%% -----------------------------------------------------------------------------
%% Put the urlencoded value into the map and key indicated by url.
%% Use TTL if ttl=xx is supplied, use jc_s if sequence=XX is supplied.
%%
-spec put_kv(Req, State) ->{true, Req, State}
                               when Req::cowboy_req:req(),
                                    State::#cb_map_state{}.

put_kv(Req, #cb_map_state{sequence=S}=State) ->
    case S of
        false -> put_kv_jc(Req, State);
        _Sequence -> put_kv_jc_s(Req, State)
    end.
    
put_kv_jc_s(Req, #cb_map_state{map=Map, 
                               key=Key, 
                               new_value=V, 
                               sequence=S, 
                               ttl = T}=State) ->
    {SHP, Path} = get_URI(Req),

    lager:debug("~p: jc_s:put(~p, ~p, ~p, ~p).",[?MODULE, Map, Key, T, S]),
    try jc_s:put(Map, Key, V, T, S) of
        {ok, Key} ->
            Req1 = cowboy_req:set_resp_header(<<"location">>, [SHP, Path], Req),
            {true, Req1, State};
        {error, out_of_seq} ->
            Req1 = cowboy_req:set_resp_header(<<"location">>, [SHP, Path], Req),
            {false, Req1, State}
    catch
        _:_ ->
            {false, Req, State}
    end.

put_kv_jc(Req, #cb_map_state{map=Map, key=Key, new_value = V, ttl = T}=State) ->
    lager:debug("~p: jc:put(~p, ~p, ~p).",[?MODULE, Map, Key, T]),


    try jc:put(Map, Key, V, T) of
        {ok, Key} ->
            {SHP, Path} = get_URI(Req),
            Req1 = cowboy_req:set_resp_header(<<"location">>, [SHP, Path], Req),
            {true, Req1, State};
        _ ->
            {false, Req, State}
    catch
        _:_ ->
            {false, Req, State}
    end.


% Ensure that we have either True, False, Null, Number, start of JSON array
% or object, or String.
ensure_valid(<<"true">>) -> <<"true">>;
ensure_valid(<<"false">>) ->  <<"false">>;
ensure_valid(<<"null">>) -> <<"null">>;
ensure_valid(BString) ->
    try binary_to_integer(BString) of
        _ -> BString
    catch
        error:badarg ->
            <<First:1/binary, _/binary>> = BString,
            case First of
                <<"\"">> -> BString;
                <<"{">> -> BString;
                <<"[">> -> BString;
                _E       -> <<"\"", BString/binary, "\"">>
            end
    end.


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
    [<<"{\"map_name\":">>, Map, <<",">>,
     <<"\"key\":">>, Key, <<",">>,
     <<"\"value\":">>, Value, <<",">>,
     <<"\"links\": [{\"rel\":\"self\",\"href\":\"">>,Url,<<"\"},">>,
     <<"{\"rel\":\"parent\",\"href\":\"">>,SHP, <<"/maps/">>, internal2url(Map),
     <<"\"}]}">>].




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

                    
% URL to navigate the RESTful state has to accomidate string types of Maps, Keys
% and Values. But, quotes (%22) is a mess in URLs, so use * in the URL but use
% the propper string when persisting/retrieving. 
%
% This pair of functions allows us to go from one (URL representation) to
% another (JC representation).
internal2url(<<First:1/binary,_/binary>> = Term) when First == <<"\"">> ->
    binary:replace(Term, <<"\"">>, <<"*">>, [global]);
internal2url(Term) ->
    Term.

url2internal(<<First:1/binary,_/binary>> = Term) when First == <<"*">> ->
    binary:replace(Term, <<"*">>, <<"\"">>, [global]);
url2internal(Term) ->
    Term.
