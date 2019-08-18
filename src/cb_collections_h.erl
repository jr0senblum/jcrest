%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum
%%% @copyright (C) 2020, Jim Rosenblum

%%% This is a Cowboy handler handling DELETE, GET, HEAD, and OPTIONS
%%% verbs for the RESTful collections of Map and Maps.
%%%
%%% Currently, only application/json is the only content-type provided.
%%%
%%% @version {@version}
%%% @end
%%% Created : 20 Oct 2018 by Jim Rosenblum
%%% ----------------------------------------------------------------------------

-module(cb_collections_h).

% Cowboy handler required functions
-export([init/2]).

% RESTFUL functions
-export([allowed_methods/2,
         content_types_provided/2,
         delete_completed/2,
         delete_resource/2,
         resource_exists/2]).

% Callback that handles constructing JSON responses
-export([collection_to_json/2]).

% Handler state
-record(cb_coll_state, {op :: map | maps | search,
                        body :: string() | [{jc:key(), jc:value()}]}).



-define(ENCODE(T), jsone:encode(T, [{float_format, [{decimals, 4}, compact]}])).

%%% ============================================================================
%%% Module callbacks required for Cowboy handler
%%% ============================================================================


%% -----------------------------------------------------------------------------
%% Initialize state to be parameter passed into the handler indicating 
%% whether we are dealing with map or maps collection.
%%
-spec init(_,nonempty_maybe_improper_list())-> {'cowboy_rest', Req, State}
                            when Req::cowboy_req:req(),
                                 State::#cb_coll_state{}.

init(Req, Opts) ->
    [Op | _] = Opts,
    State = #cb_coll_state{op=Op, body = ""},
    {cowboy_rest, Req, State}.



%%% ============================================================================
%%% RESTful callbacks
%%% ============================================================================


%% -----------------------------------------------------------------------------
%%
-spec allowed_methods (Req, State) -> {Method, Req, State}
                                      when Req::cowboy_req:req(),
                                           State::#cb_coll_state{},
                                           Method::nonempty_list().

allowed_methods(Req, State) ->
    Methods = [<<"DELETE">>, <<"GET">>, <<"HEAD">>, <<"OPTIONS">>], 
    {Methods, Req, State}.


%% -----------------------------------------------------------------------------
%% Only provide JSON for now.
%%
-spec content_types_provided(Req, State) -> {Types, Req, State}
                                            when Req::cowboy_req:req(),
                                                 State::#cb_coll_state{},
                                                 Types::cowboy:types().

content_types_provided(Req, State) ->
   Types =  [
             {<<"application/json">>, collection_to_json}
            ],
    {Types, Req, State}.


%% -----------------------------------------------------------------------------
%% DELETE the resource collection, maps or map.
%%
-spec delete_resource(Req, State) -> {Result, Req, State}
                                     when Result::boolean(),
                                          Req::cowboy_req:req(),
                                          State::#cb_coll_state{}.

delete_resource(Req, #cb_coll_state{op = maps} = State) ->                                          
    ok = jc:flush(),
    {true, Req, State};

delete_resource(Req, #cb_coll_state{op = map} = State) ->
    MapName = unfix(cowboy_req:binding(map, Req)),   
    ok = jc:clear(MapName),
    {true, Req, State}.


%% -----------------------------------------------------------------------------
%% Returns true when the resources is completely DELETEd.
%%
-spec delete_completed(Req, State) -> {Result, Req, State}
                                     when Result::boolean(),
                                          Req::cowboy_req:req(),
                                          State::#cb_coll_state{}.

delete_completed(Req, #cb_coll_state{op = maps} = State) ->
    {size, Sizes} = jc:cache_size(),
    {key_to_value, {records, N}, _} = proplists:lookup(key_to_value, Sizes),
    {N == 0, Req, State};

delete_completed(Req, #cb_coll_state{op = map} = State) ->
    Map = unfix(cowboy_req:binding(map, Req)),
    {jc:map_exists(Map) == false, Req, State}.


%% -----------------------------------------------------------------------------
%% Returns true when the resources exists, else false.
-spec resource_exists (Req, State) -> {boolean(), Req, State}
                                      when Req::cowboy_req:req(),
                                           State::#cb_coll_state{}.

resource_exists(Req, #cb_coll_state{op = maps} = State) ->
    % If there are any cache entries, than Maps exist.
    {size, Sizes} = jc:cache_size(),
    {key_to_value, {records, N}, _} = proplists:lookup(key_to_value, Sizes),
    {N > 0, Req, State};

resource_exists(Req, #cb_coll_state{op = map} = State) ->
    Map = unfix(cowboy_req:binding(map, Req)),
    {jc:map_exists(Map), Req, State};

resource_exists(Req, #cb_coll_state{op = search} = State) ->
    % If the resource exists, store in the State so we don't have to
    % pull them again as the GET unfolds.
    Map = unfix(cowboy_req:binding(map, Req)),
    Path = unfix(cowboy_req:binding(path, Req)),

    case jc:values_match(Map, Path) of
        {ok, []} ->
            {false, Req, State};
        {ok, Results} ->
            {true, Req, State#cb_coll_state{body=Results}}
    end.




%%% ============================================================================
%%% Function to package Map and Maps collections into JSON 
%%% ============================================================================


%% -----------------------------------------------------------------------------
%% Convert jc:key_set(map) and jc:maps() responses to json and 
%%
-spec collection_to_json(Req, State) ->{Value, Req, State}
                                      when Value::nonempty_list(iodata()),
                                           Req::cowboy_req:req(),
                                           State::#cb_coll_state{}.

collection_to_json(Req, #cb_coll_state{op = map} = State) ->
    {map_collection_body(Req), Req, State};

collection_to_json(Req, #cb_coll_state{op = search} = State) ->
    {kv_collection_body(Req, State#cb_coll_state.body), Req, State};

collection_to_json(Req, #cb_coll_state{op = maps} = State) ->
    {get_or_head_maps(Req), Req, State}.






%%% ============================================================================
%%% Internal functions
%%% ============================================================================

% kv collection is the result of a json .path search, KVList is the results of
% the search.
kv_collection_body(#{method := Verb} = Req, KVList) ->
    MapName = unfix(cowboy_req:binding(map, Req)),
    lager:debug("~p: ~p map ~p as JSON.",[?MODULE, Verb, MapName]),
    case Verb of
        <<"GET">> ->
            kvs_to_json(Req, MapName, KVList);
        <<"HEAD">> ->
            <<>>
    end.

map_collection_body(#{method := Verb} = Req) ->
    MapName = unfix(cowboy_req:binding(map, Req)),
    lager:debug("~p: ~p map ~p as JSON.",[?MODULE, Verb, MapName]),
    case Verb of
        <<"GET">> ->
            {ok, KeyList} = jc:key_set(MapName),
            map_to_json(Req, MapName, KeyList);
        <<"HEAD">> ->
            <<>>
    end.


get_or_head_maps(#{method := Verb} = Req) ->
    lager:debug("~p: ~p maps as JSON.", [?MODULE, Verb]),
    case Verb of
        <<"GET">> ->
            {maps, MapList} = jc:maps(),
            maps_to_json(Req, MapList);
        <<"HEAD">> ->
            <<>>
    end.


% ------------------------------------------------------------------------------
% Return {Scheme://Host:Port, Path} of the request.
%
get_URI(Req) ->
    Scheme = cowboy_req:scheme(Req),
    Host = cowboy_req:host(Req),
    Port = list_to_binary(integer_to_list(cowboy_req:port(Req))),
    Path = cowboy_req:path(Req),
    {[Scheme, <<"://">>, Host, <<":">>, Port],Path}.




% ------------------------------------------------------------------------------
% Construct the JSON represention of a map collection.
%
kvs_to_json(Req, MapName, KVList) ->
    {SHP, Path} = get_URI(Req),
    Url = [SHP, Path],
    ListOfMaps = 
        lists:foldl(fun({Key, Value}, Acc) ->
                            [[<<"{\"key\":">>, Key, <<",">>,
                              <<"\"value\":">>, Value,<<",">>,
                              <<"\"links\": [{\"rel\":\"self\",">>,
                              <<"\"href\":\"">>, SHP, <<"/maps/">>,fix(MapName), <<"/">>, fix(Key), <<"\"},">>,
                              <<"{\"rel\":\"parent\",\"href\":\"">>,SHP, <<"/maps/">>,fix(MapName), <<"\"}">>,
                              <<"]}">>]|Acc]
                    end,
                    [],
                    KVList),

    Separated = lists:join(<<",">>,ListOfMaps),
    [<<"{\"map_name\":">>, MapName, <<", \"items\": [">>, Separated, <<"],">>,
     <<"\"links\": [{\"rel\":\"self\",\"href\":\"">>,Url,<<"\"},">>,
     <<"{\"rel\":\"parent\",\"href\":\"">>,SHP,<<"/maps/">>,fix(MapName),<<"\"}]}">>].

map_to_json(Req, MapName, KeyList) ->
    {SHP, Path} = get_URI(Req),
    Url = [SHP, Path],
    ListOfMaps = 
        lists:foldl(fun(Key, Acc) ->
                            [[<<"{\"key\":">>, Key, <<",">>,
                              <<"\"links\": [{\"rel\":\"item\",">>,
                              <<"\"href\":\"">>, Url, <<"/">>,fix(Key),
                              <<"\"}]}">>]|Acc]
                    end,
                    [],
                    KeyList),
    Separated = lists:join(<<",">>,ListOfMaps),
    [<<"{\"map_name\":">>, MapName, <<", \"keys\": [">>, Separated, <<"],">>,
     <<"\"links\": {\"rel\":\"parent\",\"href\":\"">>,SHP,<<"/maps\"}}">>].


% ------------------------------------------------------------------------------
% Construct the JSON represention the maps collection.
%
maps_to_json(Req, MapList) ->
    {SHP, Path} = get_URI(Req),
    Url = [SHP, Path],

    ListOfMaps = 
        lists:foldl(fun(MapName, Acc) -> 
                            [[<<"{\"map_name\":">>,MapName,<<",">>,
                              <<"\"links\": [{\"rel\":\"collection\",">>,
                              <<"\"href\":\"">>, Url, <<"/">>,fix(MapName),
                              <<"\"}]}">>]|Acc]
                    end,
                    [],
                    MapList),
    Separated = lists:join(<<",">>,ListOfMaps),
    [<<"{\"maps\":">>, <<"[">>, Separated, <<"]}">>].


fix(<<First:1/binary,_/binary>> = Term) when First == <<"\"">> ->
    binary:replace(Term, <<"\"">>, <<"*">>, [global]);
fix(Term) ->
    Term.


 
unfix(<<First:1/binary,_/binary>> = Term) when First == <<"*">> ->
    binary:replace(Term, <<"*">>, <<"\"">>, [global]);
unfix(Term) ->
    Term.
