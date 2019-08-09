%%
%% Tests
%%

-module(rest_tests).
-include_lib("eunit/include/eunit.hrl").



all_my_test_() ->
    [{"Empty cache should not have any maps", fun empty_test/0},
     {"Options for Maps", fun maps_options_test/0},
     {"Options for Map", fun map_options_test/0},
     {"Options for Cache Item", fun item_options_test/0},
     {"Head test - no complaints", fun head_test/0},
     {"Simple put test", fun simple_put_test/0},
     {"Delete mkv test", fun delete_mkv_test/0},
     {"Delete map test", fun delete_map_test/0},
     {"Get and navigation for Map and Maps", fun hateoas_test/0},
     {"Seq and TTL tests", fun seq_test/0}].


% Empty cache, maps should not exist.
% Use first test to start up the server. Not great, but...
empty_test() ->
    net_kernel:start(['jc@127.0.0.1', longnames]),
    application:set_env(jc, cache_nodes, ['jc@127.0.0.1']),
    application:ensure_all_started(jc),
    application:set_env(jcrest, server_ip, "127.0.0.1"),
    application:set_env(jcrest, server_port, 8080),
    application:set_env(jcrest, server_root, "/"),
    lager:set_loglevel(lager_console_backend, error),
    application:ensure_all_started(jcrest),

    jc:flush(),
    ?assertMatch({ok,{{"HTTP/1.1",404,"Not Found"},
                        [_date,
                         {"server","Cowboy"},
                         {"content-length","0"}],
                        []}},    
                   httpc:request(get, {"http://127.0.0.1:8080/maps", []}, [], [])),
    
    jc:put(<<"1">>,<<"1">>,<<"1">>),

    ?assertMatch({ok,{{"HTTP/1.1",200,"OK"},
                      [_date,
                       {"server","Cowboy"},
                       _length,
                       {"content-type","application/json"}],
                      _body}},
                 httpc:request(get, {"http://127.0.0.1:8080/maps", []}, [], [])),
    jc:flush().


% Allowable methods on Maps collections are Delete, Get, Head, and Options.
maps_options_test() ->
    ?assertMatch({ok,{{"HTTP/1.1",200,"OK"},
                      [_date , _server, 
                       {"allow","DELETE, GET, HEAD, OPTIONS"},
                       {"content-length","0"}], 
                      []}},
                 httpc:request(options, 
                               {"http://127.0.0.1:8080/maps", []},
                               [],
                               [])).

% Allowable methods on Map are Delete, Get, Head, and Options.
map_options_test() ->
    jc:flush(),
    jc:put(<<"\"aMap\"">>,<<"1">>,<<"1">>),
    ?assertMatch({ok,{{"HTTP/1.1",200,"OK"},
                      [_date , _server, 
                       {"allow","DELETE, GET, HEAD, OPTIONS"},
                       {"content-length","0"}], 
                      []}},
                 httpc:request(options, 
                               {"http://127.0.0.1:8080/maps/aMap", []},
                               [],
                               [])).

% Allowable methods on cahce entry (M,K,V) are Delete, Get, Head, Options, Put.
item_options_test() ->
    jc:flush(),
    jc:put(<<"\"aMap\"">>,<<"1">>,<<"1">>),
    ?assertMatch({ok,{{"HTTP/1.1",200,"OK"},
                      [_date , _server, 
                       {"allow","DELETE, GET, HEAD, OPTIONS, PUT"},
                       {"content-length","0"}], 
                      []}},
                 httpc:request(options, 
                               {"http://127.0.0.1:8080/maps/aMap/1", []},
                               [],
                               [])).

head_test() ->
    jc:flush(),
    jc:put(<<"\"bed\"">>,<<"1">>,<<"2">>),    

    ?assertMatch({ok,{{"HTTP/1.1",200,"OK"},
                       [_date, _server,
                        {"content-length","0"}],
                       []}},
                 httpc:request(head, {"http://127.0.0.1:8080/maps", []}, [], [])),

    ?assertMatch({ok,{{"HTTP/1.1",404,"Not Found"},
                       [_date, _server,
                        {"content-length","0"}],
                       []}},
                 httpc:request(head, {"http://127.0.0.1:8080/mapss", []}, [], [])),
    
    ?assertMatch({ok,{{"HTTP/1.1",200,"OK"},
                       [_date, _server,
                        {"content-length","0"}],
                       []}},
                 httpc:request(head, {"http://127.0.0.1:8080/maps/%22bed%22", []}, [], [])),

    ?assertMatch({ok,{{"HTTP/1.1",404,"Not Found"},
                       [_date, _server,
                        {"content-length","0"}],
                       []}},
                 httpc:request(head, {"http://127.0.0.1:8080/maps/%22not%22", []}, [], [])),
    
    ?assertMatch({ok,{{"HTTP/1.1",200,"OK"},
                       [_date, _server,
                        {"content-length",_},
                        {"content-type","application/json"}],
                       []}},
                 httpc:request(head, {"http://127.0.0.1:8080/maps/%22bed%22/1", []}, [], [])),

    ?assertMatch({ok,{{"HTTP/1.1",404,"Not Found"},
                       [_date, _server,
                        {"content-length","0"}],
                       []}},
                 httpc:request(head, {"http://127.0.0.1:8080/maps/%22bed%22/11", []}, [], [])).


% Put should create, update to existing should return no content.   
simple_put_test() ->
    jc:flush(),
    ?assertMatch({ok,{{"HTTP/1.1",201,"Created"},
                       [_date,
                        {"location","http://127.0.0.1:8080/maps/aMap/key1"},
                        {"server","Cowboy"},
                        {"content-length","0"}],
                      []}},
                 httpc:request(put, {"http://127.0.0.1:8080/maps/aMap/key1", 
                                     [],
                                     "application/x-www-form-urlencoded",
                                     "value=\"key1Value\""}, [], [])),

    ?assertMatch({ok,{{"HTTP/1.1",204,"No Content"},
                  [_date,
                   {"location","http://127.0.0.1:8080/maps/aMap/key1"},
                   {"server","Cowboy"}],
                  []}},
                 httpc:request(put, {"http://127.0.0.1:8080/maps/aMap/key1", 
                                     [],
                                     "application/x-www-form-urlencoded",
                                     "value=\"key1Value\""}, [], [])),

    ?assertMatch({ok,{{"HTTP/1.1",201,"Created"},
                       [_date,
                        {"location","http://127.0.0.1:8080/maps/true/false"},
                        {"server","Cowboy"},
                        {"content-length","0"}],
                      []}},
                 httpc:request(put, {"http://127.0.0.1:8080/maps/true/false", 
                                     [],
                                     "application/x-www-form-urlencoded",
                                     "value=null"}, [], [])),

    ?assertEqual(jc:get(<<"true">>, <<"false">>), {ok, <<"null">>}).

% Delete Map Key Value test
delete_mkv_test() ->
    jc:flush(),
    jc:put(<<"\"bed\"">>,<<"1">>,<<"1">>),
    jc:put(<<"\"bed\"">>,<<"2">>,<<"2">>),
    ?assertMatch({ok,{{"HTTP/1.1",204,"No Content"},
                      [_date,
                       {"server","Cowboy"}],
                      []}},
                 httpc:request(delete, 
                               {"http://127.0.0.1:8080/maps/%22bed%22/1",
                                []}, [], [])),

    ?assertMatch({ok,{{"HTTP/1.1",404,"Not Found"},
                      [_date,
                       {"server","Cowboy"},
                       {"content-length","0"}],
                      []}},
                 httpc:request(delete, 
                               {"http://127.0.0.1:8080/maps/bed/1",
                                []}, [], [])),
    ?assertEqual(miss, jc:get(<<"\"bed\"">>, <<"1">>)),
    ?assertEqual({ok, <<"2">>}, jc:get(<<"\"bed\"">>, <<"2">>)),

    ?assertMatch({ok,{{"HTTP/1.1",204,"No Content"},
                      [_date,
                       {"server","Cowboy"}],
                      []}},
                 httpc:request(delete, 
                               {"http://127.0.0.1:8080/maps/%22bed%22/2",
                                []}, [], [])),
    ?assertEqual(miss, jc:get(<<"\"bed\"">>, 2)),
    ?assertEqual(false, jc:map_exists(<<"\"bed\"">>)).


% Delete Map test
delete_map_test() ->
    jc:flush(),
    jc:put(<<"\"bed\"">>,<<"1">>,<<"1">>),
    jc:put(<<"\"other\"">>,<<"1">>,<<"1">>),
    ?assertEqual(jc:map_exists(<<"\"bed\"">>), true),
    ?assertEqual(jc:map_exists(<<"\"other\"">>), true),

    ?assertMatch({ok,{{"HTTP/1.1",204,"No Content"},
                      [_date,
                       {"server","Cowboy"}],
                      []}},
                 httpc:request(delete, 
                               {"http://127.0.0.1:8080/maps/%22bed%22",
                                []}, [], [])),

    ?assertMatch({ok,{{"HTTP/1.1",404,"Not Found"},
                      [_date,
                       {"server","Cowboy"},
                       {"content-length","0"}],
                      []}},
                 httpc:request(delete, 
                               {"http://127.0.0.1:8080/maps/%22bed%22",
                                []}, [], [])),

    ?assertEqual(jc:map_exists(<<"\"other\"">>), true),
    ?assertEqual(jc:map_exists(<<"\"bed\"">>), false),

    jc:put(<<"\"bed\"">>,<<"1">>,<<"1">>),

    ?assertMatch({ok,{{"HTTP/1.1",204,"No Content"},
                      [_date,
                       {"server","Cowboy"}],
                      []}},
                 httpc:request(delete, 
                               {"http://127.0.0.1:8080/maps",
                                []}, [], [])),

    ?assertEqual(jc:map_exists(<<"\"other\"">>), false),
    ?assertEqual(jc:map_exists(<<"\"bed\"">>), false),

    ?assertMatch({ok,{{"HTTP/1.1",404,"Not Found"},
                      [_date,
                       {"server","Cowboy"},
                       {"content-length","0"}],
                      []}},
                 httpc:request(delete, 
                               {"http://127.0.0.1:8080/maps",
                                []}, [], [])).



% Hypermedia as the Engine of Application State: HATEOAS 
hateoas_test()->
    jc:flush(),
    jc:put(<<"\"map1\"">>,<<"\"key1\"">>,<<"\"value1\"">>),
    jc:put(<<"\"map1\"">>,<<"\"key2\"">>,<<"\"value2\"">>),
    jc:put(<<"\"map2\"">>,<<"\"key1\"">>,<<"\"value1\"">>),
    jc:put(<<"\"map2\"">>,<<"\"key2\"">>,<<"\"value2\"">>),
    
    % GET on maps should return JSON representation of the maps collection
    {ok, {{_, 200, "OK"}, _, Maps}} = 
        httpc:request(get, {"http://127.0.0.1:8080/maps", []}, [], []),
    
    % Convert result to map
    JMap = jsone:decode(list_to_binary(Maps)),

    % Should contains the two maps, map1 and map2
    MapNames = jwalk:get({"maps","map_name"}, JMap),
    lists:member(<<"map2">>, MapNames),
    lists:member(<<"map1">>, MapNames),
    2 = length(MapNames),

    % map1 portion of the jason contains map1 json, and that contains the URL
    % for the map1 collection
    Map1 = jwalk:get({"maps",{select,{"map_name", "map1"}}}, JMap),
    <<"http://127.0.0.1:8080/maps/%22map1%22">> = 
        jwalk:get({"links",{select, {"rel", "collection"}}, "href", 1}, Map1),

    % map2 portion of the jason contains map2 json, and that contains the URL
    % for the map1 collection.
    Map2 = jwalk:get({"maps",{select,{"map_name", "map2"}}}, JMap),
    <<"http://127.0.0.1:8080/maps/%22map2%22">> = 
        jwalk:get({"links",{select, {"rel", "collection"}}, "href", 1}, Map2),
    

    % GET on map should return JSON representation of the map collection
    {ok, {{_, 200, "OK"}, _, Map}} = 
        httpc:request(get, {"http://127.0.0.1:8080/maps/%22map2%22", []}, [], []),
    
    % Convert result to map
    JMapC = jsone:decode(list_to_binary(Map)),

    % Should be looking at map2
    <<"map2">> = jwalk:get({"map_name"}, JMapC),

    % Should have two keys, key1 and key2 with value1 and value2
    Keys = jwalk:get({"keys"}, JMapC),
    2 = length(Keys),
    <<"key1">> = jwalk:get({"keys",{select, {"key","key1"}},"key", 1}, JMapC),
    <<"key2">> = jwalk:get({"keys",{select, {"key","key2"}},"key", 1}, JMapC),

    % Parent should be the maps collection
    <<"parent">> =jwalk:get({"links","rel"}, JMapC),
    <<"http://127.0.0.1:8080/maps">> =jwalk:get({"links","href"}, JMapC),

    % Get the item
    <<"item">> = jwalk:get({"keys",{select, {"key","key1"}},"links", "rel", 1}, JMapC),
    <<"item">> = jwalk:get({"keys",{select, {"key","key2"}},"links", "rel", 1}, JMapC),
    Key1Ref = jwalk:get({"keys",{select, {"key","key1"}},"links", "href", 1}, JMapC),
    Key2Ref = jwalk:get({"keys",{select, {"key","key2"}},"links", "href", 1}, JMapC),
    
    % GET on Key1 should return JSON representation of the m,k,v collection
    {ok, {{_, 200, "OK"}, _, Item}} = 
        httpc:request(get, {binary_to_list(Key1Ref), []}, [], []),
    
    % Convert result to map
    JItem = jsone:decode(list_to_binary(Item)),
    <<"map2">> = jwalk:get({"map_name"}, JItem),
    <<"key1">> = jwalk:get({"key"}, JItem),
    <<"value1">> = jwalk:get({"value"}, JItem),

    <<"http://127.0.0.1:8080/maps/%22map2%22">> = jwalk:get({"links", {select,{"rel","parent"}},"href",1}, JItem),
    <<"http://127.0.0.1:8080/maps/%22map2%22/%22key1%22">> =  jwalk:get({"links", {select,{"rel","self"}},"href",1}, JItem),


    % GET on Key2 should return JSON representation of the m,k,v collection
    {ok, {{_, 200, "OK"}, _, Item2}} = 
        httpc:request(get, {binary_to_list(Key2Ref), []}, [], []),
    
    % Convert result to map
    JItem2 = jsone:decode(list_to_binary(Item2)),
    <<"map2">> = jwalk:get({"map_name"}, JItem2),
    <<"key2">> = jwalk:get({"key"}, JItem2),
    <<"value2">> = jwalk:get({"value"}, JItem2),

    <<"http://127.0.0.1:8080/maps/%22map2%22">> = jwalk:get({"links", {select,{"rel","parent"}},"href",1}, JItem2),
    <<"http://127.0.0.1:8080/maps/%22map2%22/%22key2%22">> =  jwalk:get({"links", {select,{"rel","self"}},"href",1}, JItem2).
    


search_test() ->
    On =
        #{<<"widget">> =>
              #{<<"debug">> => <<"on">>,
                <<"image">> =>
                    #{<<"alignment">> => <<"center">>,<<"hOffset">> => 250,
                      <<"name">> => <<"sun1">>,<<"src">> => <<"Images/Sun.png">>,
                      <<"vOffset">> => 250},
                <<"text">> =>
                    #{<<"alignment">> => <<"center">>,
                      <<"data">> => <<"Click Here">>,<<"hOffset">> => 250,
                      <<"name">> => <<"text1">>,
                      <<"onMouseUp">> =>
                          <<"sun1.opacity = (sun1.opacity / 100) * 90;">>,
                      <<"size">> => 36,<<"style">> => <<"bold">>,
                      <<"vOffset">> => 100},
                <<"window">> =>
                    #{<<"height">> => 500,<<"name">> => <<"main_window">>,
                      <<"title">> => <<"Sample Konfabulator Widget">>,
                      <<"width">> => 500}}},
    Off = jwalk:set({"widget","debug"}, On, <<"off">>),

    jc:put(<<"\"graphics\"">>, <<"\"on\"">>, jsone:encode(On)),
    jc:put(<<"\"graphics\"">>, <<"\"off\"">>, jsone:encode(Off)),



    { ok,{{"HTTP/1.1",200,"OK"},
          [_date,
           {"server","Cowboy"},
           {"content-length", _},
           {"content-type","application/json"}], Result}} = 
        httpc:request(get, {"http://127.0.0.1:8080/maps/%22graphics%22/search/widget.image.name=%22sun1%22", []}, [], []),
    AsMap = jsone:decode(list_to_binary(Result)),
    
    Items = maps:get(<<"items">>, AsMap),
    ?assertEqual(2, length(Items)),

    ?assertEqual(<<"off">>, maps:get(<<"key">>,lists:nth(1,Items))),
    ?assertEqual(<<"on">>, maps:get(<<"key">>,lists:nth(2,Items))),

    { ok,{{"HTTP/1.1",200,"OK"},
          [_date,
           {"server","Cowboy"},
           {"content-length", _},
           {"content-type","application/json"}], Results2}} = 
        httpc:request(get, {"http://127.0.0.1:8080/maps/%22graphics%22/search/widget.debug=%22off%22", []}, [], []),
    AsMap2 = jsone:decode(list_to_binary(Results2)),
    
    Items2 = maps:get(<<"items">>, AsMap2),
    ?assertEqual(1, length(Items2)),

    ?assertEqual(<<"off">>, maps:get(<<"key">>,lists:nth(1,Items2))),

    { ok,{{"HTTP/1.1",404, "Not Found"},
          [_date,
           {"server","Cowboy"},
           {"content-length", "0"}], []}} = 
        httpc:request(get, {"http://127.0.0.1:8080/maps/%22graphics%22/search/widget.debug=1", []}, [], []).

    
seq_test() ->
    {ok,{{"HTTP/1.1",201,"Created"},
         [_date,
          {"location","http://127.0.0.1:8080/maps/map11/ttlKey"},
          {"server","Cowboy"},
          {"content-length","0"}],
         []}
    } = httpc:request(put, {"http://127.0.0.1:8080/maps/map11/ttlKey", 
                            [],
                            "application/x-www-form-urlencoded",
                            "value=value2&ttl=3&sequence=10"}, [], []),

    {ok, <<"value2">>} = jc:get(<<"map11">>, <<"ttlKey">>),

    % Bad request because of lower sequence no.
    {ok,{{"HTTP/1.1",400,"Bad Request"},
         [_date,
          {"location","http://127.0.0.1:8080/maps/map11/ttlKey"},
          {"server","Cowboy"},
          {"content-length","0"}],
         []}
    } = httpc:request(put, {"http://127.0.0.1:8080/maps/map11/ttlKey", 
                            [],
                            "application/x-www-form-urlencoded",
                            "value=value2&ttl=3&sequence=1"}, [], []),
    timer:sleep(3004),

    miss = jc:get(<<"map11">>, <<"ttlKey">>).




    








    
   

    

