%%
%% Tests
%%

-module(hateoas_tests).
-include_lib("eunit/include/eunit.hrl").

-export([all_my_test_/0,
         options_test/0,
         navigate_test/0]).




all_my_test_() ->
    [{"Options for Map and Maps",  fun options_test/0},
     {"Get and navigation for Map and Maps", fun navigate_test/0}].


% Allowable methods on Maps collections are Delete, Get, Head, and Options.
options_test() ->
    net_kernel:start(['jc@127.0.0.1', longnames]),
    application:set_env(jc, cache_nodes, ['jc@127.0.0.1']),
    application:ensure_all_started(jc),
    application:set_env(jcrest, server_ip, "127.0.0.1"),
    application:set_env(jcrest, server_port, 8080),
    application:set_env(jcrest, server_root, "/"),
    lager:set_loglevel(lager_console_backend, error),
    application:ensure_all_started(jcrest),

    {ok,{{"HTTP/1.1",200,"OK"},
         [_date , _server, 
          {"allow","DELETE, GET, HEAD, OPTIONS"},
          {"content-length","0"}], 
         []}} = httpc:request(options, 
                              {"http://127.0.0.1:8080/maps", []},
                              [],
                              []),
    true.

   
% Hypermedia as the Engine of Application State: HATEOAS 
navigate_test()->
    jc:put(<<"map1">>,<<"key1">>,<<"value1">>),
    jc:put(<<"map1">>,<<"key2">>,<<"value2">>),
    jc:put(<<"map2">>,<<"key1">>,<<"value1">>),
    jc:put(<<"map2">>,<<"key2">>,<<"value2">>),
    
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
    <<"http://127.0.0.1:8080/maps/map1">> = 
        jwalk:get({"links",{select, {"rel", "collection"}}, "href", 1}, Map1),

    % map2 portion of the jason contains map2 json, and that contains the URL
    % for the map1 collection.
    Map2 = jwalk:get({"maps",{select,{"map_name", "map2"}}}, JMap),
    <<"http://127.0.0.1:8080/maps/map2">> = 
        jwalk:get({"links",{select, {"rel", "collection"}}, "href", 1}, Map2),
    

    % GET on map should return JSON representation of the map collection
    {ok, {{_, 200, "OK"}, _, Map}} = 
        httpc:request(get, {"http://127.0.0.1:8080/maps/map2", []}, [], []),
    
    % Convert result to map
    JMapC = jsone:decode(list_to_binary(Map)),

    % Should be looking at map2
    <<"map2">> = jwalk:get({"map_name"}, JMapC),

    % Should have two keys, key1 and key2 with value1 and value2
    Keys = jwalk:get({"keys"}, JMapC),
    2 = length(Keys),
    <<"key1">> = jwalk:get({"keys",{select, {"key","key1"}},"key", 1}, JMapC),
    <<"key2">> = jwalk:get({"keys",{select, {"key","key2"}},"key", 1}, JMapC),
    <<"value1">> = jwalk:get({"keys",{select, {"key","key1"}},"value", 1}, JMapC),
    <<"value2">> = jwalk:get({"keys",{select, {"key","key2"}},"value", 1}, JMapC),

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

    <<"http://127.0.0.1:8080/maps/map2">> = jwalk:get({"links", {select,{"rel","parent"}},"href",1}, JItem),
    <<"http://127.0.0.1:8080/maps/map2/key1">> =  jwalk:get({"links", {select,{"rel","self"}},"href",1}, JItem),


    % GET on Key2 should return JSON representation of the m,k,v collection
    {ok, {{_, 200, "OK"}, _, Item2}} = 
        httpc:request(get, {binary_to_list(Key2Ref), []}, [], []),
    
    % Convert result to map
    JItem2 = jsone:decode(list_to_binary(Item2)),
    <<"map2">> = jwalk:get({"map_name"}, JItem2),
    <<"key2">> = jwalk:get({"key"}, JItem2),
    <<"value2">> = jwalk:get({"value"}, JItem2),

    <<"http://127.0.0.1:8080/maps/map2">> = jwalk:get({"links", {select,{"rel","parent"}},"href",1}, JItem2),
    <<"http://127.0.0.1:8080/maps/map2/key2">> =  jwalk:get({"links", {select,{"rel","self"}},"href",1}, JItem2).




    

    








    
   

    

