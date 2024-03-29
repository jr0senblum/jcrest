# jcrest
Restful interface for JC


[![Build Status](https://travis-ci.org/jr0senblum/jcrest.svg?branch=master)](https://travis-ci.org/jr0senblum/jcrest)

### Introduction
* Restfull Interface for the  JC cache
  * Provided: application/json  
  * Accepted: application/x-www-form-urlencoded  
  * PUT parameters of value and (optionally) ttl and sequence    
  * DELETE, GET, HEAD, and OPTIONS verb support for Maps  
  * DELETE, GET, HEAD, OPTIONS, and PUT are supported verbs for cache entries 
    (Map, Key, Values).  
    * http://127.0.0.1:8080/maps  
    * http://127.0.0.1:8080/maps/someMap  
    * http://127.0.0.1:8080/maps/someMap/someKey  
    * http://127.0.0.1:8080/maps/someMap/search/widget.text.hOffset=250  

### Configuration  
* rebar.config pulls in JC via dependency  
* ip, port, and root defined in sysconfig in jcrest stanza 

```javascript
 {jcrest,
  [       
          {server_ip, "127.0.0.1"},
          {server_port, 8080},
          {server_root, "/"}
  ]
}
```


#### compile  
$ make compile

#### run tests  
$ make eunit

#### dialyze  
$ make dialyze


### Examples  

`curl -i -X OPTIONS http://127.0.0.1:8080/maps`  

```javascript
HTTP/1.1 200 OK
allow: DELETE, GET, HEAD, OPTIONS
content-length: 0
date: Wed, 21 Aug 2019 13:53:37 GMT
server: Cowboy
```


`curl -X PUT -d 'value=200&ttl=100&sequence=10' http://127.0.0.1:8080/maps/unit/3A`  produces   

```javascript
http://127.0.0.1:8080/maps/  
{
 maps: [
        {
         map_name: "unit",
         links: [
                 {
                  rel: "collection",
                  href: "http://127.0.0.1:8080/maps/*unit*"
                 }
                ]
        }
       ]
}


http://127.0.0.1:8080/maps/*unit*  
{
 map_name: "unit",
 keys: [
        {
         key: "3A",
         links: [
                 {rel: "item",
                  href: "http://127.0.0.1:8080/maps/*unit*/*3A*"
                 }
                ]
        }
       ],
 links: {
         rel: "parent",
         href: "http://127.0.0.1:8080/maps"
        }
}  

http://127.0.0.1:8080/maps/*unit*/*3A*  
{
 map_name: "unit",
 key: "3A",
 value: 200,
 links: [
         {
          rel: "self",
          href: "http://127.0.0.1:8080/maps/*unit*/*3A*"
         },
         {
          rel: "parent",
          href: "http://127.0.0.1:8080/maps/*unit*"
         }
        ]
}
```   

 

