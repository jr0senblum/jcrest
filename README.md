# jcrest
Restful interface for JC

* Restfull Interface
  * Maps, Keys, and Values can be in binary representation of Integers, true, false, null, or strings
  * Provided: application/json, Accepted: application/x-www-form-urlencoded
  * DELETE, GET, HEAD, and OPTIONS verb support for Maps
  * DELETE, GET, HEAD, OPTIONS, and PUT are supported verbs for cache entries 
    (Map, Key, Values).
    * http://127.0.0.1:8080/maps
    * http://127.0.0.1:8080/maps/someMap
    * http://127.0.0.1:8080/maps/someMap/someKey
    * http://127.0.0.1:8080/maps/someMap/search/widget.text.hOffset=250
    * urlencoded type accepted with PUT parameters of value and (optionally) ttl an sequence
  
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

 

