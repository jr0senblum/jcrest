# jcrest
Restful interface for JC

* Restfull Interface
  * Maps, Keys, and Values can be in Integers, true, false, null, or strings
  * Provided: application/json, Accepted: application/x-www-form-urlencoded
  * DELETE, GET, HEAD, and OPTIONS verb support for Maps
  * DELETE, GET, HEAD, OPTIONS, and PUT are supported verbs for cache entries 
    (Map, Key, Values).
    * http://127.0.0.1:8080/maps
    * http://127.0.0.1:8080/maps/someMap
    * http://127.0.0.1:8080/maps/someMap/someKey
    * http://127.0.0.1:8080/maps/someMap/search/widget.text.hOffset=250
    * urlencoded type accepted with PUT parameters of value and (optionally) ttl an sequence
      curl -X PUT -d 'value=200&ttl=100' http://127.0.0.1:8080/maps/unit/3A 
    * Adding a sequence to the parameters uses the jc_s module
      curl -X PUT -d 'value=200&ttl=100&sequence=10' http://127.0.0.1:8080/maps/unit/3A 

