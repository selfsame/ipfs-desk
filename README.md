# playground for ipfs.js with cljs and om.next

## setting up ipfs

* install ipfs (https://ipfs.io/)
  * `ipfs init`
  * `ipfs daemon`
  * allow CORS for localhost ipfs server
    * `ipfs config --json API.HTTPHeaders.Access-Control-Allow-Origin "[\"http://localhost:3449\"]"`
    * `ipfs config --json API.HTTPHeaders.Access-Control-Allow-Methods "[\"PUT\", \"GET\", \"POST\"]"`
    * `ipfs config --json API.HTTPHeaders.Access-Control-Allow-Credentials "[\"true\"]"`
* cljs
  * start figwheel `lein run -m clojure.main --init scripts/figwheel.clj`


_Drag files into the app to add them_

![ipfs-dropping](https://cloud.githubusercontent.com/assets/2467644/13241805/9a97f05a-d9bc-11e5-9b84-980a80e71a9b.gif)
