(ns ipfs.core (:refer-clojure :exclude [cat]))

(declare construct)

(def cache (set! (.-cache js/window) #js {}))
(defn dag? [o] (or (= (aget o "DataSize") 2) (= (aget o "Data") "\b")))

(defn- ipfs-fn 
  ([prop] (ipfs-fn js/ipfs prop))
  ([root prop]
    (fn [data f]
      ((aget root prop) data
        (fn [err res]
          (if (or err (not res))
              (.error js/console err)
              ((or f identity) res)))))))

(def add (ipfs-fn "add"))
(def cat (ipfs-fn "cat"))
(def object-stat (ipfs-fn (.-object js/ipfs) "stat"))
(def object-get (ipfs-fn (.-object js/ipfs) "get"))

(defn subname [s] (last (re-find #"(.*\\)?([^\\]*)$" s)))

(defn js-eval [b] (js/eval (str "(" (.toString b) ")")))

(defn resolve 
  ([s] (resolve s identity))
  ([s cb]
    (let [root #js {}]
      (object-get s 
        (fn [v]
          (if (dag? v) 
            (construct v cb root) 
            (if-let [cached (get cache s)] 
              (cb cached)
              (cat s #(cb (aset cache s (js-eval %)))))))) 
      root)))

(defn construct 
  ([o cb] (construct o cb #js {}))
  ([o cb root]
    (.map (.-Links o)
      (fn [link] 
        (resolve (.-Hash link) 
          #(aset root (subname (.-Name link)) %)))) 
    (cb root)))

(set! (.-resolve js/window) resolve)
