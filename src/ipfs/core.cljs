(ns ^:figwheel-always ipfs.core 
  (:refer-clojure :exclude [cat realize])
  (:require [hyper.js :refer [log]]))


(declare construct)

(def cache (set! (.-cache js/window) #js {}))
(defn dag? [o] (or (= (aget o "DataSize") 2) (= (aget o "Data") "\b")))
(defn link? [s] (re-find #"^Qm[a-zA-Z0-9]{44}$" s))

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

(defn realize
  ([s] (or (aget cache s) (realize s identity)))
  ([s cb]
    (if-let [cached (aget cache s)] 
      (do (cb cached) cached)
      (let [root #js {}]
        (aset cache s root)
        (object-get s 
          (fn [v]
            (if (dag? v) 
              (construct v cb root) 
              (if-let [cached (get cache s)] 
                (cb cached)
                (cat s #(cb (aset cache s %))))))) 
        root))))

(defn construct 
  ([o cb] (construct o cb #js {}))
  ([o cb root]
    (.map (.-Links o)
      (fn [link] 
        (realize (.-Hash link) 
          #(aset root (subname (.-Name link)) %)))) 
    (cb root)))

(def lib (realize "QmVfZLPJkbKfVALskNsiCzsbPtVPp2CmuiU9aaRc1ctSKA"))

(log lib)