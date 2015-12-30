(ns heh.core
  (:require-macros [cljs.core.async.macros :refer [go alt!]]
                   [heh.core :refer [component html]])
  (:require
      [om.core :as om :include-macros true]
      [om.dom :as dom :include-macros true]
      [cljs.core.async :as async :refer [>! <! put! chan pipe pub sub unsub close!]]))


(def PRIVATE (js/Object.))
(def BROADCAST-CHANS (atom {}))
(def BROADCAST-PUBS (atom {}))

(defn owner-key [owner] 
  (.. owner -_reactInternalInstance -_rootNodeID))

(defn private! [owner korks f]
  (let [func (if (= (type #()) (type f)) f (fn [v] f))
        k (owner-key owner)
        kcol (if (sequential? korks) korks [korks])
        store (update-in (or (aget PRIVATE k) {}) kcol func)]
    (aset PRIVATE k store)))


(defn private
  ([owner] (private owner []))
  ([owner korks]
    (let [kcol (if (sequential? korks) korks [korks])]
      (get-in (aget PRIVATE (owner-key owner)) kcol))))


 

(defn- start-match [s1 s2]
  (when-not (= s1 s2)
    (when (and (string? s1) (string? s2))
      (let [s1c (count s1)
            s2c (count s2)
            s2sub (if (>= s2c s1c) (subs s2 0 s1c) nil)]
        (when s2sub
          (== s1 s2sub))))))


(defn- p-sub->c [p]
  (let [c (chan)] 
    (sub p true c) c))
 

(defn -register [ks]
  (mapv
    (fn [k]
      (when-not (k @BROADCAST-CHANS)
        (swap! BROADCAST-CHANS conj {k (chan)}))
      (when-not (k @BROADCAST-PUBS)
        (swap! BROADCAST-PUBS conj {k (pub (k @BROADCAST-CHANS) map?)}))) ks))

(defn -goblock [owner events]
  (private! owner :_chans 
    (mapv 
      (fn [[k f]]
        (let [p (k @BROADCAST-PUBS)
              c (p-sub->c p)
              o-k (owner-key owner)]
          (do 
            (go (while true
              (let [event (<! c)
                    m (meta event)]
                (cond m
                  (cond (:up m) (if (start-match o-k (:up m)) (f event))
                      (:down m) (if (start-match (:down m) o-k) (f event)))
                  :else (f event)))))
            [k c]))) 
      events)))

(defn -clean [owner]  
  (mapv 
    (fn [[k c]] (unsub (k @BROADCAST-PUBS) true c)) 
    (private owner :_chans))
  (private! owner :_chans nil))



(defn emit! 
  ([k e] (when-let [c (k @BROADCAST-CHANS)] (put! c e)))
  ([_ k e] (emit! k e)))

(defn up! [owner k e]
  (when (map? e)
    (when-let [c (k @BROADCAST-CHANS)]
        (put! c (with-meta e {:up (owner-key owner)})))))

(defn down! [owner k e]
  (when (map? e)
      (when-let [c (k @BROADCAST-CHANS )]
        (put! c (with-meta e {:down (owner-key owner)})) true)))
 




 




  
  

