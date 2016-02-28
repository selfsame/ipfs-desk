(ns ifps3.util
  (:require 
    [ifps3.data]
    [om.core :as om :include-macros true]
    [dollar.bill :as $ :refer [$]]
    [cognitect.transit :as transit :refer [writer reader read]]))

(defn ->edn [o] (transit/write (transit/writer :json) o))
(defn edn-> [s] (transit/read (transit/reader :json) s))

(defn img-fit [[w h] [ww wh]]
  (let [hratio (if (> h wh) (/ wh h) 1.0)
        ratio (if (> (* w hratio) ww) (/ ww w) hratio)
        [rw rh] (mapv * [w h] [ratio ratio])]
    [rw rh]))

(defn location [e]
  [ (int (.-clientX e)) (int (.-clientY e))])

(defn root-ref [& more]
  (let [ref (get-in (om/root-cursor ifps3.data/DATA) more)]
             (om/ref-cursor ref)))
