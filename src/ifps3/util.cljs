(ns ifps3.util
  (:require 
    [ifps3.data]
    [om.core :as om :include-macros true]
    [dollar.bill :as $ :refer [$]]
    [cognitect.transit :as transit :refer [writer reader read]]))

(defn clog [thing] (.log js/console thing ))

(defn ->edn [o] (transit/write (transit/writer :json) o))
(defn edn-> [s] (transit/read (transit/reader :json) s))

(defn put-local [k v] (.setItem (aget  js/window "localStorage") k v))

(defn get-local [k] (.getItem (aget  js/window "localStorage") k ))

(defn format-bytes [n]
  (let [ns (str n)
        ord (.ceil js/Math (/ (count ns) 3))
        res (* n (get {1 1 2 .001 3 .000001 4 .000000001 5 .000000000001} ord))
        suff (get {1 "b" 2 "Kb" 3 "Mb" 4 "gb" 5 "tb"} ord)]
    (str (/ (int (* res 10)) 10) "" suff)))

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

(defn resize-dataurl [data width height]
  (let [img (first ($ (str "<img src='" data "'>")))
        thumb (.createElement js/document "img")
        canvas (first ($ (str "<canvas width='" width "' height='" height "'></canvas>")))
        ctx (.getContext canvas "2d")]
    (.drawImage ctx img 0 0 width height)
    (set! (.-src thumb) (.toDataURL canvas "image/png"))
    ($/append (.-body js/document) thumb)
    (.toDataURL canvas "image/png")))