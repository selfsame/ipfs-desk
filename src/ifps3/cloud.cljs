(ns ifps3.cloud
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require 
    [om.next :as om :refer-macros [defui]]
    [orbit.core :refer-macros [orbit de-orbit]]
    [cljs.core.async :as async :refer [>! <! put! chan]]
    [cljs.reader :as reader]
    [clojure.string :as string]
    [ifps3.data :as data])
  (:use 
    [ifps3.util :only [clog ->edn edn-> put-local get-local root-ref resize-dataurl]]
    [cljs.pprint :only [pprint]]))


'[amazon s3 stuff]

(def bucket "ifps3.selfsamegames.com")
(def bucket-url "http://s3.amazonaws.com/ifps3.selfsamegames.com/")
(def region "us-east-1")
(def access-key "AKIAJMSYCISSQMTCJXQQ")
(def path-separator "%5C/")

(defn ajax-get [url f fail]
  (when-let [req (new js/XMLHttpRequest)]
    (set! (.-onreadystatechange req)
      (fn [e] 
        (if (= 4 (.-readyState req))
          (if (= 200 (.-status req)) 
            (f (.. e -target -response))
            (when fail (fail e))))))
    (.open req "GET" url false)
    (.overrideMimeType req "text/xml; charset=iso-8859-1")
    (.send req)))

(defn aws-credentials []
  (.update (.-config js/AWS)
    #js {"accessKeyId" access-key
         "secretAccessKey" (get-local "secret") }))

(def S3 (.-S3 js/AWS))
(defn s3 [] (S3. #js {"region" region, "maxRetries" 15}))

(aws-credentials)

(defn get-object
  ([k] (get-object k #()))
  ([k f]
   (.getObject (s3)
     #js {"Bucket" bucket "Key" k}
     (fn [err res]
       (if err (do (.log js/console [err])))
       (if res (f res))))))

(defn list-objects
  ([f] (list-objects {} f))
  ([opts f]
   (.listObjects (s3)
     (clj->js (conj  {"Bucket" bucket } opts))
     (fn [err res]
       (if err (do (.log js/console err)))
       (if res (f res))))))

(defn delete-object
  ([k f]
   (.deleteObject (s3)
     #js {"Bucket" bucket "Key" k}
     (fn [err res]
       (if err (.log js/console err))
       (if res (f res))))))

(defn put-object
  ([k v opts] (put-object k v opts #()))
  ([k v opts f]
   (.putObject (s3)
     (clj->js (conj {"Bucket" bucket "Key" k "Body" v "ACL" "public-read"} opts))
     (fn [err res]
       (if err (.log js/console err))
       (f res)))))

(defn s3-exant? [uid f] 
  (list-objects {"Prefix" uid}
  #(if (first (get (js->clj %) "Contents")) (f true) (f false ))))

(defn put-db []
  (put-object "dags/db" (->edn (:db @ifps3.data/DATA)) {}))

(defn get-db []
  (ajax-get (str bucket-url "dags/db") 
    (fn [e] 
      (swap! ifps3.data/DATA conj {:db (edn-> e)})
      (pprint (:db @ifps3.data/DATA)) )
    (fn [e] (put-object "dags/db" (->edn {}) {}))))














'[ipfs.js stuff]


(defn file-meta [file]
  {:name     (.-name file) 
   :size (.-size file)
   :type     (.-type file)
   :modified (.toJSON (.-lastModifiedDate file))})



(defn image? [s] (if (re-find #"^image" s) true false))

(def mime-matchers {
  :gif #"^GIF"
  :png #"�PNG"
  :jpg #"����"
  :bmp #"BM�"
  :txt #"^"})

(defn guess-mime [buffer]
  (let [s (.substr (.toString buffer) 0 100)
        found (first (first (filter (fn [[k v]] (re-find v s)) mime-matchers)))]
    #_(prn 'guess-mime found s)
    found))

(defn buffer->data-image [buffer]
  (str "data:image;base64," (.toString buffer "base64")))



(defn save-data [& ks]
  (mapv 
    (fn [k] (let [data (get @(om.next/app-state @data/RECONCILER) k)]
      (put-local (str k) (->edn data))))
    ks))

(defn ifps-add [data f]
  (.add js/ipfs data
    (fn [err res]
      (if (or err (not res))
        (.error js/console err)
        (f res)))))

(defn ifps-cat [data f]
  (.cat js/ipfs data
    (fn [err res]
      (if (or err (not res))
        (.error js/console err)
        (f res)))))

(defn ifps-ls [data f]
  (.ls js/ipfs data
    (fn [err res]
      (if (or err (not res))
        (.error js/console err)
        (f res)))))


(defn image-loaded [e id]
  (om/transact! @data/RECONCILER 
    `[(std/update-in ~{:path [:meta/by-id id] 
      :fn #(assoc % :thumbnail (resize-dataurl (.-result (.-target e)) 32 32))}) :Main])
  (save-data :meta/by-id))

(defn file-loaded [e file]
  (clog #js [e file])
  (ifps-add 
    ((.-Buffer js/ipfs) (.-result (.-target e))) 
    (fn [res] 
      (let [id (.-Hash res)]
        (om/transact! @data/RECONCILER 
          `[(std/update-in ~{:path [:meta/by-id id] :fn #(merge % (file-meta file) {:id id})}) :Main])
        (if (image? (.-type file))
            (let [reader (new js/FileReader)]
              (aset reader "onload" #(do (image-loaded % id)))
              (.readAsDataURL reader file))
            (save-data :meta/by-id))))))

(defn read-file [file]
  (let [reader (new js/FileReader)]
    (aset reader "onload" #(do (file-loaded % file)))
    (.readAsArrayBuffer reader file)))



;(macroexpand '(orbit (+ 1 1)))
;(prn (orbit (+ 1 1)))


