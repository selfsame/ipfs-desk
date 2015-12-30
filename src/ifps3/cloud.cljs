(ns ifps3.cloud
  (:require 
    [om.next :as om :refer-macros [defui]]
    [clojure.string :as string]
    [ifps3.data :as data])
  (:use 
    [ifps3.util :only [clog ->edn edn-> put-local get-local root-ref]]
    [cljs.pprint :only [pprint]]))


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
    #_ (set! (.-responseType req) "arraybuffer")
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



(defn put-db []
  (put-object "dags/db" (->edn (:db @ifps3.data/DATA)) {}))

(defn get-db []
  (ajax-get (str bucket-url "dags/db") 
    (fn [e] 
      (swap! ifps3.data/DATA conj {:db (edn-> e)})
      (pprint (:db @ifps3.data/DATA)) )
    (fn [e] (put-object "dags/db" (->edn {}) {}))))










(defn file-meta [file]
  {:name     (.-name file) :size (.-size file)
   :type     (.-type file)
   :modified (.toJSON (.-lastModifiedDate file))})

(defn archive-file [file data meta]
  (let [cb (fn [e]
             (prn "put object")
             (let [result (.-result (.-target e))]
               (put-object (str "t/" (:uid meta)) result {"ContentType" (:type meta)}
                 (fn [z] (do
                           (prn "put confirm")
                           (om/transact! (root-ref :uploads (:name meta)) #(conj % {:archived true}))
                           (om/transact! (root-ref :db :meta) conj {(:uid meta) meta})

                           )))))
        reader (new js/FileReader)]
    (aset reader "onload" cb)
    (.readAsArrayBuffer reader file))
  )

(defn s3-exant? [uid f] 
  (list-objects {"Prefix" uid}
  #(if (first (get (js->clj %) "Contents")) (f true) (f false ))))

(defn upload-status! [data v]
  #_ (if (om/cursor? data)
    (om/update! data [:archived] v)
    (prn data)) )

(defn binary-file-loaded [file data meta]
  "upload binary to db, if success update local db with uid:meta and commit"
  (prn "read binary")
  (s3-exant? (str "t/" (:uid meta))
    #(if % (upload-status! (root-ref :uploads (:name meta)) true)
           (archive-file file data meta))))




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

(defn file-loaded [e file]
  (ifps-add 
    ((.-Buffer js/ipfs) (.-result (.-target e))) 
    (fn [res] 
      (om/transact! @data/RECONCILER 
        `[(db/add ~{(.-Hash res) (conj (file-meta file) {:type 2 :id (.-Hash res)})}) :Main])
      (save-data :meta/by-id))))


(defn read-file [file]
  (let [reader (new js/FileReader)]
    (clog (str "read-file " (.-type file)))
    (aset reader "onload" #(do (file-loaded % file)))
    (.readAsArrayBuffer reader file)))
