(ns ifps3.cloud
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require 
    [om.next :as om :refer-macros [defui]]
    [orbit.core :refer-macros [orbit de-orbit ipjs]]
    [cljs.core.async :as async :refer [>! <! put! chan]]
    [cljs.pprint :as pprint]
    [cljs.reader :as reader]
    [hyper.js :refer [log put-local get-local]]
    [clojure.string :as string]
    [ifps3.data :as data])
  (:use 
    [ifps3.util :only [->edn edn-> root-ref]]
    ))



(defn file-meta [file]
  {:name     (.-name file) 
   :size     (.-size file)
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
  (prn 'save-data ) 
  (mapv 
    (fn [k] (let [data (get @(om.next/app-state @data/RECONCILER) k)]
      (put-local (str k) (->edn data))))
    ks) ks)

(defn load-data [& ks]
  (prn 'load-data)
  (select-keys (swap! data/DATA (fn [col] (merge-with conj col (into {} (map #(vector % (edn-> (get-local (str %)))) ks))))) (vec ks)))

(defn- ifps-fn 
  ([prop] (ifps-fn js/ipfs prop))
  ([root prop]
    (fn [data f]
      ((aget root prop) data
        (fn [err res]
          (if (or err (not res))
            (.error js/console err)
            (f res)))))))

(def ifps-add (ifps-fn "add"))
(def ifps-cat (ifps-fn "cat"))
(def ifps-object-stat (ifps-fn (.-object js/ipfs) "stat"))
(def ifps-object-get (ifps-fn (.-object js/ipfs) "get"))





(defn remap-keys [table col]
  (reduce (fn [a k] 
    (if-let [n (get table k)] 
      (assoc (dissoc a k) n (get a k))
      a)) col (keys col)))


(defn record-dag [res]
  (let [data (remap-keys {"Hash" :id "Links" :links "CumulativeSize" :cumulative-size} res)
        data (select-keys data [:id :links :cumulative-size])]
  (pprint/pprint data)
  (om/transact! @data/RECONCILER `[(std/update-in ~{:path [:dags/by-id (:id data)] :fn #(merge % data)}) :Main])
  (save-data :dags/by-id)))

(defn image-loaded [e id]
  (om/transact! @data/RECONCILER 
    `[(std/update-in ~{:path [:meta/by-id id] 
      :fn #(assoc % :thumbnail (hyper.js/resize-dataurl (.-result (.-target e)) 32 32))}) :Main])
  (save-data :meta/by-id))

(defn file-loaded [e file]
  (log #js [e file])
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

'(

(map (fn [[_ id]]
  (ifps-object-stat id
  (comp 
    (fn [pre] (ifps-object-get id
      (comp record-dag  #(apply merge (map js->clj [pre %])) )))))) 
  (:dags @data/DATA))



)



