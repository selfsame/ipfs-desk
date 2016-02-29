(ns ^:figwheel-always ifps3.core
  (:require 
    [goog.events :as events]
    [om.next :as om :refer-macros [defui]]
    [om.dom :as dom]
    [hyper.terse :refer [private private!] :refer-macros [html]]
    [hyper.js :refer [ put-local get-local]]
    [hyper.tools]
    [dollar.bill :as $ :refer [$]]
    [pdfn.core :refer [and* or* not* is*] :refer-macros [defpdfn pdfn]]
    [ifps3.data :as data]
    [ifps3.util :as util]
    [ipfs.core]
    [ifps3.cloud :as cloud]
    [cljs.pprint :as pprint])
  (:use 
    [ifps3.util :only [->edn edn->]]))

(enable-console-print!)
(set! om/*logger* nil)

(defn render-count 
  ([this] (render-count this {}))
  ([this opts]
  (html (<span.render-count (style (conj {:zIndex 99999} (or opts {})))
    (str (:render-count (private! this :render-count inc)))))))



(defn read-local
  [{:keys [query ast state] :as env} k params]
    (if (om/ident? (:key ast))
      (get-in @state (:key ast))
      (om/db->tree query (get @state k) @state)))
 
(defpdfn read)

(pdfn read [env k params]
  (let [{:keys [state ast]} env]
    ;(prn k (:ast env) (keys env))
    (cond (= :prop (-> env :ast :type))
      {:value (get @(:state env) k)}
      :else 
      {:value (read-local env k params)})))

(pdfn read [env k params]
  {k #{:view}}
  {:value (get-in @(:state env) [k])})

(pdfn read [env k params]
  {k (is* :meta/by-id)}
  (let [{:keys [state ast]} env
        local (get-in @state [k])]
  (merge {:value local}
    (if (= {} local)
        {:localStorage ast} {}))))

(pdfn read [env k params]
  {k (is* :secret)}
  {:value (get-local "secret")})

(pdfn read [env k params]
  {k (is* :selection)}
  {:value (get @(:state env) k)})

(pdfn read [env k params]
  {k (is* :dags)}
  (let [{:keys [state ast query]} env]
    ;TODO local and remote
    (prn (get @state k) (map (comp (:dags/by-id @state) last) (get @state k)))
  {:value (om/db->tree query (get @state k) @state)}))


(defpdfn mutate)

(pdfn mutate [env k props] {:action #()})

(pdfn mutate [env k props]
  {k (is* 'db/add)}
  {:action (fn [] (swap! (:state env) update-in [:meta/by-id] 
    #(conj % props)))})

(pdfn mutate [env k props]
  {k (is* 'state/conj)}
  {:action (fn [] (swap! (:state env) #(conj % props)))})


(pdfn mutate [env k props]
  {k (is* 'std/update-in)}
  {:action (fn [] (swap! (:state env) update-in (:path props) (:fn props)))})

(defpdfn send-query)

(pdfn send-query [k v cb]
  {k (is* :localStorage)}
  (cb (into {} (map #(vector % (edn-> (get-local (str %)))) v))))

(pdfn send-query [k v cb]
  {k (is* :s3)}
  (mapv (fn [fk] #_(cloud/get-object (str fk) 
    #(cb {fk (edn-> (.toString (.-Body %)))}))) v))

(defn dispatch-send [q cb] (mapv (fn [[k v]] (send-query k v cb)) q))


(defpdfn merge-kv)

(pdfn merge-kv [k a b] [k (or b a)])

(pdfn merge-kv [k a b]
  {k (is* 'no #_:dags/by-id)}
  [k (vec (set (concat a b)))])

(defn dispatch-merge [a b]
  (into {} (mapv (fn [k] (merge-kv k (get a k) (get b k)))   
    (set (concat (keys a) (keys b))))))



(def reconciler (om/reconciler {
  :state data/DATA 
  :parser (om/parser {:read read :mutate mutate})
  :remotes [:s3 :localStorage]
  :send dispatch-send
  :merge-tree dispatch-merge}))

(cloud/load-data :meta/by-id :dags/by-id)

(reset! data/RECONCILER reconciler)


(defui IPHash
  Object
  (render [this]
    (let [s (:value (om/props this))
          [a b] (mapv (comp #(.toString % 16) #(.abs js/Math %) hash) 
                      (re-seq #"[a-zA-Z0-9]{23}" (str s)))]
    (html 
      (<span.ipfs
        (style {:border (str "0.2em solid #" (apply str (take 6 a)))}) 
        (<span.hash.noselect "#" 
          (style {:color  (str "#" (apply str (take 6 b)))}))
        (.substr s 0 1)
        (<span 
          (style {:fontSize ".001em" :letterSpacing -3.5 :opacity 0.0}) 
          (.substr s 1 39))
        (.substr s 40 46)
        (<br))))))
(def iphash (om/factory IPHash))


(defn selected? [id] ((:selection @data/DATA) id))

(defn toggle-selection [k]
  (om/transact! reconciler `[(std/update-in ~{
    :path [:selection] 
    :fn (fn [col] (if (col k) (disj col k) (conj col k)))}) :selection]))

(defui File
  static om/Ident
  (ident [this props]
    [:meta/by-id (:id props)])
  static om/IQuery
  (query [this] 
    `[:id :type])
  Object
  (render [this]
    (let [props (om/props this)
          id (:id props)
          select {:path [:selection] :fn (fn [col] (if (col id) (disj col id) (conj col id)))}]
      (html
        (<div.file
          (class (if (selected? id) "selected" ""))
          (iphash {:value (:id props)})
          (<span.hotspot 
            (onClick (fn [e] (om/transact! reconciler `[(std/update-in ~select) :selection])))
            #_(when (:thumbnail props)
              (<img (src (:thumbnail props)) (style {:verticalAlign :middle})))
            (<span.record (:name props))
            (<span.record (last (.split (str (:type props)) "/")))
            (<span.record.right (hyper.tools/format-bytes (:size props)))))))))
(def file (om/factory File))


(defn name->path [s] (map last (re-seq #"([^\\]+)[\\]*" s)))

(defn format-dag-obj [o]
  {:id (get o "Hash") :name (last (name->path (get o "Name"))) :type (get o "Type")})

(defn normalize-dag [o]
  (let [o (js->clj o)
        res (into {} (map 
              (comp #(vector (:id %) %) format-dag-obj)
              (get-in o ["Objects" 0 "Links"])))]
  ;TODO db/add by-id map instead of single v
  (om/transact! reconciler `[(db/add ~res) :folders/by-id])
  (cloud/save-data :meta/by-id :dags/by-id)))



(defui DataEditor
  Object
  (render [this]
    (let [props (om/props this)
          [schema-id schema-spec] (:schema props)
          selection (remove nil? (set (map #(get-in @data/DATA [:meta/by-id %]) (:selection props))))
          validate (fn [scheme] 
            (fn [e] (set! (.-value (.-target e)) 
                          (data/validate! scheme (.. e -target -value)))))]
    (html 
      (<h3 (str schema-id))
      (<pre (with-out-str (pprint/pprint schema-spec)))
      (map 
        (fn [[k v]] 
          (let [values (vec (set (map k selection)))
                best (first values)
                other (rest values)
                other? (pos? (count other))
                locked ((or (get-in props [:schema/locked schema-id]) #{}) k)]
          (if locked 
            (<code (str k) (style {:color :white :background :gray :display :block}) 
              (<span.right (prn-str best) (if other? "..") (style {:width "70%" :text-align :right :overflow :hidden})))
            (<div (str k) (str locked)
              (style {:background :silver}) 
              (<input (style {:float :right :width :50%})
                (onBlur (validate v))
                (onChange (validate v)))
              (when other
                    (<pre (style {:margin "0"})
                      (<span.multi-value (interpose "\n" other))))))))
        schema-spec)))))
(def data-editor (om/factory DataEditor))


(defui Dag
  static om/Ident
  (ident [this props]
    [:dags/by-id (:id props)])
  static om/IQuery
  (query [this] 
    `[:id :links])
  Object
  (render [this]
    (let [props (om/props this)
          id (:id props)
          select {:path [:selection] :fn (fn [col] (if (col id) (disj col id) (conj col id)))}]
      (html
        (<div.dag
          (class (if (selected? id) "selected" ""))
          (iphash {:value (:id props)})
          (<span.hotspot) 
            (onClick (fn [e] (om/transact! reconciler `[(std/update-in ~select) :selection])))
            (<div (style {:padding-left :1em})
              (map #(<pre (iphash {:value (get % "Hash")})#_(get % "Name"))
                (:links props))))))))
(def dag (om/factory Dag))


(defn on-drop [e]
  (let [files (js->clj (.-files (.-dataTransfer e)))
        length (.-length files)]
    (dorun (for [i (range 0 length)
                 :let [file (aget files i)]]
             (cloud/read-file file)))))

(defn view-set-fn [v]
  (fn [e] (om/transact! reconciler `[(state/conj {:view ~v }) :view])))

(defui Main
  static om/Ident
  (ident [this props] :Main)
  static om/IQueryParams
  (params [this] {})
  static om/IQuery
  (query [this] 
    `[:meta/by-id
      {:dags ~(om/get-query Dag) }
      :selection
      :schema/by-id
      :schema/locked
      :view])
  Object
  (componentDidMount [this]
    (.addEventListener js/document "drop"
      (fn [e] (on-drop e) (om/set-state! this {:over false}) ))
    (.addEventListener js/window "dragenter"
      (fn [e] (.stopPropagation e) (.preventDefault e) (om/set-state! this  {:over true}) ))
    (.addEventListener js/window "mouseout"
      (fn [e] (.preventDefault e))))
  (render [this]
    (let [props (om/props this)
          params (om/get-params this)]
    (let [view (:view props)
          ;TODO less bad

          view-list (cond (keyword? view) (vals (view props))
                          (vector? view) [(get-in props view)]
                          :else [])]
      (pprint/pprint view)
      (html
        (<div.app
          (render-count this)
          (<div.sidebar 
            (<h3 "local ipfs store")
            (<code "meta stores")(<br)
            (map 
              #(<div.selectable.keyword (key %) 
                (style {:display :inline-block :float :left :clear :both})
                (<code (str %)) (onClick (view-set-fn %))) 
              [:meta/by-id])

            (<br)(<code "schemas")(<br)
            (map (fn [[k v]]
              (<span.selectable.keyword.schema (key (rand))
                (class (if (selected? k) "selected" ""))
                (style {:display :inline-block :float :left :clear :both})
                (onClick (fn [e] (toggle-selection k)))
                (<code (str k))))
                (:schema/by-id props))

            (<code "dags")
            (map #(<span.selectable (key (:id %))
                (style {:display :inline-block :float :left :clear :both})
                (onClick (view-set-fn [:dags/by-id %]))
                
                (iphash {:value (:id %)})) 
              (:dags props)))

          (<div.desktop
            (<div.info 
              (style {:fontSize 14 :background :orange :position :absolute
                :left 8 :top 0 :right 0 :height "1.2em" })
              (str view))
            (<pre (map file view-list))
            (if (:over (om/get-state this)) 
                (<div.filedrop "drop to add file")))
          (<div.editor
            (map (fn [[k v]]
              (<div
                (data-editor (conj props {:schema [k v]}))))
            (filter (comp (:selection props) first) (:schema/by-id props)))
            )))))))



(om/add-root! reconciler Main (first ($ "#app")))

(defonce ^:private -populate
  (do (events/listen (.-body js/document) "keydown"
        #(do (identity (.-keyCode %)) (swap! data/KEYS conj (.-keyCode %))))
      (events/listen (.-body js/document) "keyup" 
        #(swap! data/KEYS disj (.-keyCode %)))))

;(pprint (:folders/by-id @(om/app-state reconciler)))

#_(put-local ":dags/by-id" (->edn ["QmTyijkMMBPxdcM853nmzhrWQLMxZLWH1Z1LxUe5QiTSK5" 
  "QmPki1KPEa59Xwyw3uUJ232t77DQ8QRVzJcRkUgcy1Wq9t"
  "QmaDs3XVVM19BwTAAqQcz1fPWhqTM1qhtrY2ZN68rjudyz"
  "QmdVteSDv4p2T5wuMc52sDftren5Whvcs3wSQqZe2QXVH4"
  "QmPUHKXhURnTeHrwpuQ8C8seWYcybhwkNrJZZs3cP4eBhY"]))
;(cloud/put-object ":dags/by-id" (->edn ["QmPuvQ5nttHZA95CKdt8155WXDvJ1yew7hG9kLCXwaAeGr"]) {} clog)

(cloud/save-data :meta/by-id :dags/by-id)

;TODO [ ] store meta data in localStorage | s3
;TODO [ ] transaction to unpin files
;TODO [ ] dag object ui
;TODO [ ] use mime meta to show icons
;TODO [ ] create/edit/organize dags
;TODO [ ] selections and metadata editing



;(def lib (ipfs.core/resolve "QmVfZLPJkbKfVALskNsiCzsbPtVPp2CmuiU9aaRc1ctSKA"))