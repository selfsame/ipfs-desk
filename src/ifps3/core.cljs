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
    (cond (= :prop (-> env :ast :type))
      {:value (get @(:state env) k)}
      :else 
      {:value (read-local env k params)})))

(pdfn read [env k params]
  {k (is* :secret)}
  {:value (get-local "secret")})

(pdfn read [env k params]
  {k (is* :desktop)}
  (let [{:keys [state ast query]} env
        res (om/db->tree query @state @state)]
    (pprint/pprint [:desktop query])
  {:value res}))

(pdfn read [env k params]
  {k (is* :view/list)}
  (let [{:keys [state ast query]} env
        view (:view @state) 
        items 
        (cond 
          (keyword? view) (om/db->tree (util/by-key-vec view (vals (get @state view))) @state @state)
          (vector? view) (om/db->tree [{[:dags/by-id (last view)] [:links]}] @state @state)
            :else [])
        res (om/db->tree query @state @state)]
  {:value (:links (last (first items)))}))

(pdfn read [env k params]
  {k (is* :dags)}
  (let [{:keys [state ast query]} env]
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
  (mapv (fn [fk] 
    #_(cloud/get-object (str fk) 
    #(cb {fk (edn-> (.toString (.-Body %)))})) ) v))

(defn dispatch-send [q cb] (mapv (fn [[k v]] (send-query k v cb)) q))


(defpdfn merge-kv)

(pdfn merge-kv [k a b] [k (or b a)])

(pdfn merge-kv [k a b]
  {k (is* 'no #_:dags/by-id )}
  [k (vec (set (concat a b)))])

(defn dispatch-merge [a b]
  (into {} (mapv (fn [k] (merge-kv k (get a k) (get b k)))   
    (set (concat (keys a) (keys b))))))



(def reconciler (om/reconciler {
  :state data/DATA 
  :parser (om/parser {:read read :mutate mutate})
  :remotes [:s3 :localStorage]
  :send dispatch-send
  :logger nil 
  :pathopt true
  :merge-tree dispatch-merge}))

(cloud/load-data :meta/by-id :dags/by-id :dags)

(reset! data/RECONCILER reconciler)
 

(defui IPHash
  Object
  (render [this]
    (let [s (str (:value (om/props this)))
          [a b] (mapv (comp #(.toString % 16) #(.abs js/Math %) hash) 
                      (re-seq #"[a-zA-Z0-9]{23}" (str s)))]
    (html 
      (<span.ipfs
        (style {:border (str "0.2em solid #" (apply str (take 6 a)))}) 
        (<span.hash.noselect "#" 
          (style {:color  (str "#" (apply str (take 6 b)))}))
        (.substr s 0 1)
        (<span 
          (style {:fontSize ".001em" :letterSpacing -4.5 :opacity 0.0}) 
          (.substr s 1 39))
        (.substr s 40 46)
        (<br))))))
(def iphash (om/factory IPHash))


(defn selected? [id] ((:selection @data/DATA) id))

(defn toggle-selection 
  ([k] (toggle-selection reconciler k))
  ([this k]
  (om/transact! this `[(std/update-in ~{
    :path [:selection] 
    :fn (fn [col] (if (col k) (disj col k) (conj col k)))})])))

(defn view-set-fn [this v]
  (fn [e] (om/transact! this `[(state/conj {:view ~v }) :view])))

(defn view-path-fn [this v]
  (fn [e] (om/transact! this `[(std/update-in {:path [:view] :fn #(vec (concat % [~v]))}) :view])))



(defui File
  ;static om/Ident
  #_(ident [this props]
    [:meta/by-id (:id props)])
  static om/IQuery
  (query [this] 
    `[:id :type :size :name])
  Object
  (render [this]
    (let [props (om/props this)
          id (:id props)
          select {:path [:selection] :fn (fn [col] (if (col id) (disj col id) (conj col id)))}]
      (html
        (<div.file
          (key (:id props))
          (class (if (selected? id) "selected" ""))
          
            (<img (src (cond 
              (:dag props) "img/icons/folder.png"
              :else "img/icons/page.png"))
              (onClick (if (:dag props) (view-set-fn this (vec (concat (:view @data/DATA) [(:id props)]))) #(prn %))))
          
          (<span.hotspot 
            
            (onClick [e] (om/transact! this `[(std/update-in ~select)]))
            #_(when (:thumbnail props)
              (<img (src (:thumbnail props)) (style {:verticalAlign :middle})))
            (<span.record (:name props) "  " (str (meta props)))

            (<span.record (last (.split (str (:type props)) "/"))
              (<a  "(http)" (href (str "http://ipfs.io/ipfs/" (:id props))))
              (style {:right :14em})
              (render-count this))
            (<span.record.right (hyper.tools/format-bytes (:size props)))
            (<span.record.right (iphash {:value (:id props)}))))))))
(def file (om/factory File {:keyfn :id}))


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
            ;(onClick (fn [e] (om/transact! this `[(std/update-in ~select)])))
            (<div (style {:padding-left :1em})
              #_(map #(<pre (style {:margin :0em :font-size :10px})(:name %))
                (:links props)) ))))))
(def dag (om/factory Dag))


(defn on-drop [e]
  (let [files (js->clj (.-files (.-dataTransfer e)))
        length (.-length files)]
    (dorun (for [i (range 0 length)
                 :let [file (aget files i)]]
             (cloud/read-file file)))))


(defui Desktop
  static om/Ident
  (ident [this props] [:desktop])
  static om/IQueryParams
  (params [this] {:view-head :cwd})
  static om/IQuery
  (query [this] `[:view {?view-head ~(om/get-query File)}])
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
          view (:view props)
          view-list (cond 
            (keyword? view) (vals (get-in @data/DATA [view]))
            (vector? view) (:links (get-in @data/DATA [:dags/by-id (last view)]))
            :else [])]
      (pprint/pprint [(meta props)])
            (prn (meta (:view/list props))(meta (first (:view/list props))))
      (html
        (<div.desktop
          (render-count this)
          (<div.info 
              (style {:fontSize 14 :background :orange 
                :position :fixed :zIndex "999999" :left 107
                :top 0 :right 167 :height "1.2em" :padding :2px})
              (if (vector? view) 
                (map-indexed (fn [idx id] 
                  (<span (style {:float :left :cursor :pointer})
                    (dag (get-in @data/DATA [:dags/by-id id]))
                    (onClick (view-set-fn this (vec (take (inc idx) view)))))) view)
                (str view)))

            (<pre (map #(file (if (get-in @data/DATA [:dags/by-id (:id %) :dag]) (assoc % :dag true) %)) 
              (sort-by (comp - :size) view-list ;(:view/list props) ;view-list
                #_(map #(with-meta % {:om-path [[:meta/by-id (:id %)]]}) (:cwd props)) )))
            (if (:over (om/get-state this)) 
                (<div.filedrop "drop to add file")))))))
(def desktop (om/factory Desktop))

(defui Main
  static om/Ident
  (ident [this props] :Main)
  static om/IQueryParams
  (params [this] {})
  static om/IQuery
  (query [this] 
    `[{:dags ~(om/get-query Dag) }
      {:desktop ~(om/get-query Desktop)}
      :view/list
      :schema/by-id
      :schema/locked])
  Object
  (render [this]
    (let [props (om/props this)]

      (html
        (<div.app
          (render-count this)
          (<div.sidebar 
            (<h3 "ipfs desk")
            (<code "meta stores")(<br)
            (map 
              #(<div.selectable.keyword (key %) 
                (style {:display :inline-block :float :left :clear :both})
                (<code (str %)) (onClick (view-set-fn this %))) 
              [:meta/by-id])

            (<br)(<code "schemas")(<br)
            (map (fn [[k v]]
              (<span.selectable.keyword.schema (key (rand))
                (class (if (selected? k) "selected" ""))
                (style {:display :inline-block :float :left :clear :both})
                (onClick (fn [e] (toggle-selection this k)))
                (<code (str k))))
                (:schema/by-id props))

            (<code "dags")
            (map #(<span.selectable (key (:id %))
                (style {:display :inline-block :float :left :clear :both})
                (onClick (view-set-fn this [(:id %)]))
                (dag %)) 
              (:dags props)))
          (desktop (assoc (:desktop props) :view/list (:view/list props)))
          #_(<div.editor
            (map (fn [[k v]]
              (<div
                (data-editor (conj props {:schema [k v]}))))
            (filter (comp (:selection props) first) (:schema/by-id props))) )
          )))))



(om/add-root! reconciler Main (first ($ "#app")))

(defonce ^:private -populate
  (do (events/listen (.-body js/document) "keydown"
        #(do (identity (.-keyCode %)) (swap! data/KEYS conj (.-keyCode %))))
      (events/listen (.-body js/document) "keyup" 
        #(swap! data/KEYS disj (.-keyCode %)))))

(defn add-dag [id]
 (swap! data/DATA update-in [:dags] #(vec (distinct (concat % [[:dags/by-id id]]))))
 (cloud/save-data :dags))

;(add-dag "QmSrCRJmzE4zE1nAfWPbzVfanKQNBhp7ZWmMnEdbiLvYNh")



;(cloud/save-data :meta/by-id :dags/by-id :dags)

;TODO [ ] store meta data in localStorage | s3
;TODO [ ] transaction to unpin files
;TODO [ ] dag object ui
;TODO [ ] use mime meta to show icons
;TODO [ ] create/edit/organize dags
;TODO [ ] selections and metadata editing



;(def lib (ipfs.core/resolve "QmVfZLPJkbKfVALskNsiCzsbPtVPp2CmuiU9aaRc1ctSKA"))