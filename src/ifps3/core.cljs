(ns ^:figwheel-always ifps3.core
  (:require 
    [goog.events :as events]
    [om.next :as om :refer-macros [defui]]
    [om.dom :as dom]
    [heh.core :as heh :refer [private private!] :refer-macros [html]]
    [dollar.bill :as $ :refer [$]]
    [pdf.core :refer [and* or* not*] :refer-macros [defpdf pdf]]
    [ifps3.data :as data]
    [ifps3.cloud :as cloud])
  (:use 
    [ifps3.util :only [clog ->edn edn-> put-local get-local format-bytes]]
    [cljs.pprint :only [pprint]]))

(enable-console-print!)
(set! om/*logger* nil)

(defn is* [v] #(= v %))

(defn render-count 
  ([this] (render-count this {}))
  ([this opts]
  (html (<span.render-count (style (conj {:zIndex 99999} (or opts {})))
    (str (:render-count (heh.core/private! this :render-count inc)))))))


(defn get-normalized [state key]
  (let [st @state]
    (into [] (map #(get-in st %)) (get st key))))

(defn read-local
  [{:keys [query ast state] :as env} k params]
    (if (om/ident? (:key ast))
      (get-in @state (:key ast))
      (om/db->tree query (get @state k) @state)))

 
(defpdf read)

(pdf read [env k params]
  (let [{:keys [state ast]} env]
    {:value (read-local env k params)}))

(pdf read [env k params]
  {k #{:view}}
  {:value (get-in @(:state env) [k])})

(pdf read [env k params]
  {k (or* (is* :meta/by-id))}
  (let [{:keys [state ast]} env
        local (get-in @state [k])]
  (merge {:value local}
    (if (= {} local)
        {:localStorage ast}))))

(pdf read [env k params]
  {k (is* :folders)}
  {:value (into {}
    (filter #(= 1 (:type (last %))) 
      (get-in @(:state env) [:meta/by-id])))})

(pdf read [env k params]
  {k (is* :secret)}
  {:value (get-local "secret")})

(pdf read [env k params]
  {k (is* :dags/by-id)}
  (let [{:keys [state ast]} env
         local (get-in @state [k])]
  (merge {:value local}
    (if (nil? local) {:s3 ast :localStorage ast}))))


(defpdf mutate)

(pdf mutate [env k props] {:action #()})

(pdf mutate [env k props]
  {k (is* 'db/add)}
  {:action (fn [] (swap! (:state env) update-in [:meta/by-id] 
    #(conj % props)))})

(pdf mutate [env k props]
  {k (is* 'state/conj)}
  {:action (fn [] (swap! (:state env) #(conj % props)))})


(defpdf send-query)

(pdf send-query [k v cb]
  {k (is* :localStorage)}
  (cb (into {} (map #(vector % (edn-> (get-local (str %)))) v))))

(pdf send-query [k v cb]
  {k (is* :s3)}
  (mapv (fn [fk] (cloud/get-object (str fk) 
    #(cb {fk (edn-> (.toString (.-Body %)))}))) v))

(defn dispatch-send [q cb] (mapv (fn [[k v]] (send-query k v cb)) q))


(defpdf merge-kv)

(pdf merge-kv [k a b] [k (or b a)])

(pdf merge-kv [k a b]
  {k (is* :dags/by-id)}
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

(reset! data/RECONCILER reconciler)






(defui IPHash
  Object
  (render [this]
    (let [s (:value (om/props this))
          [a b] (mapv (comp #(.toString % 16) #(.abs js/Math %) hash) 
                      (re-seq #"[a-zA-Z0-9]{23}" s))]
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


(defui File
  static om/Ident
  (ident [this props]
    [:meta/by-id (:id props)])
  static om/IQuery
  (query [this] 
    `[:id
      :type])
  Object
  (render [this]
    (let [props (om/props this)
          exit-url (str "https://ipfs.io/ipfs/" (:id props))]
      (html
        (<div
          (iphash {:value (:id props)}) " "
          (<a (href exit-url)   
            "(link)"
            #_(<img 
              (style { :margin "0.3em" :maxHeight "4em" :verticalAlign :middle})
              (src exit-url))
            (target "_blank"))
          "  " (:name props)
          (style {:width 550}))))))
(def file (om/factory File))

(defn name->path [s] (map last (re-seq #"([^\\]+)[\\]*" s)))

(defn format-dag-obj [o]
  {:id (get o "Hash") :name (last (name->path (get o "Name"))) :type (get o "Type")})

(defn normalize-dag [o]
  (let [o (js->clj o)
        res (into {}
    (map 
      (comp #(vector (:id %) %) format-dag-obj)
      (get-in o ["Objects" 0 "Links"])))]
  ;TODO db/add by-id map instead of single v
  (om/transact! reconciler
    `[(db/add ~res) :folders/by-id])
  (cloud/save-data :meta/by-id :folders/by-id)))

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
      :dags/by-id
      {:folders [:id :name]}
      :view])

  Object
  (componentDidMount [this]
    (clog this)
    (.addEventListener js/document "drop"
      (fn [e] (on-drop e)
              (om/set-state! this {:over false}) ))
    (.addEventListener js/window "dragenter"
      (fn [e] (.stopPropagation e) (.preventDefault e)
              (om/set-state! this  {:over true}) ))
    (.addEventListener js/window "mouseout"
      (fn [e] (.preventDefault e))))

  (render [this]
    (let [props (om/props this)
          params (om/get-params this)]
    (let [view (:view props)
          ;TODO less bad
          view-list (cond (keyword? view) (vals (view props))
                          (vector? view) [(get-in props view)])]
      (html
        (<div.app
          (render-count this)
          (<div.sidebar 
            (<h3 "local ipfs store")
            (<code "meta")(<br)
            (map 
              #(<div.selectable.keyword (key (rand)) (<code (str %)) (onClick (view-set-fn %))) 
              [:meta/by-id :folders])

            (<br)(<code "dags")
            (map #(<span.selectable (key (rand)) (<br)
                (onClick (fn [e] (cloud/ifps-ls % normalize-dag)))
                (iphash {:value %})) 
              (:dags/by-id props))

            (<br)(<code "folders")
            (map #(<span.selectable (key (rand)) (<br)
                (onClick (view-set-fn [:folders (:id %)]))
                (iphash {:value (:id %)})
                (<code (:name %))) 
                (vals (:folders props))))
          (<div.desktop
            (<div.info 
              (style {:fontSize 14 :background :orange :position :absolute
                :left 8 :top 0 :right 0 :height "1.2em" })
              (str view))
          (<pre (map file view-list))
          (if (:over (om/get-state this)) 
              (<div.filedrop "drop to add file")))))))))



(om/add-root! reconciler Main (first ($ "#app")))

(defonce ^:private -populate
  (do (events/listen (.-body js/document) "keydown"
        #(do (prn (.-keyCode %)) (swap! data/KEYS conj (.-keyCode %))))
      (events/listen (.-body js/document) "keyup" 
        #(swap! data/KEYS disj (.-keyCode %)))))

;(pprint (:folders/by-id @(om/app-state reconciler)))
;(cloud/ifps-ls "QmSVPzx65kWgq63vokQR937RSusm979n1cqNSDhCCKYAD1" normalize-dag)
#_(put-local ":dags/by-id" (->edn ["QmTyijkMMBPxdcM853nmzhrWQLMxZLWH1Z1LxUe5QiTSK5" 
  "QmPki1KPEa59Xwyw3uUJ232t77DQ8QRVzJcRkUgcy1Wq9t"
  "QmaDs3XVVM19BwTAAqQcz1fPWhqTM1qhtrY2ZN68rjudyz"
  "QmdVteSDv4p2T5wuMc52sDftren5Whvcs3wSQqZe2QXVH4"
  "QmPUHKXhURnTeHrwpuQ8C8seWYcybhwkNrJZZs3cP4eBhY"]))
;(cloud/put-object ":dags/by-id" (->edn ["QmPuvQ5nttHZA95CKdt8155WXDvJ1yew7hG9kLCXwaAeGr"]) {} clog)

;TODO [x] store meta data in localStorage | s3
;TODO [ ] transaction to unpin files
;TODO [ ] dag object ui
;TODO [ ] use mime meta to show icons
;TODO [ ] create/edit/organize dags
;TODO [ ] selections and metadata editing

