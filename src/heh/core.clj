(ns heh.core)

(def ^:private component-protocols
  {'init-state         `om/IInitState
   'should-update      `om/IShouldUpdate
   'will-mount         `om/IWillMount
   'did-mount          `om/IDidMount
   'will-unmount       `om/IWillUnmount
   'will-update        `om/IWillUpdate
   'did-update         `om/IDidUpdate
   'will-receive-props `om/IWillReceiveProps
   'render             `om/IRender
   'render-state       `om/IRenderState})

(def ^:private transforms
  {'init-state
  (fn [code _ dname]
    (let [bef (take 2 code)
          aft (drop 2 code)]
    (concat bef
      [(list 'set! '(.. owner -constructor -displayName) dname)
      {}]
      aft)))
  'will-mount
  (fn [code catches dname]
     (concat code 
      [(list 'heh.core/-register
        (mapv (comp first rest) catches))]))
  'did-mount
  (fn [code catches dname]
     (concat code 
      [(list 'heh.core/-goblock 'owner
        (mapv (comp vec rest) catches))]))
   'will-unmount
  (fn [code catches dname]
     (concat code 
      [(list 'heh.core/-clean 'owner)]))})

(def ^:private defaults 
  { 'init-state '(init-state [_])
    'will-mount '(will-mount [_])
    'did-mount '(did-mount [_] )
    'will-unmount '(will-unmount [_])})
    
(defmacro component [sym args & code]
  (let [sym-str (str sym)
        catches (filter #(= (first %) 'on) code)
        dfns (mapv defaults (remove (set (map first code)) (keys defaults)))
        pcols 
        (mapcat 
          #(if-let [p (component-protocols (first %))]
            [p ((transforms (first %) (fn [a b c] a)) % catches sym-str)] 
            []) 
          (concat  dfns code))] 

    `(defn ~sym ~args
       (reify
        ~'om/IDisplayName
         (~'display-name [~'_] 
          (~'.log ~'js/console ~sym-str) ~sym-str)
         ~@pcols)))) 




(def -attrs #{'onTouchMove 'onKeyDown 'onLoad 'onLoadedData 'onInput 'onChange 'onDragStart 'onDrop
              'onContextMenu 'onFocus 'href 'onClick 'onMouseUp 'onCut 'onMouseOut 'onTouchEnd 'onDrag
              'onDragExit 'ref 'onDragEnter 'onTouchCancel 'value 'onSubmit 'onMouseDown 'onMouseEnter
              'type 'src 'onTouchStart 'onBlur 'onWheel 'onPaste 'onMouseMove 'onHover 'style 'onKeyUp
              'onMouseOver 'onKeyPress 'onDragOver 'onDragLeave 'draggable 'onDoubleClick 'id 'class
              'checked 'onMouseLeave 'onDragEnd 'onCopy 'onScroll 'target 'key})


(defn attr-merge [a b]
  (cond (string? b) b
        (vector? b) (vec (concat a b))
        (sequential? b) (concat a b)
        :else (conj a b)))

(defn attr-post [m]
  (cond (empty? (:class m)) (dissoc m :class)
        :else 
        (let [_c (list 'apply 'str (list 'interpose " " (:class m)))]
          (assoc (dissoc m :class) :className (list 'clj->js _c)))))

(defn style-js [m]
  (cond (map? (:style m))
     (update-in m [:style] #(list 'clj->js %))
     :else m))

(defn safe-seq? [form] (or (list? form) (vector? form)))

(defn html? [form] 
  (and (safe-seq? form)
    (symbol? (first form))
    (:_html (meta (first form)))))

(defn meta-use [form]
  (let [mm (mapv #(:_html (meta %)) form)
        res (if (first mm)
              (concat 
                ;om.dom/div
                [(first form)]
                ;{:id "foo"}
                [(list 'clj->js (attr-post (apply merge-with attr-merge (filter map? mm))))]
                ;; dom forms are the only valid sequences 
                (map identity
                  ;#(cond (html? %) (list %)
                  ;          (safe-seq? %) (list %)
                  ;          :else (list %)) 
                  (filter #(not= '_nil %) (rest form))))
              form)]
      res))
   

(defn -attr [form]
  (let [attr (first form)
        value (first (rest form))]
    (cond (= 'class attr) (vec (rest form))
       :else value)))

(defmacro each [bound col & body]
  `(~'for [~bound ~col] (~'do ~@body)))

(defn soup [s]
  (let [[_ tag] (re-find #"\<([\w-_]+)" s)
        [_ id] (re-find #"\#([\w-_]+)" s)
        mchr (re-matcher #"\.([\w-_]+)" s)
        classes (vec (filter string? (each _ (range 10) (last (re-find mchr)))))
        mta (conj {} (if id {:id id} {}) (if-not (empty? classes) {:class classes} {}))]
    (when tag
      (with-meta
        (symbol (str "om.dom/" tag))
        {:_html mta}))))

(defn html-recur [form]
  (cond (symbol? form) (or (soup (str form)) form)
     (vector? form) (meta-use (mapv html-recur form))
     (list? form) (cond (-attrs (first form))
                           (let [res {(keyword (first form)) (-attr form)}]
                             (with-meta '_nil {:_html res}))
                         :else (meta-use (map html-recur form)))
     :else form))
 
(defmacro html [& body]
  (let [forms (html-recur body)
        form (if (= 1 (count forms))
                 (first forms)
                 (concat ['om.dom/div {}] forms))]
    `~(do form)))


