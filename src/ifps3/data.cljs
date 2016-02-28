(ns ifps3.data)

(def KEYS (atom #{}))

(defonce RECONCILER (atom nil))

(defn link? [s] (and 
	(string? s)
	(= 46 (count s))
	(= "Qm" (.substr s 0 2))))

(def schema-predicates
  { :nil?      nil?
    :number?   number?     :integer?   integer? 
    :string?   string?     :vector?    vector?
    :set?      set?        :map?       map? 
    :link?     link?       :boolean?   #{true false nil}})

(defn nevermind [end s] (if (= (.substr s -1 1) end) (.substr s 0 (dec (count s))) s))

(def validator {
  :number? #(let [res (str (first (re-find #"^\-?[0-9]*(\.[0-9]*)?" (apply str (re-seq #"[\-0-9\.]" %)))))]
              (if (or (#{"" "-"} res) (= \. (last res))) 
                  res (js/parseFloat res)))
  :integer? #(apply str (re-find #"[1-9][0-9]*" ((validator :number?) %)))
  :string? identity
  :keyword? #(let [res (apply str (cons \: (re-seq #"[^\\\/\(\)\[\]\{\}\#\ W\:\,\~\`\"\']+" %)))] (if (> (count res) 1) res ""))
  :link? #(if (link? %) % "")
  :boolean? #(get {"f" "false" "t" "true"} (first (re-find #"^(f|t|false|true)" (.toLowerCase %))))
  })

(defn validate! [scheme s] 
  (cond (keyword? scheme)
    ((get validator scheme identity) s)))

(defonce DATA (atom 
  {:meta/by-id {}
   :dags/by-id {}
   :dags [
      [:dags/by-id "QmVfZLPJkbKfVALskNsiCzsbPtVPp2CmuiU9aaRc1ctSKA"]]
   :schema/by-id {
   	:std {
      :size :integer? 
      :type :string? 
      :modified :string? 
      :name :string? 
      :id :link?}
    :providence {
      :role :keyword?
      :width :integer?
      :dpi :number?
      :mine :boolean? 
      :generated :boolean?
      :application [:link?]}
   }
   :schema/locked {:std #{:id :modified :size}}
   :folders []
   :view :meta/by-id
   :secret nil
   :selection #{:std}
  }))



