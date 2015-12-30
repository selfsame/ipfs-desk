(ns ifps3.data)

(def KEYS (atom #{}))

(def RECONCILER (atom nil))


(defonce DATA
  {:meta/by-id {}
   :folders []
   :view :meta/by-id
   :secret nil
  })



