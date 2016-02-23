(ns ^:figwheel-always orbit.core)


(defmacro orbit [code]
  (let [code-str (str code)
        temp-hash (gensym "ref")]
    `(let [~'buffer (~'(.-Buffer js/ipfs) ~code-str)]
      (declare ~temp-hash)
      (~'ifps-add ~'buffer (fn [res#] (~'set! ~temp-hash (symbol (.-Name res#)))))
      (def ~temp-hash nil))))

(defmacro de-orbit [id]
  (let [temp (gensym "ref")]
    `(let []
      (declare ~temp)
      (~'ifps-cat ~(str id) (fn [res#] (set! ~temp (~'reader/read-string (~'.toString res#)))))
      (def ~temp nil))))