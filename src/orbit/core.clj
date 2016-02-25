(ns ^:figwheel-always orbit.core)

(defmacro orbit [code]
  (let [temp-hash (gensym "ref")]
    `(let [~'buffer (~'(.-Buffer js/ipfs) ~(str code))]
      (declare ~temp-hash)
      (~'ifps-add ~'buffer 
        (fn [res#] 
          (~'set! ~temp-hash 
            (symbol (.-Name res#)))))
      (def ~temp-hash nil))))

(defmacro de-orbit [id]
  (let [temp (gensym "ref")]
    `(let []
      (declare ~temp)
      (~'ifps-cat ~(str id) (fn [res#] 
        (set! ~temp (~'reader/read-string 
          (~'.toString res#)))))
      (def ~temp nil))))

(defmacro ipjs [id]
  (let [temp (symbol id)]
    `(do (declare ~temp)
         (~'ifps3.cloud/ifps-cat ~(str id) 
         (fn [res#] (set! ~temp (~'js/eval 
          (~'.toString res#)))))
         (def ~temp nil))))