(ns cypher.core)

(defn rot13
  "Encrypt a message"
  [message]
 	(reduce str (map rotchar message))
  )

(defn rotchar
  "rotate char"
  [c]
  (let [offset (- (int c) (int \a))]
    (if (neg? offset)
      c
      (first (drop (+ 13 offset) (cycle (seq "abcdefghijklmnopqrstuvwxyz")))))))

()