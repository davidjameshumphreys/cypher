(ns cypher.core)

(defn rotchar
  "rotate char"
  [c]
	(let [f #(first (drop (+ 13 (- (int c) (int %1))) (cycle (seq %2))))]
	(cond
     	(Character/isUpperCase c)
     		(f \A "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    	(Character/isLowerCase c)
     		(f \a "abcdefghijklmnopqrstuvwxyz")
      	:else c)))

(defn rot13
  "Encrypt a message"
  [message]
 	(reduce str (map rotchar message)))
