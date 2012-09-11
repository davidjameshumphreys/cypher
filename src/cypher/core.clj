(ns cypher.core)

(defn rot13
  "Encrypt a message"
  [message]
 	(reduce str (map rotchar message)))

(defn rotchar
  "rotate char"
  [c]
	(cond
     	(Character/isUpperCase c)
     		(first (drop (+ 13 (- (int c) (int \A))) (cycle (seq "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))))
    	(Character/isLowerCase c)
     		(first (drop (+ 13 (- (int c) (int \a))) (cycle (seq "abcdefghijklmnopqrstuvwxyz"))))
      	:else c))