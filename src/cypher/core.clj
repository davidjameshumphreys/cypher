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


(defn rot-alpha [shifter]
  (let [diff (- (int shifter) (int \A))
        alphabet (cycle (map char (range (int \A) (inc (int \Z)))))]
    (take 26 (drop diff alphabet))
    )
  )

(defn shift-char [shift-amount chr]
  (let [alphabet (cycle (map char (range (int \A) (inc (int \Z)))))]
    (first (drop shift-amount alphabet))
    )
  )

(defn shift-alphabet [c]
  (let [alphabet (cycle (map char (range (int \A) (inc (int \Z)))))
        delta    (dec (- (int c) (int \A)))]
    (vec (take 26 (drop delta alphabet)))
    )
  )

(defn apply-alphabet [a str]
  (map (fn [i] (get a (inc (- (int i) (int \A))))) str)
  )

(defn reverse-alphabet [c str]
  (let [alphabet (cycle (map char (range (int \A) (inc (int \Z)))))
        delta    (- 26 (- (int c) (int \A)))]
    (vec (take 26 (drop delta alphabet)))
    )
  )

(defn reverse-alphabets [as str]
  (let [fns (map (fn [a] (partial reverse-alphabet a)) as)]
    (map apply-alphabet (cycle fns) str)))
(defn apply-alphabets [as str]
  (let [fns (map (fn [a] (partial apply-alphabet a)) as)]
    (map apply-alphabet (cycle fns) str)
    )
  )

;;

(defn position [c]
  (- (int c) (int \A)))

(def full-alphabet (vec (map char (range (int \A) (inc (int \Z))))))

(defn shift-letter [password letter]
  (let [p (position password)
        l (position letter)]
    (get full-alphabet (rem (+ p l) 26)))
  )
(defn unshift-letter [password letter ]
  (let [p (position password)
        l (position letter)]
    (get full-alphabet (rem (+ (- 26 p) l) 26))))

(defn- cycle-shift [text pass f]
  (apply
   str
   (map
    (fn [a b] (a b))
    (cycle (map (fn [a] (partial f a)) pass))
    text)
   )
  )

(defn scramble [text pass]
  (cycle-shift text pass shift-letter))

(defn unscramble [text pass]
  (cycle-shift text pass unshift-letter))

(defn find-cycles [text]
  (let [pairs (partition-all 2 1 text)
        ]
    (frequencies pairs)))
