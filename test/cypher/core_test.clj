(ns cypher.core-test
  (:use clojure.test
        cypher.core))

#_(deftest shift-alphabet-test
  (testing "Shifting"
    (is (= (vec "ZABCDE") (take 6 (shift-alphabet \Z))))
    ))

#_(deftest apply-alphabet-test
  (testing "Applying the new alphabet"
    (is (= (vec "ZZZ") (apply-alphabet (shift-alphabet \Z) "AAA")))
    (is (= (vec "ZAZ") (apply-alphabet (shift-alphabet \Z) "ABA")))
    ))

#_(deftest multi-test
  (testing "Cycling two alphabets"
    (let [pass (map shift-alphabet "ZY")]
      (is (= (vec "ZYZ") (apply-alphabets pass "AAA")))
      )
    ))

#_(deftest two-pass-test
  (testing "Re-applying the password will provide the original text"
    (let [password "QWIJIBO"
          phrase   "IAMAROBOTFROMMARS"
          crypttext (apply-alphabets password phrase)]
      (is (= (vec phrase) (reverse-alphabets password crypttext)))
      )
    ))

#_(deftest unshift-test
  (testing "Unshifting an alphabet:"
    (= (vec "AAA") (reverse-alphabet \Z "ZZZ"))
    (= (vec "ABA") (reverse-alphabet \Z "ZAZ"))
    ))

#_(deftest unshift-cycle-test
  (testing "Unshifting with a password"))

(deftest positions
  (testing "the indexing of chars:"
    (are [orig idx] (= orig idx)
         0  (position \A)
         25 (position \Z)
         1  (position \B)
         24 (position \Y)
         )
    ))

(deftest shift-test
  (testing "Expected shifting of chars:"
    (are [orig expected pass] (= expected (shift-letter pass orig))
         \A \A \A
         \A \B \B
         )
))

(deftest unshift-test
  (testing "Unshifting of chars:"
    (are [orig expected pass] (= expected (unshift-letter pass orig))
         \A \A \A
         \B \B \A
         \Q \C \O
         )))

(deftest wiki-test
  (testing "example from wiki:"
    (let [txt "ATTACKATDAWN"
          key "LEMON"
          exp "LXFOPVEFRNHR"]
      (is (= exp (scramble txt key)))
      (is (= txt (unscramble exp key)))
      )
    ))
