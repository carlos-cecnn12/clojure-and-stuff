; Problem set 1

(require '[clojure.test :refer [deftest is run-tests]])

;(defn !
 ; [n]
  ;(if (zero? n)
   ;  1
   ;  (*' n (! (dec n))))

(defn !
  [n]
  (reduce *' (range 1 (inc n))))

(defn duplicate
  [lst]
  (if (empty? lst)
    ()
    (cons (first lst)
          (cons (first lst)
                (duplicate (rest lst))))))


(defn pow
  [a b]
  (reduce *' (repeat b a)))

(defn swapper
 [x y lst]
 (map (fn [element]
        (cond
          (= x element) y
          (= y element) x
          :else element))
      lst))


(deftest test-!
  (is (= 1
         (! 0)))
  (is (= 120
         (! 5)))
  (is (= '(1 1 2 6 24 120 720 5040 40320 362880 3628800)
         (map ! (range 11))))
  (is (= 15511210043330985984000000N
         (! 25)))
  (is (= 815915283247897734345611269596115894272000000000N
         (! 40))))

(deftest test-duplicate
  (is (= '(1 1 2 2 3 3 4 4 5 5)
         (duplicate '(1 2 3 4 5))))
  (is (= ()
         (duplicate ())))
  (is (= '(a a)
         (duplicate '(a))))
  (is (= '(a a b b c c d d e e f f g g h h)
         (duplicate '(a b c d e f g h)))))

(run-tests)