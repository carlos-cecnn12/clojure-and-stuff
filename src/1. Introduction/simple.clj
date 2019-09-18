; Simple Clojure excercises

(require '[clojure.test :refer [deftest is run-tests]])
(require '[clojure.math.numeric-tower :refer [sqrt]])

(defn f2c [farDeg]
  (/(*(- farDeg 32) 5) 9))

(deftest test-f2c
  (is (= 100.0 (f2c 212.0)))
  (is (= 0.0 (f2c 32.0)))
  (is (= -40.0 (f2c -40.0))))

(defn sign [n]
  (if (> n  0)
    1
    (if (< n 0)
      -1
      0)))

(deftest test-sign
  (is (= -1 (sign -5)))
  (is (= 1 (sign 10)))
  (is (= 0 (sign 0))))

(defn roots [a b c]
  (let [tmp1 (- b)
        tmp2 (sqrt (- (* b b) (* 4 (* a c))))
        tmp3 (* 2 a)
        x1 (/ (+ tmp1 tmp2)tmp3)
        x2 (/ (- tmp1 tmp2)tmp3)]
    [x1 x2]))


(deftest test-roots
  (is (= [-1 -1] (roots 2 4 2)))
  (is (= [0 0] (roots 1 0 0)))
  (is (= [-1/4 -1] (roots 4 5 1))))

(defn bmi [w h]
  (let [ tmp (/ w (* h h))]
    (if (< tmp 20)
      'underweight
      (if (< tmp 25)
        'normal
        (if (< tmp 30)
          'obese1
          (if (< tmp 40)
            'obese2
            'obese3))))))


(deftest test-bmi
  (is (= 'underweight (bmi 45 1.7)))
  (is (= 'normal (bmi 55 1.5)))
  (is (= 'obese1 (bmi 76 1.7)))
  (is (= 'obese2 (bmi 81 1.6)))
  (is (= 'obese3 (bmi 120 1.6))))

(run-tests)

