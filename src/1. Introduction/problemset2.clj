;----------------------------------------------------------
; Problem Set #1
; Date: September 20, 2019.
; Authors:
;          A01373264 Pepper Pots
;          A01160611 Anthony Stark
;----------------------------------------------------------

(require '[clojure.test :refer [deftest is run-tests]])
(require '[clojure.math.numeric-tower :refer [abs]])

(defn replic
  [n lst]
  (if (empty? lst)
    ()
    (concat (repeat n (first lst)) (replic n (rest lst)))))

(defn expand
  [lst]
  (let [tmp (range 1 (+ 1 (count lst)))]
    (mapcat repeat tmp lst)))

(defn insert
  [n lst]
  (let [[less more] (split-with #(< % n) lst)]
    (concat less [n] more)))


(defn my-sort
  [lst]
  (loop [lst lst
         tmp ()]
    (empty? lst)
    tmp
    (recur (rest lst)
           (insert (first lst) tmp))))

(defn rotate-left
  [n lst]
  (if (zero? n)
    lst
    (if (empty? lst)
      ()
      (if (> n (count lst))
        (concat (first (rest (split-at (rem n (count lst)) lst)))
                (first (split-at (rem n (count lst)) lst)))
        (if (< n 0)
          (if (> (abs n) (count lst))
            (concat (first (rest (split-at (+ (count lst) (rem n (count lst))) lst)))
                    (first (split-at (+ (count lst) (rem n (count lst))) lst)))
            (concat (first (rest (split-at (+ (count lst) n ) lst)))
                    (first (split-at (+ (count lst) n) lst))))
          (concat (first (rest (split-at n lst)))
                  (first (split-at n lst))))))))

(defn binary
  [n]
  (if (zero? n)
    ()
    (concat (binary (quot n 2)) (list (rem n 2)))))

(defn gcd
  [a b]
  (if (zero? b)
    a
    (gcd b (mod a b))))

(defn insert-everywhere
  [x lst]
  (if (empty? lst)
    (list x)
    (loop [lst lst
           tmp ()
           i 0]
      (= (count lst) i)
      tmp
      (recur (inc i) (concat tmp (list (first (split-at i lst)) (list x) (first (rest (split-at i lst)))))))))




(defn pack
  [lst]
 (partition-by identity lst))

(defn compress
  [lst]
  (map first (pack lst)))

(defn encode
  [lst]
  (map #(vector (count %) (first %)) (pack lst)))

(defn encode-modified
  [lst]
  (map #(if (= 1 (first %))
          (second %)
          %)(encode lst)))

(defn decode
  [lst]
  (mapcat #(if (vector? %)
             (repeat (first %) (second %))
             (list %)) lst))


(deftest test-insert
  (is (= '(14) (insert 14 ())))
  (is (= '(4 5 6 7 8) (insert 4 '(5 6 7 8))))
  (is (= '(1 3 5 6 7 9 16) (insert 5 '(1 3 6 7 9 16))))
  (is (= '(1 5 6 10) (insert 10 '(1 5 6)))))

(deftest test-expand
  (is (= () (expand ())))
  (is (= '(a) (expand '(a))))
  (is (= '(1 2 2 3 3 3 4 4 4 4) (expand '(1 2 3 4))))
  (is (= '(a b b c c c d d d d e e e e e)
         (expand '(a b c d e)))))

(deftest test-replic
  (is (= () (replic 7 ())))
  (is (= () (replic 0 '(a b c))))
  (is (= '(a a a) (replic 3 '(a))))
  (is (= '(1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4)
         (replic 4 '(1 2 3 4)))))

(deftest test-rotate-left
  (is (= () (rotate-left 5 ())))
  (is (= '(a b c d e f g) (rotate-left 0 '(a b c d e f g))))
  (is (= '(b c d e f g a) (rotate-left 1 '(a b c d e f g))))
  (is (= '(g a b c d e f) (rotate-left -1 '(a b c d e f g))))
  (is (= '(d e f g a b c) (rotate-left 3 '(a b c d e f g))))
  (is (= '(e f g a b c d) (rotate-left -3 '(a b c d e f g))))
  (is (= '(a b c d e f g) (rotate-left 7 '(a b c d e f g))))
  (is (= '(a b c d e f g) (rotate-left -7 '(a b c d e f g))))
  (is (= '(b c d e f g a) (rotate-left 8 '(a b c d e f g))))
  (is (= '(g a b c d e f) (rotate-left -8 '(a b c d e f g))))
  (is (= '(d e f g a b c) (rotate-left 45 '(a b c d e f g))))
  (is (= '(e f g a b c d) (rotate-left -45 '(a b c d e f g)))))

(deftest test-binary
  (is (= () (binary 0)))
  (is (= '(1 1 1 1 0) (binary 30)))
  (is (= '(1 0 1 1 0 0 0 0 0 1 0 0 0 0 1 1) (binary 45123))))

(deftest test-gcd
  (is (= 1 (gcd 13 7919)))
  (is (= 4 (gcd 20 16)))
  (is (= 6 (gcd 54 24)))
  (is (= 7 (gcd 6307 1995)))
  (is (= 12 (gcd 48 180)))
  (is (= 14 (gcd 42 56))))

(deftest test-insert-everywhere
    (is (= '((1)) (insert-everywhere 1 ())))
    (is (= '((1 a) (a 1)) (insert-everywhere 1 '(a))))
    (is (= '((1 a b c) (a 1 b c) (a b 1 c) (a b c 1))
           (insert-everywhere 1 '(a b c))))
    (is (= '((1 a b c d e)
             (a 1 b c d e)
             (a b 1 c d e)
             (a b c 1 d e)
             (a b c d 1 e)
             (a b c d e 1))
           (insert-everywhere 1 '(a b c d e))))
    (is (= '((x 1 2 3 4 5 6 7 8 9 10)
             (1 x 2 3 4 5 6 7 8 9 10)
             (1 2 x 3 4 5 6 7 8 9 10)
             (1 2 3 x 4 5 6 7 8 9 10)
             (1 2 3 4 x 5 6 7 8 9 10)
             (1 2 3 4 5 x 6 7 8 9 10)
             (1 2 3 4 5 6 x 7 8 9 10)
             (1 2 3 4 5 6 7 x 8 9 10)
             (1 2 3 4 5 6 7 8 x 9 10)
             (1 2 3 4 5 6 7 8 9 x 10)
             (1 2 3 4 5 6 7 8 9 10 x))
           (insert-everywhere 'x '(1 2 3 4 5 6 7 8 9 10)))))


(deftest test-pack
  (is (= () (pack ())))
  (is (= '((a a a a) (b) (c c) (a a) (d) (e e e e))
         (pack '(a a a a b c c a a d e e e e))))
  (is (= '((1) (2) (3) (4) (5)) (pack '(1 2 3 4 5))))
  (is (= '((9 9 9 9 9 9 9 9 9)) (pack '(9 9 9 9 9 9 9 9 9)))))

(deftest test-compress
  (is (= () (compress ())))
  (is (= '(a b c d) (compress '(a b c d))))
  (is (= '(a b c a d e)
         (compress '(a a a a b c c a a d e e e e))))
  (is (= '(a) (compress '(a a a a a a a a a a)))))

(deftest test-encode
  (is (= () (encode ())))
  (is (= '([4 a] [1 b] [2 c] [2 a] [1 d] [4 e])
         (encode '(a a a a b c c a a d e e e e))))
  (is (= '([1 1] [1 2] [1 3] [1 4] [1 5])
         (encode '(1 2 3 4 5))))
  (is (= '([9 9]) (encode '(9 9 9 9 9 9 9 9 9)))))

(deftest test-encode-modified
  (is (= () (encode-modified ())))
  (is (= '([4 a] b [2 c] [2 a] d [4 e])
         (encode-modified '(a a a a b c c a a d e e e e))))
  (is (= '(1 2 3 4 5) (encode-modified '(1 2 3 4 5))))
  (is (= '([9 9]) (encode-modified '(9 9 9 9 9 9 9 9 9)))))

(deftest test-decode
  (is (= () (decode ())))
  (is (= '(a a a a b c c a a d e e e e)
         (decode '([4 a] b [2 c] [2 a] d [4 e]))))
  (is (= '(1 2 3 4 5) (decode '(1 2 3 4 5))))
  (is (= '(9 9 9 9 9 9 9 9 9) (decode '([9 9])))))

(run-tests)