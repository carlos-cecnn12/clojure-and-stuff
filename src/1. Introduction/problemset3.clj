;----------------------------------------------------------
; Problem Set #3
; Date: October 04, 2019.
; Authors:
;          A01373264 Carlos Emilio Carbajal Nogues
;          A01373471 Marina Itzel Haro Hernandez
;----------------------------------------------------------


(require '[clojure.test :refer [deftest is run-tests]])
(require '[clojure.math.numeric-tower :refer [abs]])

(defn aprox=
  "Checks if x is approximately equal to y. Returns true
  if |x - y| < epsilon, or false otherwise."
  [epsilon x y]
  (< (abs (- x y)) epsilon))


(defn there-exists-one
  "Returns true if there is exactly one element
   in lst that satisfies pred, otherwise returns
   false."
  [pred lst]
  (let [x (filter pred lst)]
         (if (= 1 (count x))
           true
           false))
  )

(deftest test-there-exists-one
  (is (not (there-exists-one pos?
                             ())))
  (is (there-exists-one pos?
                        '(-1 -10 4 -5 -2 -1)))
  (is (there-exists-one neg?
                        '(-1)))
  (is (not (there-exists-one symbol?
                             '(4 8 15 16 23 42))))
  (is (there-exists-one symbol?
                        '(4 8 15 sixteen 23 42))))

(defn my-drop-while
  "Returns a list of items from lst dropping the
  initial items that evaluate to true when passed
  to f. Once a false value is encountered, the rest
  of the list is returned. Function f should accept
  one argument."
  [f lst]
  (loop [lst lst
         result ()]
    (if (not-empty lst)
      (if (f (first lst))
        (recur (rest lst)
               result)
        (recur (drop (count lst) lst)
               (concat lst result)))
      result)))


(deftest test-my-drop-while
  (is (= () (my-drop-while neg? ())))
  (is (= '(0 1 2 3 4)
         (my-drop-while
           neg?
           '(-10 -9 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4))))
  (is (= '(2 three 4 five)
         (my-drop-while
           symbol?
           '(zero one 2 three 4 five))))
  (is (= '(0 one 2 three 4 five)
         (my-drop-while
           symbol?
           '(0 one 2 three 4 five)))))

(defn bisection
  "Takes a, b, and f as arguments. It finds the
   corresponding root using the bisection method."
  [a b f]
  (loop [a  a
         b  b]
    (let [c  (/ (+ a b) 2)]
      (if (< (abs (f c)) 1e-15)
        c
        (if (neg? (* (f a) (f c)))
          (recur a
                 c)
          (recur c
                 b))))))

(deftest test-bisection
  (is (aprox= 0.0001
              3.0
              (bisection 1 4 (fn [x] (* (- x 3) (+ x 4))))))
  (is (aprox= 0.0001
              -4.0
              (bisection -5 0 (fn [x] (* (- x 3) (+ x 4))))))
  (is (aprox= 0.0001
              Math/PI
              (bisection 1 4 (fn [x] (Math/sin x)))))
  (is (aprox= 0.0001
              (* 2 Math/PI)
              (bisection 5 10 (fn [x] (Math/sin x)))))
  (is (aprox= 0.0001
              1.618033988749895
              (bisection 1 2 (fn [x] (- (* x x) x 1)))))
  (is (aprox= 0.0001
              -0.6180339887498948
              (bisection -10 1 (fn [x] (- (* x x) x 1))))))


(defn linear-search
  "Takes three arguments: a vector vct, a
  data value x, and an equality function eq-fun.
  It sequentially searches for x in vct using eq-fun
  to compare x with the elements contained in vct."
  [vct x eq-fun]
  (if (empty? vct)
    'nil
    (loop [vct vct
           p 0
           e 0]
      (if (empty? vct)
        (if (= e 0)
          'nil
          p)
        (if (eq-fun (first vct) x)
          (recur (drop (count vct) vct)
                 p
                 1)
          (recur (rest vct)
                 (inc p)
                 e))))))

(deftest test-linear-search
  (is (nil? (linear-search [] 5 =)))
  (is (= 0 (linear-search [5] 5 =)))
  (is (= 4 (linear-search
             [48 77 30 31 5 20 91 92
              69 97 28 32 17 18 96]
             5
             =)))
  (is (= 3 (linear-search
             ["red" "blue" "green" "black" "white"]
             "black"
             identical?)))
  (is (nil? (linear-search
              [48 77 30 31 5 20 91 92
               69 97 28 32 17 18 96]
              96.0
              =)))
  (is (= 14 (linear-search
              [48 77 30 31 5 20 91 92
               69 97 28 32 17 18 96]
              96.0
              ==)))
  (is (= 8 (linear-search
             [48 77 30 31 5 20 91 92
              69 97 28 32 17 18 96]
             70
             #(<= (abs (- %1 %2)) 1)))))

(defn deriv
  "Takes f and h as its arguments, and returns a
  new function that takes x as argument, and
  which represents the derivative of f given a certain value for h."
  [f h]
  (fn [a]
    (/ (- (f (+ a h)) (f a)) h)))

(defn f [x] (* x x x))
(def df (deriv f 0.001))
(def ddf (deriv df 0.001))
(def dddf (deriv ddf 0.001))

(deftest test-deriv
  (is (aprox= 0.05 75 (df 5)))
  (is (aprox= 0.05 30 (ddf 5)))
  (is (aprox= 0.05 6 (dddf 5))))

(defn newton
  "Takes f and n as its arguments, and returns the corresponding
   value of Xn. Use the deriv function from the previous problem to compute"
  [f n]
  (loop[m 0 xn 0]
    (if (= m n)
      xn
      (recur (inc m) (- xn (/ (f xn) ((deriv f 0.0001) xn))))))
  )


(deftest test-newton
  (is (aprox= 0.00001
              10.0
              (newton (fn [x] (- x 10))
                      1)))
  (is (aprox= 0.00001
              -0.5
              (newton (fn [x] (+ (* 4 x) 2))
                      1)))
  (is (aprox= 0.00001
              -1.0
              (newton (fn [x] (+ (* x x x) 1))
                      50)))
  (is (aprox= 0.00001
              -1.02987
              (newton (fn [x] (+ (Math/cos x)
                                 (* 0.5 x)))
                      5))))

(defn integral
  "Takes as arguments a, b, n, and f.
  It returns the value of the integral,
  using Simpson’s rule."
  [a b n f]
  (let [h (/ (- b a) n)]
    (loop [k 1
           result  (+ (f (+ a (* h 0)))
                      (f (+ a (* h n))))]
      (let [y (f (+ a (* k h)))]
        (if (= k n)
          (* (/ h 3)result)
          (if (= 0 (mod k 2))
            (recur (inc k)
                   (+ (* 2 y) result))
            (recur (inc k)
                   (+ (* 4 y) result))))))))


(deftest test-integral
  (is (= 1/4 (integral 0 1 10 (fn [x] (* x x x)))))
  (is (= 21/4
         (integral 1 2 10
                   (fn [x]
                     (integral 3 4 10
                               (fn [y]
                                 (* x y))))))))


(defn binary-search
  "It implements the binary search algorithm, searching
  for x in vct using the lt-fun to compare x with the elements
  contained in vct. The lt-fun should accept two arguments, a and b,
  and return true if a is less than b, or false otherwise. "
  [vtc x lt-fun])


(def small-list [4 8 15 16 23 42])

(def big-list [0 2 5 10 11 13 16 20 24 26
               29 30 31 32 34 37 40 43 44
               46 50 53 58 59 62 63 66 67
               70 72 77 79 80 83 85 86 94
               95 96 99])

(def animals ["dog" "dragon" "horse" "monkey" "ox"
              "pig" "rabbit" "rat" "rooster" "sheep"
              "snake" "tiger"])
(defn str<
  "Returns true if a is less than b, otherwise
   returns false. Designed to work with strings."
  [a b]
  (< (compare a b) 0))

(deftest test-binary-search
  (is (nil? (binary-search [] 5 <)))
  (is (= 3 (binary-search small-list 16 <)))
  (is (= 0 (binary-search small-list 4 <)))
  (is (= 5 (binary-search small-list 42 <)))
  (is (nil? (binary-search small-list 7 <)))
  (is (nil? (binary-search small-list 2 <)))
  (is (nil? (binary-search small-list 99 <)))
  (is (= 17 (binary-search big-list 43 <)))
  (is (= 0 (binary-search big-list 0 <)))
  (is (= 39 (binary-search big-list 99 <)))
  (is (nil? (binary-search big-list 12 <)))
  (is (nil? (binary-search big-list -1 <)))
  (is (nil? (binary-search big-list 100 <)))
  (is (= 5 (binary-search animals "pig" str<)))
  (is (= 0 (binary-search animals "dog" str<)))
  (is (= 11 (binary-search animals "tiger" str<)))
  (is (nil? (binary-search animals "elephant" str<)))
  (is (nil? (binary-search animals "alligator" str<)))
  (is (nil? (binary-search animals "unicorn" str<))))



(run-tests)