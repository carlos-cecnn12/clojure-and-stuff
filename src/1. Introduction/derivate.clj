(declare variable?
         same-variable?
         sum?
         augend
         addend
         make-sum
         product?
         multiplicand
         multiplier
         make-product
         exponentiation?
         base
         exponent
         make-exponentiation)

(defn deriv
  [exp var]
  (cond
    (number? exp)
    0

    (variable? exp)
    (if (same-variable? exp var)
      1
      0)

    (sum? exp)
    (make-sum (deriv (augend exp) var)
              (deriv (addend exp) var))

    (product? exp)
    (make-sum (make-product (multiplicand exp)
                            (deriv (multiplier exp) var))
              (make-product (multiplier exp)
                            (deriv (multiplicand exp) var)))))

(defn variable?
  [exp]
  (symbol? exp))

(defn same-variable?
  [v1 v2]
  (and (variable? v1)
       (variable? v2)
       (= v1 v2)))

(defn sum?
  [exp]
  (and (list? exp)
       (= 3 (count exp))
       (= '+ (first exp))))

(defn augend
  [exp]
  (nth exp 1))

(defn addend
  [exp]
  (nth exp 2))

(defn make-sum
  [a1 a2]
  (cond
    (= a1 0) a2
    (= a2 0) a1
    (and (number? a1)
         (number? a2)) (+ a1 a2)
    :else (list '+ a1 a2)))

(defn product?
  [exp]
  (and (list? exp)
       (= 3 (count exp))
       (= '* (first exp))))

(defn multiplicand
  [exp]
  (nth exp 1))

(defn multiplier
  [exp]
  (nth exp 2))

(defn make-product
  [m1 m2]
  (cond
    (= m1 0) 0
    (= m2 0) 0
    (= m1 1) m2
    (= m2 1) m1
    (and (number? m1)
         (number? m2)) (* m1 m2)
    :else (list '* m1 m2)))

(defn exponentiation?
  [exp]
  (and (list? exp)
       (= 3 (count exp))
       (= '** (first exp))))

(defn base
  [exp]
  (nth exp 1))

(defn exponent
  [exp]
  (nth exp 2))

(defn make-exponentiation
  [b e]
  (cond
    (= e 0) 1
    (= e 1) b
    (= b 0) 0
    (= b 1) 1
    (and (number? b)
         (number? e)) (reduce * (repeat e b)))
    :else (list '** b e))

