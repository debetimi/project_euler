(ns project-euler.utils
  "Contains commonly used utility functions"
  (:require [clojure.math.numeric-tower :as math]
            [clojure.string :as string]))

;; Fibonacci
(def lazy-fibonacci
  "fibonacci lazy sequence"
  (letfn [(fib [a b]
            (cons a (lazy-seq (fib b (+' a b)))))]
    (fib 0 1)))

;;; String Manipulations
(defn left-shift 
  "Rotates a string left"
  [w] 
  (str (subs w 1) (subs w 0 1)))

(defn right-shift 
  "Rotates a string right"
  [w]
  (string/reverse (left-shift (string/reverse w))))

;;; Prime  Functions
(def prime-factors
  "Returns a list of prime factors of a number
   e.g (prime-factors 4) -> (2 2)"
  (let [cache (atom {})]
    (fn [number]
      (loop [n number f 2 pfacts '()]
        (cond 
          (or (contains? @cache n) (<= n 1)) (get (swap! cache assoc number (concat pfacts (get @cache n))) number)
          (> f (math/sqrt n)) (get (swap! cache assoc number (conj pfacts n)) number)
          :else (let [divisible? (zero? (rem n f))]
                  (recur (if divisible? (/ n f) n) 
                         (if divisible? f (if (= 2 f) (inc f) (+ 2 f))) 
                         (if divisible? (conj pfacts f) pfacts))))))))

(def lazy-primes
  "Christophe Grandes lazy prime implementation.
   successively adding odd multiples of discovered prime
   numbers to the sieve. When a number is encountered that
   exists in the sieve it is removed and replaced with the next
   odd multiple of its base prime that isn't already in the sieve
   this allows the algorithm to be memory efficient
   http://clj-me.cgrand.net/2009/07/30/everybody-loves-the-sieve-of-eratosthenes"
  (letfn [(enqueue [sieve n step]
            (let [m (+ n step)]
              (if (sieve m)
                (recur sieve m step)
                (assoc sieve m step))))
          (next-sieve [sieve candidate]
            (if-let [step (sieve candidate)]
              (-> sieve
                  (dissoc candidate)
                  (enqueue candidate step))
              (enqueue sieve candidate (+ candidate candidate))))
          (next-primes [sieve candidate]
            (if (sieve candidate)
              (recur (next-sieve sieve candidate) (+ candidate 2))
              (cons candidate 
                    (lazy-seq (next-primes (next-sieve sieve candidate) 
                                           (+ candidate 2))))))]
    (cons 2 (lazy-seq (next-primes {} 3)))))

(defn prime? [x] (= x (first (prime-factors x))))

(def factors
  "Returns set of factors of n"
  (memoize 
    (fn [n]
      (let [reducer (fn [factors prime] 
                      (concat factors (map (partial * prime) factors)))]
        (into (sorted-set) (reduce reducer [1] (prime-factors n)))))))

(def factors2
  "Returns a list of factors of n"
  (memoize 
    (fn [n]
      (let [power-set (fn [prime-and-freq]
                        (let [prime (key prime-and-freq)
                              freq (val prime-and-freq)]
                          (map math/expt (repeat prime) (range 1 (inc freq)))))
            reducer (fn [factors prime-power-set] 
                      (concat factors (flatten (map #(map (partial * %) factors) prime-power-set))))]
        (reduce reducer [1] (map power-set (frequencies (prime-factors n))))))))

;;; Math Operations
(defn lcm 
  "Returns the least common multiple of a and b"
  [a b]
  (/ (* a b) (math/gcd a b)))

(defn factorial 
  "Returns n!"
  [n] 
  (reduce *' (range 1 (inc n))))

(defn num->digits 
  "Returns a sequence of the digits in a number"
  [n]
  (let [char-to-digits {\0 0, \1 1,
                        \2 2, \3 3,
                        \4 4, \5 5,
                        \6 6, \7 7,
                        \8 8, \9 9}]
    (map char-to-digits (str n)))) 

(defn digits->num
  "Takes a sequence of digits and returns the number"
  [digits]
  ((comp read-string (partial apply str)) digits))
