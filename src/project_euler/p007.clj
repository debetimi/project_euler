(ns project-euler.p007)
;;; By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
;;; What is the 10 001st prime number?



;; http://clj-me.cgrand.net/2009/07/30/everybody-loves-the-sieve-of-eratosthenes
(defn lazy-primes3
  "Christophe Grandes lazy prime implementation.
  successively adding odd multiples of discovered prime
  numbers to the sieve.  When a number is encountered that
  exists in the sieve it is removed and replaced with the next
  odd multiple of its base prime that isn't already in the sieve
  this allows the algorithm to be memory efficient"
  []
  (letfn  [(enqueue  [sieve n step]
             (let  [m  (+ n step)]
               (if  (sieve m)
                 (recur sieve m step)
                 (assoc sieve m step))))
           (next-sieve  [sieve candidate]
             (if-let  [step  (sieve candidate)]
               (-> sieve
                   (dissoc candidate)
                   (enqueue candidate step))
               (enqueue sieve candidate  (+ candidate candidate))))
           (next-primes  [sieve candidate]
             (if  (sieve candidate)
               (recur  (next-sieve sieve candidate)  (+ candidate 2))
               (cons candidate 
                     (lazy-seq  (next-primes  (next-sieve sieve candidate) 
                                             (+ candidate 2))))))]
    (cons 2  (lazy-seq  (next-primes  {} 3)))))


(defn solve [] 
  (last (take 10001 (lazy-primes3))))
