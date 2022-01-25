#lang racket

;; This Fibonacci function follows directly from its mathematical definition.
;; It looks pretty for sure, but absolutely useless for n > 10
(define (fib n)
  (cond [(= n 1) 1]
        [(= n 2) 2]
        [else (+ (fib (- n 1))
                 (fib (- n 2)))]))

;; This version of the Fibonacci function uses caching and tail recursion
;; for greater efficiency
(define (fib-3 n)
  (define (thunk rev-seed n)
    (cond [(< n 1) 0]
          [(= n 1) (second rev-seed)]
          [(= n (length rev-seed)) (first rev-seed)]
          [else (thunk
                 (cons (+ (first rev-seed)
                          (second rev-seed))
                       rev-seed)
                 n)]))
  (thunk (list 2 1) n))

;; prints all Fibonacci number up to the n-th
(define (test-fib n)
  (for ([i (in-range 1 (+ 1 n))])
    (println (fib-3 i))))

;; test if n is divisible by factor
;; or that factor is a factor of n
(define (is-divisible-by n factor)
  (zero? (modulo n factor)))

;; lists the first n prime numbers, starting from 2
(define (first-n-primes n) ;; int->list[int]
  (define (thunk primes)
    (if (= n (length primes))
        primes
        (thunk (cons
                (for/last
                    ([i (in-naturals (+ 1 (first primes)))]
                     #:final (for/and
                                 ([j primes])
                               (not (is-divisible-by i j))))
                  i)
                primes))))
  (thunk (list 2)))

;; this version of prime number calculation uses a list crawler
;; and the sieve method
(define (first-n-primes-2 n)
  ;; the multiplier is chosen based on some prime gap thingy
  ;; with a little heuristic spice thrown in
  (define search-space
    (range 2 (* 2 n (if (< n 10)
                        5
                        (log n)))))
  (define (thunk search-pair)
    (define primes (car search-pair))
    (define next (car (cdr search-pair)))
    (define rest (cdr (cdr search-pair)))
    (cond
      [(empty? rest) (cons next primes)]
      [else (thunk (cons
                    (cons next primes)
                    (filter (lambda (p)
                              (not (is-divisible-by p next)))
                            rest)))]))
  (take-right (thunk (cons (list) search-space)) n))

;; returns all prime numbers less than n
(define (primes-below-n n)
  (define search-space (range 2 n))
  (define (thunk crawler)
    (define primes (car crawler))
    (define rest (cdr crawler))
    (cond
      [(number? rest) (cons rest primes)]
      [(empty? rest) primes]
      [else (thunk (cons
                    (cons (car rest) primes)
                    (filter (lambda (p)
                              (not (is-divisible-by p (car rest))))
                            (cdr rest))))]))
  (thunk (cons (list) search-space)))

(define (test-prime-gen)
  (time (first-n-primes 10) (void))
  (time (first-n-primes-2 10) (void)))

(define (test-prime-gen-2)
  (for/and ([i (range 2 500 2)])
    (= i (length (first-n-primes-2 i)))))

(define (euler-3 n)
  (define factors
    (filter (lambda (x) (= 0 (modulo n x)))
            (build-list (integer-sqrt n) (lambda (x) (+ 2 x)))))
  (define prime-factors
    (remove*
     (for*/list
         ([i factors]
          [j factors]
          #:when (and (> i j) (= 0 (modulo i j))))
       i)
     factors))
  (first (reverse prime-factors)))

(define (euler-3-2 n)
  (let* ([factors (build-list (integer-sqrt n)
                              (lambda (x)
                                (let ([xx (+ 2 x)])
                                  (= 0 (modulo n xx)))))]
         [thunk (lambda (crawler)
                  (let ([primes (car crawler)]
                        [next (car (cdr crawler))]
                        [rest (cdr (cdr crawler))])
                    (if (empty? rest)
                        (cons next primes)
                        (thunk (cons (cons next primes)
                                     (filter (lambda (rr)
                                               (not (= 0 (modulo rr next))))
                                             rest))))))])
    (thunk (cons (list) factors))))

(define (test-euler-3)
  (printf "Euler 3: The largest prime factor of 600851475143 is ~v~n"
          (euler-3 600851475143)))

;; test if a number is a palindrome
(define (is-palindrome n)
  (define num-string
    (number->string n))
  (define half-length
    (ceiling (/ (string-length num-string) 2)))
  (for/and ([i (in-range (+ 1 half-length))])
    (eq? (string-ref num-string i)
         (string-ref num-string (- (string-length num-string) i 1)))))

;; test if a string is a palindrome
(define (is-palindrome-2 str)
  (define rev-str (reverse (string->list str)))
  (for/and ([i str]
            [j rev-str])
    (eq? i j)))

(define (euler-4 n)
  (define largest-n-digit (- (expt 10 n) 1))
  (define palindromes
    (for*/list
        ([i (range largest-n-digit 0 -1)]
         [j (range largest-n-digit 0 -1)]
         #:when (is-palindrome-2 (number->string (* i j))))
      (* i j)))
  (apply max palindromes))

(define (test-euler-4)
  (printf "Euler 4: The largest palindrome from the product of two 3-digit numbers are: ~v~n"
          (euler-4 3)))

(define (euler-7)
  (first (first-n-primes-2 10001)))

(define (test-euler-7)
  (printf "Euler 7: The 10,001st prime number is ~v~n" (euler-7)))

(define euler-8-text
  (string-append
   "73167176531330624919225119674426574742355349194934"
   "96983520312774506326239578318016984801869478851843"
   "85861560789112949495459501737958331952853208805511"
   "12540698747158523863050715693290963295227443043557"
   "66896648950445244523161731856403098711121722383113"
   "62229893423380308135336276614282806444486645238749"
   "30358907296290491560440772390713810515859307960866"
   "70172427121883998797908792274921901699720888093776"
   "65727333001053367881220235421809751254540594752243"
   "52584907711670556013604839586446706324415722155397"
   "53697817977846174064955149290862569321978468622482"
   "83972241375657056057490261407972968652414535100474"
   "82166370484403199890008895243450658541227588666881"
   "16427171479924442928230863465674813919123162824586"
   "17866458359124566529476545682848912883142607690042"
   "24219022671055626321111109370544217506941658960408"
   "07198403850962455444362981230987879927244284909188"
   "84580156166097919133875499200524063689912560717606"
   "05886116467109405077541002256983155200055935729725"
   "71636269561882670428252483600823257530420752963450"))

(define (euler-8 n)
  (define subsets
    (for/list ([start (in-naturals)]
               [end (in-naturals n)]
               #:final (= end (string-length euler-8-text)))
      (let ([sstr (substring euler-8-text start end)])
        (for/list ([start2 n]
                   [end2 (inclusive-range 1 n)])
          (let ([cc (substring sstr start2 end2)])
            (string->number cc))))))
  (apply max
         (for/list ([subset subsets])
           (apply * subset))))

(define (test-euler-8)
  (printf "Euler 8: The highest product of 13 adjacent digits is ~v~n"
          (euler-8 13)))

(define (euler-9)
  (for*/first ([c (inclusive-range 997 334 -1)]
               [b (inclusive-range (- 1000 c 1) 2 -1)]
               #:when (let ([a (- 1000 c b)])
                        (= (sqr c) (+ (sqr a) (sqr b)))))
    ;;(* (- 1000 b c) b c)
    (list (- 1000 b c) b c)
    ))

(define (test-euler-9)
  (define trip (euler-9))
  (printf "Euler 9: The Pythagorean triplet is ~v~n" trip)
  (printf "Their product is ~v~n" (apply * trip)))

(define (euler-10 n)
  (apply + (primes-below-n n)))

(define (test-euler-10)
  (printf "Euler 10: Sum of all primes below two million is ~v~n"
          (euler-10 2000000)))

(define (test-euler-all)
  (test-euler-3)
  (test-euler-4)
  (test-euler-7)
  (test-euler-8)
  (test-euler-9))











    
