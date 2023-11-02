(require 'alexandria)
(use-package :alexandria)

(defparameter *small-primes*
  '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 
    127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 
    257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 
    401 409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499 503 509 521 523 541))

(defun factorise (n)
  "Find prime factors of n."
  (multiple-value-bind (acc n) (small-factorise n)
    (if n
        (rho-factorise n acc)
        acc)))

(defun small-factorise (n &optional (small-primes *small-primes*) acc)
  "Find prime factors of n by searching a list.
   Return a list of prime factors and either a composite, n, or nil"
  (if-let (small-primes (rest-if (lambda (p) (zerop (mod n p))) small-primes))
    (let* ((p (car small-primes))
           (acc (cons p acc)))
      (if (= n p)
          (values acc nil)
          (small-factor (floor n p) small-primes acc)))
    (values acc n)))

(defun rest-if (predicate seq)
  "Return the subseq from the first element in seq which satisfies predicate to the end.
   (Copied from my utils package)"
  (when-let ((i (position-if predicate seq)))
    (subseq seq i)))

(defun rho-factorise (n &optional acc)
  "Find prime factors of n using Pollard's Rho algorithm."
  (if (primep n)
      (cons n acc)
      (let ((d (rho-factor n 2)))
        (rho-factorise (floor n d) (cons d acc)))))

(defun rho-factor (n &optional (x (random n)) (y x))
  "Find a single prime factor of n."
  (format t "Factorising n = ~d:~%" n)
  (flet ((next (x) (mod (1+ (expt x 2)) n)))
    (let* ((x (next x))
           (y (next (next y)))
           (d (gcd (- y x) n)))
      (format t "  x = ~d, y = ~d, gcd(y-x, n) = ~d~%"  x y d)
      (if (= d 1)
        (rho-factor n (next x) (next (next y)))
        (if (= d n)
            (rho-factor n)
            (if (primep d) d (rho-factor d)))))))

(defun primep (n)
  (or (find n *small-primes*)
      (loop repeat 50
            for base = (+ (random (- n 2)) 2) 
            always (= 1 (mod (expt base (1- n)) n)))))
