(defparameter *small-primes*
  '(2 3))

(defun factor (n)
  (append
    (loop with primes = *small-primes*
              for p = (car primes)
              while p
              until (= n 1)
              if (zerop (mod n p)) 
                collect p and do (setf n (floor n p))
              else do (setf primes (cdr primes)))
    (loop for m = n then (floor m f)
          until (= m 1)
          until (primep m)
          for f = (flet ((next (x) (mod (1+ (expt x 2)) m)))
                    (loop for x = 2 then (next x)
                          for y = x then (next (next y))
                          for d = 1 then (gcd (- y x) m)
                          do (print (list x y d))
                          while (= d 1)
                          finally (return d)))
          unless (= 1 f) collect f)))

