
(import :std/iter
		:std/srfi/1
		:std/sort)

;;

(def (compute-closest-bin-frac min-range max-range)
  (let lp ((bin (/ 1.0 2.0))
		   (acc-num 0.0)
		   (acc []))
	(let (x (+ acc-num bin))
	  (cond ((and (>= x min-range) (<= x max-range))
			 (reverse! [1 . acc]))
			((< x min-range)
			 (lp (/ bin 2.0) x [1 . acc]))
			(else (lp (/ bin 2.0) acc-num [0 . acc]))))))

(compute-closest-bin-frac 0.0122125384 0.0122125387)

;;

(def (arithmetic-multiply range-pair alist)
  (match range-pair
	([min max]
	 (let* ((diff (- max min))
			
			(alist-min (cadr (car alist)))
			(alist-max (caddr (last alist)))
			(alist-range (- alist-max alist-min))
			(alist-scale
			 (lambda (x)
			   (/ (- x alist-min)
				  alist-range)))

			(adjust
			 (lambda (x)
			   (+ (* diff (alist-scale x)) min))))
	   (map (match <>
			  ([x base range]
			   [x (adjust base) (adjust range)])
			  (x (error "unexpected alist element" x alist)))
			alist)))
	(else (error "unexpected range-pair" range-pair))))

(def (arithmetic-range alist data-seq)
  "alist is a the weight alist processed through 'cdf'. data-seq is the
data sequence to be encoded."
  (for-each (lambda (x)
			  (or (assq x alist)
				  (error "data not found in alist" x alist)))
			data-seq)
  (foldl (lambda (x acc)
		   (arithmetic-multiply (cdr (assq x acc))
								acc))
		 alist
		 data-seq))

(def (arithmetic-encoding alist data-seq)
  "alist is a the weight alist processed through 'cdf'. data-seq is the
data sequence to be encoded.
returns the encoded data-seq message"
  (let (coding-number-line (arithmetic-range alist data-seq))
	(let ((min-range (cadr (car coding-number-line)))
		  (max-range (caddr (last coding-number-line))))
	  (compute-closest-bin-frac min-range max-range))))

(def (test-arithmetic-encoding)
  (arithmetic-encoding (cdf (two-dice-sum))
					 [2 7 4 8 7]))

(def (two-dice-sum)
  (let* ((sums
		  (for/fold (r []) (x (in-range 1 7))
			(for/fold (acc r) (y (in-range 1 7))
			  (cons (+ x y) acc))))
		 (entries (sort (delete-duplicates sums) <)))
	(let (len (length sums))
	  (map (lambda (x)
			 [x
			  (/ (count (cut = <> x) sums)
				 len) ...])
		   entries))))

(def (cdf weights)
  (let lp ((weights weights) (acc []) (sum 0.0))
	(match weights
	  ([[x . prob] . weights]
	   (let (next-sum (+ sum prob))
		 (lp weights
			 [[x sum next-sum] . acc]
			 next-sum)))
	  ([] (reverse acc)))))

;; binary fractions
(def (closest-bin-frac base-10-float)
  (if (number? base-10-float)
	(let lp ((float base-10-float)
			 (acc []))
	  (let (x (* float 2))
		(match x
		  ((? (cut > <> 1.0)) (lp (- x 1.0) [1 . acc]))
		  (0.0 (reverse [0 . acc]))
		  (1.0 (reverse [1 . acc]))
		  (else (lp x [0 . acc])))))
	base-10-float))

(def (shortest-bin-frac base-10-float0 base-10-float1)
  (let lp ((x (closest-bin-frac base-10-float0))
		   (y (closest-bin-frac base-10-float1))
		   (acc []))
	(cond ((or (null? x) (null? y)) (reverse acc))
		  ((= (car x) (car y))
		   (lp (cdr x) (cdr y) (cons (car x) acc)))
		  (else (reverse (cons 1 acc))))))

(def (bin-frac-list->rep bin-frac)
  (foldl (lambda (x acc)
		   (string-append acc (if (= x 0) "0" "1")))
		 ""
		 bin-frac))

(def (test-bin-frac)
  (let ((x (closest-bin-frac 0.0122125384))
		(y (closest-bin-frac 0.0122125387)))
	(let (z (shortest-bin-frac x y))
	  (displayln "x: \n" (bin-frac-list->rep x))
	  (displayln "y: \n" (bin-frac-list->rep y))
	  (displayln "shortest bin frac: \n" (bin-frac-list->rep z)))))

;; arithmetic decoding

(def (bin-frac->decimal bin-frac)
  (let lp ((acc-num 0.5)
		   (acc 0.0)
		   (bin-frac bin-frac))
	(match bin-frac
	  ([x . bin-frac]
	   (lp (/ acc-num 2.0)
		   (if (= x 1)
			 (+ acc acc-num)
			 acc)
		   bin-frac))
	  ([] acc))))

(def (arithmetic-decoding weight-alist code len)
  "decoding the encoded message when the number of messages are known"
  (let (decimal-code (bin-frac->decimal code))
	(let lp ((n len)
			 (weight-alist weight-alist)
			 (acc []))
	  (if (= n 0)
		(reverse acc)
		(let (range (find
					 (match <>
					   ([_ min-range max-range]
						(and (>= decimal-code min-range)
							 (<= decimal-code max-range)))
					   (else (error "decoding failed")))
					 weight-alist))
		  (lp (1- n)
			  (arithmetic-multiply (cdr range) weight-alist)
			  [(car range) . acc]))))))

(def (test-arithmetic-decoding)
  (arithmetic-decoding (cdf (two-dice-sum))
					   [0 0 0 0 0 0 1 1 1 1]
					   4))