(require 'better-monad-parse)

(assert 
 (buffer-input-p (create-buffer-input (get-buffer "*scratch*"))) t "Failed to create a buffer-input ")
(assert
 (= 1 (buffer-input-index (create-buffer-input (get-buffer "*scratch*"))))
 t
 "A fresh buffer-input should have index 1.")

(assert 
 (= 3 (buffer-input-index (set-buffer-input-index (create-buffer-input "*scratch*") 3)))
 t
 "set-buffer-input-index failed to set.")

(assert
 (let* ((bi (create-buffer-input "*scratch*"))
		(bu (incr-buffer-input-index bi))
		(i (buffer-input-index bu)))
   (= i (+ (buffer-input-index bi) 1)))
 t
 "Buffer input increment failed.")

(assert 
 (let* ((bi (create-buffer-input "*scratch*"))
		(updated (set-buffer-input-index bi 10)))
   (not (= (buffer-input-index bi) (buffer-input-index bi)))
   t
   "Buffer index failed to update functionally."))

(assert 
 (equal t (input-empty? "")) t "input-empty? failed for empty string.")
(assert 
 (equal nil (input-empty? "a")) t "input-empty? empty failed for non-empty-string.")

(assert 
 (equal t (input-empty? nil)) t "input-empty? failed for empty list.")
(assert 
 (equal nil (input-empty? '(a b c))) t "input-empty? failed for full list.")

(assert 
 (with-temp-buffer 
   (equal t (input-empty? (current-buffer))))
 t
 "Temporary buffer should be empty.")

(assert 
 (with-temp-buffer
   (insert "abc")
   (equal nil (input-empty? (current-buffer))))
 t
 "A buffer with things INSERTED should not be empty.")

(assert 
 (equal "(" (input-first "(abd"))
 t
 "Input first failed for string input.")

;;; this file must be open in emacs for this test.
(assert 
 (equal "(" (input-first (get-buffer "better-monad-parse.el")))
 t
 "Input first failed for buffer input.")

(assert 
 (equal 'a (input-first '(a b c)))
 t
 "Input first failed for list input.")

(assert 
 (equal "est" (input-rest "test"))
 t
 "input-rest failed for string input.")

(assert
 (equal '(b c d) (input-rest '(a b c d)))
 t
 "input-rest failed for list input.")

(assert 
 (equal "r" 
		(input-first (input-rest (get-buffer "better-monad-parse.el"))))
 t
 "input-rest failed for buffer/buffer-input input.")

(assert
 (equal (=nil 'anything) nil)
 t
 "=nil should always be nil.")

(assert
 (equal (=item "abcd")
		(list (cons "a" "bcd")))
 t
 "=item should parse the first character off a string.")

(assert 
 (equal (=item '(a b c d))
		(list (cons 'a '(b c d))))
 t
 "=item should parse the first item off a list.")

(assert 
 (equal (=item (get-buffer "better-monad-parse.el"))
		(list (cons "(" (input-rest (get-buffer "better-monad-parse.el")))))
 t
 "=item shoudl parse the first item off a buffer.")

(assert (equal (=rest 'anything) 
			   (list (cons 'anything 'anything)))
		t
		"=rest should put its state into the monad.")

(assert 
 (equal (funcall (=>items 3) '(a b c d e))
		(list (cons '(a b c) '(d e))))
 t
 "=>items should parse items off of a list")

(assert 
 (equal (funcall (=>items 3) "abcdef")
		(list (cons '("a" "b" "c") "def")))
 t
 "=>items should parse items off of a string.")

(assert 
 (equal (funcall (=>items 3) (get-buffer "better-monad-parse.el"))
		(list (cons '("(" "r" "e") 
					(input-rest (input-rest (input-rest (get-buffer "better-monad-parse.el")))))))
 t
 "=>items should parse items off of a buffer.")

(assert 
 (equal (parse/first-result (monadic-do monad-parse
										(items <- (=>items 4))
										(m-return items)) "abcdef")
		(list "a" "b" "c" "d"))
 t
 "Simple Monadic form parsing failed for string input.")

(assert 
 (equal (parse/first-result (monadic-do monad-parse
										(items <- (=>items 4))
										(m-return items)) (list "a" "b" "c" "d" "e" "f"))
		(list "a" "b" "c" "d"))
 t
 "Simple Monadic form parsing failed for list input.")

(assert 
 (equal (parse/first-result (monadic-do monad-parse
										(items <- (=>items->string 4))
										(m-return items)) "abcdef")
		"abcd")
 t
 "=>items->string parsing failed for string input.")

(assert
 (equal (parse/first-result (=>satisfies
							 (lambda (x) (equal x 'cat)))
							'(cat dog))
		'cat)
 t
 "Should have parsed 'cat from the list.")

(assert
 (equal (parse/first-result (=>satisfies
							 (lambda (x) (equal x 'cat)))
							'(dog cat))
		nil)
 t
 "Should have failed to parse 'cat from the list.")

(assert 
 (equal (parse/first-result (=>list =item)
							'(a b c))
		(list 'a))
 t
 "Should parse an 'a and wrap it in a list.")

(assert 
 (equal 
  (parse/first-result 
   (=>zero-plus-more 
	(=>satisfies 
	 (lambda (x) (equal 'cat x))))
   '(cat cat cat dog zebra))
  '(cat cat cat))
 t
 "Should parse only three 'cat symbols.")

(let ((=parse-cat (=>satisfies
				   (lambda (x)
					 (equal x 'cat)))))
  (assert 
   (equal 
	(parse/first-result 
	 (=>one-plus-more =parse-cat)
	 '(cat cat cat cat dog))
	'(cat cat cat cat))
   t
   "Should have parsed four cats."))

(let ((=parse-cat (=>satisfies
				   (lambda (x)
					 (equal x 'cat)))))
  (assert 
   (equal 
	(parse/first-result 
	 (=>one-plus-more =parse-cat)
	 '(zebra cat cat cat cat dog))
	nil)
   t
   "Should have parsed nil."))





