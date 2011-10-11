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
 (equal (parse/first-result 
		 (=>maybe (=>satisfies (lambda (x) (= x 10))))
		 '(10 11))
		10)
 t
 "=maybe failed to return the correct value when the parser succeeded.")

(assert 
 (equal (parse/first-result 
		 (=>maybe (=>satisfies (lambda (x) (= x 11))))
		 '(10 11))
		nil)
 t
 "=maybe failed to return the correct value when the parser failed.")

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

(assert
 (equal (parse/first-result 
		 (=>zero-plus-more->string =alpha)
		 "abCdef112314")
		"abCdef")
 t
 "=alpha failed to parse.")

(assert 
 (equal (parse/first-result
		 (=>zero-plus-more->string =alpha-upper)
		 "ABCd")
		"ABC")
 t
 "=alpha-upper parsed a lowercase character.")

(assert
 (equal (parse/first-result
		 (=>zero-plus-more->string =alpha-lower)
		 "abcD")
		"abc")
 t
 "=alpha-lower parsed a lowercase character.")

(assert
 (equal (parse/first-result
		 (=>zero-plus-more =digit)
		 "12345x")
		(list "1" "2" "3" "4" "5"))
 t
 "=digit failed to parse as expected.")

(assert 
 (equal (parse/first-result 
		 (=>string "test") "testit")
		"test")
 t
 "=string failed for a string input.")

(assert 
 (with-temp-buffer
   (insert "test it") 
   (equal (parse/first-result 
		   (=>string "test") (current-buffer))
		  "test"))
 t
 "=string failed for a buffer input.")

(assert 
 (equal (parse/first-result 
		 (=>string "test") '("test" "it"))
		"test")
 t
 "=string failed for a list input.")

(assert
 (equal (parse/first-result
		 =sign "+")
		"+")
 t
 "failed to parse + as a sign")

(assert
 (equal (parse/first-result
		 =sign "-")
		"-")
 t
 "failed to parse - as a sign")

(assert 
 (equal (parse/first-result
		 =string-of-digits->string "12345a")
		"12345")
 t
 "failed to parse a string of digits")

(assert 
 (equal (parse/first-result
		 =string-of-digits->string "a")
		"")
 t
 "failed to parse an empty string of digits")

(assert 
 (equal 
  (parse/first-result 
   =number-char "3.14159")
  3.14159)
 t
 "Failed to parse a decimal number.")

(assert 
 (equal 
  (parse/first-result
   =number 
   '(1 2 3 4))
  1)
 t
 "Failed to parse number from a list with =number.")

(assert 
 (equal 
  (parse/first-result
   =number 
   "1.27")
  1.27)
 t
 "Failed to parse number from a string with =number.")

(assert
 (equal (parse/first-result 
		 (=>this-symbol 'x) 
		 '(x y z))
		'x)
 t
 "Failed to parse 'x with =>this-symbol.")

(assert
 (equal (parse/first-result 
		 (=>this-symbol 'x) 
		 '(y z))
		nil)
 t
 "Failed to fail to parse 'x with =>this-symbol.")

(assert (parse/first-result
		 =punctuation "!abc")
		"!"
		t
		"Failed to parse a punctuation character with =punctuation.")

(assert (parse/first-result
		 =punctuation "$abc")
		"$"
		t
		"Failed to parse a punctuation character with =punctuation.")


(assert 
 (equal 'x (parse/first-result 
			(=>this-symbol 'x) '(x y z)))
 t
 "=>this-symbol failed to parse the expected symbol.")

(assert 
 (equal nil
		(parse/first-result (=>this-symbol 'y) '(x y z)))
 t
 "=>this-symbol parsed 'y when it should have failed.")

(assert 
 (equal "test" (parse/first-result (=>equal "test") 
								   '("test" "a" "thing")))
 t
 "=>equal failed to parse an equal object.")

(assert 
 (equal nil (parse/first-result (=>equal "test") 
								   '(10 "a" "thing")))
 t
 "=>equal failed to fail when it should have.")

(assert 
 (equal 'x (parse/first-result (=>eq 'x) 
							   '(x "a" "thing")))
 t
 "=>eq failed to parse an eq object.")

(assert 
 (equal nil (parse/first-result (=>eq "test") 
								'("test" "a" "thing")))
 t
 "=>eq failed to fail when it should have because strings are not EQ.")

(assert 
 (equal 
  '(a "b" 'c "d") (parse/first-result (=>n-equal 4 '(a "b" 'c "d"))
									  '(a "b" 'c "d" 10 11 12)))
 t
 "=>n-equal failed to parse EQUAL lists.")




