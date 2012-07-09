(ert-deftest group-test ()
  "Test the group function"
  (should (equal (group 2 '(1 2 3 4 5 6))
				 '((1 2) (3 4) (5 6))))
  (should (equal (group 1 '(1 2 3))
				 '((1) (2) (3)))))

(ert-deftest mkassoc-test ()
  (should (equal (mkassoc '(key val))
				 '((key . val))))
  (should (equal (mkassoc '(a 1 b 2 c 3))
				 '((a . 1) (b . 2) (c . 3)))))

(ert-deftest fn-test ()
  (should (equal (mapcar (fn (x) (+ x 10)) '(1 2 3))
				 '(11 12 13))))

