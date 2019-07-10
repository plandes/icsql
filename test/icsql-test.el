;;; package --- Summary
;;; Commentary:
;;
;; Unit tests of icsql-test.el
;;
;;; Code:

(require 'ert)
(require 'dash)
(require 'icsql)

(ert-deftest test-load ()
  "For now, just invoke the entries list to check for function creation fails."
  (should (icsql-list)))

(ert-deftest test-download ()
  "For now, just invoke the entries list to check for function creation fails."
  (let* ((sql-icsql-path "target")
	 (jar (icsql-jar-path)))
    (should (stringp jar))
    (should (file-exists-p jar))))

(provide 'icsql-test)

;;; icsql-test ends here
