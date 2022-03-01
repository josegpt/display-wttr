;; Tests for display-wttr.el

(require 'ert)
(require 'display-wttr)

(ert-deftest happy-path-test ()
  (should (= (+ 1 2) 3)))

(defun display-wttr-update ()
  (setq display-wttr-list nil)
  (display-wttr-filter "" "display-wttr")
  (display-wttr-sentinel "" "finished\n"))

(ert-deftest display-wttr-test ()
  (display-wttr)
  (should (memq 'display-wttr-string global-mode-string))
  (should (equal display-wttr-string "display-wttr "))
  (should (equal display-wttr-list '("display-wttr")))
  (should (= (aref display-wttr-timer 4) 3600))
  (should (equal (aref display-wttr-timer 5) 'display-wttr-update-handler)))
