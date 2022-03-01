;; Tests for display-wttr.el

(require 'ert)
(require 'display-wttr)

(ert-deftest display-wttr-fetch-url-test ()
  (should (string= (display-wttr-fetch-url) "https://wttr.in/?format=4"))
  (setq display-wttr-location "New+York")
  (setq display-wttr-format "2")
  (should (equal (display-wttr-fetch-url) "https://wttr.in/New+York?format=2")))


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
