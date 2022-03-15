;; Tests for display-wttr.el

(require 'ert)
(require 'display-wttr)

(ert-deftest display-wttr-fetch-url-test ()
  (should (string= (display-wttr--fetch-url "") "https://wttr.in/?format=4"))
  (should (string= (display-wttr--fetch-url "New+York") "https://wttr.in/New+York?format=4"))
  (should (string= (display-wttr--fetch-url "New+Jersey") "https://wttr.in/New+Jersey?format=4")))

(defun display-wttr-update ()
  (setq display-wttr-list '("display-wttr"))
  (setq display-wttr-string "display-wttr "))

(ert-deftest display-wttr-test ()
  (display-wttr)
  (should (memq 'display-wttr-string global-mode-string))
  (should (equal display-wttr-string "display-wttr "))
  (should (equal display-wttr-list '("display-wttr")))
  (should (= (aref display-wttr-timer 4) 3600))
  (should (equal (aref display-wttr-timer 5) 'display-wttr-update-handler)))
