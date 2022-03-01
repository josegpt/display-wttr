;; Tests for display-wttr.el

(require 'ert)
(require 'display-wttr)

(ert-deftest display-wttr-fetch-url-test ()
  (should (string= (display-wttr-fetch-url) "https://wttr.in/?format=4"))
  (setq display-wttr-location "New+York")
  (setq display-wttr-format "2")
  (should (equal (display-wttr-fetch-url) "https://wttr.in/New+York?format=2")))

(setq run-hooks-called nil)
(defun run-hooks (&rest hooks)
  (setq run-hooks-called t))

(setq force-mode-line-update-called nil)
(defun force-mode-line-update (&optional all)
  (setq force-mode-line-update-called t))

(ert-deftest display-wttr-sentinel-test ()
  (setq run-hooks-called nil)
  (display-wttr-sentinel "" "")
  (setq force-mode-line-update-called nil)
  (should (equal display-wttr-string nil))
  (display-wttr-sentinel "" "finished\n")
  (setq display-wttr-list '("display-wttr"))
  (should (equal display-wttr-string "display-wttr "))
  (should (equal run-hooks-called t))
  (should (equal force-mode-line-update-called t)))


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
