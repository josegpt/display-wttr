;; Tests for display-wttr

(require 'ert)
(require 'display-wttr)

(ert-deftest display-wttr-fetch-url-test ()
  (should (string= (display-wttr-fetch-url) "https://wttr.in/?format=4"))
  (should (progn
            (setq display-wttr-location "Clifton")
            (setq display-wttr-format "1")
            (string= (display-wttr-fetch-url) "https://wttr.in/Clifton?format=1"))))
