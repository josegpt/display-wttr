;;; display-wttr.el --- Display wttr in mode line of Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2022 Jose G Perez Taveras <josegpt27@gmail.com>
;; Version: 0.0.5

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; Heavily inspired by: display-time-mode

;;; Code:

(defgroup display-wttr nil
  "Display wttr in mode line of Emacs."
  :prefix "display-wttr-"
  :group 'mode-line)

(defcustom display-wttr-fetch-executable "curl"
  "Executable to be used by wttr to fetch information."
  :type 'string)

(defcustom display-wttr-fetch-options "-s"
  "Options to be passed to the fetch executable used by wttr."
  :type 'string)

(defcustom display-wttr-location ""
  "Display wttr location supports any combination of the `one-line output'.

Valid format values:
  Cities:
    Empty*
    Nuremberg
    Salt+Lake+City
    Nuremberg:Hamburg:Berlin
    {Nuremberg,Hamburg,Berlin}

  3-letter airport codes:
    muc
    ham
    ewr
  ~:
    ~Vostok+Station
    ~Eiffel+Tower
    ~Kilimanjaro
  @:
    @github.com
    @msu.ru

  (*If you omit the location name, you will get the report for
  your current location based on your IP address.)

  (~Add the character ~ before the name to look up that special
  location name before the weather is then retrieved:)

  (@IP-addresses (direct) or domain names (prefixed with @) to
  specify a location)

For more information on how to specify locations make sure to
check: `https://github.com/chubin/wttr.in#one-line-output'"
  :type 'string)

(defcustom display-wttr-format "4"
  "Format to be passed to wttr.
Display wttr format supports any option from thw `one-line
output' of `https://wttr.in'

Valid format values:
  Predefined format options:
    1
    2
    3
    4

  Custom format options:
    c Weather condition,
    C Weather condition textual name,
    x Weather condition, plain-text symbol,
    h Humidity,
    t Temperature (Actual),
    f Temperature (Feels Like),
    w Wind,
    l Location,
    m Moon phase 🌑🌒🌓🌔🌕🌖🌗🌘,
    M Moon day,
    p Precipitation (mm/3 hours),
    P Pressure (hPa),
    D Dawn*,
    S Sunrise*,
    z Zenith*,
    s Sunset*,
    d Dusk*,
    T Current time*,
    Z Local timezone.
    (*times are shown in the local timezone)

Examples:
  1: ☀️ +28°F
  2: ☀️ 🌡️+28°F 🌬️→7mph
  3: New York, New York, United States: ☀️ +28°F
  4: New York, New York, United States: ☀️ 🌡️+28°F 🌬️→7mph
  %l:+%c+%t\n: New York, New York, United States: ☀️ +28°F

For more information on the one-line output make sure to visit:
`https://github.com/chubin/wttr.in#one-line-output'"
  :type 'string)

(defcustom display-wttr-interval (* 60 60)
  "Seconds between updates of wttr in the mode line."
  :type 'integer)

(defcustom display-wttr-hook nil
  "List of functions to be called when the wttr is updated in the mode line."
  :type 'hook)

(defvar display-wttr-string nil
  "String used in mode line to display wttr string.
It should not be set directly, but is instead updated by the
`display-wttr' function.")
;;;###autoload(put 'display-wttr-string 'risky-local-variable t)

(defvar display-wttr-list nil
  "List of wttr unprocessed results.
This way a flash is avoided when updating `display-wttr-string'.
It should not be set directly, but is instead updated by the
`display-wttr' function.")
;;;###autoload(put 'display-wttr-list 'risky-local-variable t)

(defvar display-wttr-timer nil
  "Timer used by wttr.")
;;;###autoload(put 'display-wttr-timer 'risky-local-variable t)

(defun display-wttr-fetch-url ()
  "Format uri for wttr to be used to fetch weather from `https://wttr.in'."
  (format "https://wttr.in/%s?format=%s"
          display-wttr-location
          display-wttr-format))

(defun display-wttr-sentinel (process event)
  "Update `display-wttr-string' only when the fetcher is finished.
Argument PROCESS holds the process to which this function is
running.
Argument EVENT passes the status of the PROCESS."
  (ignore process)
  (when (string= "finished\n" event)
    (setq display-wttr-string
          (concat (string-join display-wttr-list " ") " "))
    (run-hooks 'display-wttr-hook))
  (force-mode-line-update 'all))

(defun display-wttr-filter (process string)
  "Update the `display-wttr' info in the mode line.
Argument PROCESS holds the process to which this function is
running.
Argument STRING holds each line of stdin or stderr of
the currently running process."
  (ignore process)
  (add-to-list 'display-wttr-list
               (string-join (split-string string) " ") t))

(defun display-wttr-update ()
  "Create a new background process to update wttr string."
  (make-process
   :name "display-wttr"
   :command `("sh" "-c"
              ,(format "%s %s %s"
                       display-wttr-fetch-executable
                       display-wttr-fetch-options
                       (display-wttr-fetch-url)))
   :filter 'display-wttr-filter
   :sentinel 'display-wttr-sentinel ))

(defun display-wttr-update-handler ()
  "Update wttr in mode line.
Calcalutes and sets up the timer for the next update of wttr with
the specified `display-wttr-interval'"
  (display-wttr-update)
  (let* ((current (current-time))
         (timer display-wttr-timer)
         (next-time (timer-relative-time
                     (list (aref timer 1) (aref timer 2) (aref timer 3))
                     (* 5 (aref timer 4)) 0)))
    (or (time-less-p current next-time)
        (progn
          (timer-set-time timer (timer-next-integral-multiple-of-time
                                 current display-wttr-interval)
                          (timer-activate timer))))))

;;;###autoload
(defun display-wttr ()
  "Enable display of wttr in mode line.
This display updates automatically every hour.  This runs the
normal hook `display-wttr-hook' after each update."
  (interactive)
  (display-wttr-mode 1))

;;;###autoload
(define-minor-mode display-wttr-mode
  "Toggle display of wttr.
When Display Wttr mode is enabled, it updates every hour (you can
control the number of seconds between updates by customizing
`display-wttr-interval')."
  :global t
  :group 'display-wttr
  ;; Cancel timer if any is running
  (and display-wttr-timer (cancel-timer display-wttr-timer))
  (setq display-wttr-string "")
  (setq display-wttr-list nil)
  (or global-mode-string (setq global-mode-string '("")))
  (when display-wttr-mode
    (or (memq 'display-wttr-string global-mode-string)
        (setq global-mode-string
              (append global-mode-string '(display-wttr-string))))
    ;; Set initial timer
    (setq display-wttr-timer
          (run-at-time t display-wttr-interval
                       'display-wttr-update-handler))
    (display-wttr-update)))

(provide 'display-wttr)
;;; display-wttr.el ends here
