# display-wttr.el

Display wttr in mode line of Emacs.

`display-wttr.el` is not in GNU ELPA nor MELPA.

![display-wttr-screenshot](/display-wttr.png "display-wttr-screenshot")

# Quick start

Users could install `display-wttr.el` by downloading the .el file and then adding it their path in their `init.el`

```command-line
$ cd `into your emacs path'
$ curl -s -o display-wttr.el https://raw.githubusercontent.com/josegpt/display-wttr/main/display-wttr.el
```

```emacs-lisp
(add-to-list 'load-path "~/emacs/path")
(display-wttr-mode 1)
```

# Depends on
- curl

# use-package

```emacs-lisp
(use-package display-wttr
  ;; :custom
  ;; (display-wttr-interval (* 60 60))
  ;; (display-wttr-location "")
  ;; (display-wttr-fomart "4")
  ;; (display-wttr-fetch-executable "curl")
  ;; (display-wttr-fetch-options "-s")
  :config
  (display-wttr-mode))
```

# Configuration

Display wttr by default does not set any location. However you can set
a specific location by adding this line to `init.el` config.

```emacs-lisp
(setq display-wttr-location "New+York")
;; (setq display-wttr-location "{London,New+York}")
;; (setq display-wttr-location "London:New+York")
```

Display wttr only allows [one-line-output](https://github.com/chubin/wttr.in#one-line-output "one-line output") from [wttr.in](https://github.com/chubin/wttr.in).

```emacs-lisp
(setq display-wttr-format "4")
;; (setq display-wttr-format "%l:+%c+%t\n")
```

If would like to use another executable to fetch for [wttr.in](https://github.com/chubin/wttr.in) data; you can replace `curl` by setting this in your `init.el` with your favorite `fetcher`.

```emacs-lisp
(setq display-wttr-fetch-executable "curl")
```

Options passed to the `fetcher` can be modify.

```emacs-lisp
(setq display-wttr-fetch-options "-s")
```

The interval in which `display-wttr.el` will be updated can be modified.

```emacs-lisp
(setq display-wttr-interval (* 60 60))
```

# LICENSE
MIT
