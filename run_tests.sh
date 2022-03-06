#!/bin/sh -e

EMACS="${EMACS:=emacs}"

NEEDED_PACKAGES="package-lint"

INIT_PACKAGE_EL="(progn \
  (require 'package) \
  (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
  (package-initialize) \
  (unless package-archive-contents \
     (package-refresh-contents)) \
  (dolist (pkg '(${NEEDED_PACKAGES})) \
    (unless (package-installed-p pkg) \
      (package-install pkg))))"

# Refresh package archives, because the test suite needs to see at least
# package-lint.
"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL"

# Byte-compile

"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
         -l display-wttr.el \
         -f batch-byte-compile \
         display-wttr.el

# Linting

"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
         -L . \
         --eval "(require 'package-lint)" \
         -f package-lint-batch-and-exit \
         display-wttr.el

