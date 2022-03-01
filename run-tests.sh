#!/bin/sh -e

EMACS="${EMACS:=emacs}"

${EMACS} --batch -l display-wttr.el -l display-wttr-test.el -f ert-run-tests-batch-and-exit
