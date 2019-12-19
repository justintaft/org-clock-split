EMACS=emacs

.PHONY: test test-batch

test:
	${EMACS} -Q -nw -L . -l org-clock-split.el \
	--eval "(let (pop-up-windows) (ert t))"

test-batch:
	${EMACS} -Q --batch -L . -l org-clock-split.el \
	--eval "(ert-run-tests-batch-and-exit '(not (tag interactive)))"
