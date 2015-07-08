EMACS=emacs

clean:
	rm -rf *.tar
	cask clean-elc


test: clean
	cask exec $(EMACS) --batch \
		-l ert \
	        -l mail-pack.el \
	        -l mail-pack-rules.el \
		-l mail-pack-rules-tests.el \
		-f ert-run-tests-batch-and-exit
