clean:
	rm -rf *.tar
	cask clean-elc

test: clean
	cask exec ert-runner

test-init:
	[ ! -f .ert-runner ] && echo "-L ." > .ert-runner
	cask exec ert-runner init
