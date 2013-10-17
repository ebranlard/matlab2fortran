
VERSION = $(shell git describe)
REVDATE = $(shell git show -s --format="%ci"|awk '{print $$1}')
PROG=matlab2fortran

all: deploy
	

deploy:release git upload reference


release:
	@echo "--- Making release"
	@echo "$(REVDATE)"
	# Preparing the doc
	@cp dev/README.md README.md
	@sed -i 's/VERSIONNUMBER/'$(VERSION)'/g' README.md
	sed -i 's/LASTREVISION/'$(REVDATE)'/g' README.md
	@sed 's/^/\% /g' README.md | cat >README_TMP
	# packing in a single matlab file..
	@cat README_TMP dev/$(PROG).m dev/f*.m > $(PROG).m
	@rm README_TMP
	# done

git:
	# git commit
	@git commit -am "New version $(VERSION)"
	@git push
upload:
	@echo "--- Uploading to server"
	@./upload.sh


test:
	@echo "--- Making tests"
	# running tests
	@echo "matlab2fortran('matlab2fortran.m');quit;" | octave --traditional 
	@mv matlab2fortran.f90 matlab2fortran_octave.f90
	@echo "matlab2fortran('matlab2fortran.m');quit;"|matlab -nojvm
	@mv matlab2fortran.f90 matlab2fortran_matlab.f90
	@mv matlab2fortran_* tests
	@diff -y --suppress-common-lines --report-identical-files tests/matlab2fortran_octave.f90 tests/matlab2fortran_matlab.f90
	@diff -y --suppress-common-lines --report-identical-files tests/matlab2fortran_matlab.f90 tests/matlab2fortran_reference.f90

reference:
	@cp tests/matlab2fortran_matlab.f90 tests/matlab2fortran_reference.f90
