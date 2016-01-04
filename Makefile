PYTHON?=python

all: help
.PHONY : all

egg:
	${PYTHON} setup.py sdist
.PHONY : egg

dist: egg
	${PYTHON} setup.py register
	${PYTHON} setup.py upload
.PHONY : dist

py-test:
	@${PYTHON} tests/test_yasi.py
.PHONY : py-test

new-test:
	@newlisp tests/test-yasi-module.lsp
.PHONY : new-test

test: py-test new-test
.PHONY : test

tags: yasi.py
	ctags yasi.py

checks:
	pep8 yasi.py tests/test_yasi.py
	@printf "\n-------------------\n"
	pylint yasi.py tests/test_yasi.py
.PHONY : checks

clean:
	rm -rf __pycache__ tags *.pyc *.bak~ tests/cases/*.bak~
.PHONY : clean

help:
	@echo "Targets:"
	@echo " > test(new-test, py-test)"
	@echo " > checks"
	@echo " > clean"
	@echo " > egg"
	@echo " > dist"
	@echo " > tags"
.PHONY : help
