test:
	find . \( -name '*.rkt' -or -name '*.yml' \) -exec raco test {} \+

test-specs:
	find . -name '*.yml' -exec raco test {} \+

SPECS_DIR = mustache/tests

get-specs:
	cat specs.txt | while read uri; do (echo '#lang mustache/spec\n' && curl $$uri) > $(SPECS_DIR)/$$(basename $$uri); done
