MODULE_NAME = mustache

compile:
	find $(MODULE_NAME) -type f -name '*.rkt' | xargs raco make

link-pkg:
	raco pkg install --link -t dir $(MODULE_NAME) || true

test: link-pkg
	find $(MODULE_NAME) -type f \( -name '*.rkt' -or -name '*.yml' \) | xargs raco test -x

test-specs:
	find $(MODULE_NAME) -name '*.yml' | xargs raco test

coverage:
	raco cover -f coveralls -d $(TRAVIS_BUILD_DIR)/coverage mustache

SPECS_DIR = mustache/tests

get-specs:
	cat specs.txt | while read uri; do (echo '#lang mustache/spec\n' && curl $$uri) > $(SPECS_DIR)/$$(basename $$uri); done

clean:
	find . -type d -name 'compiled' | xargs rm -rf
