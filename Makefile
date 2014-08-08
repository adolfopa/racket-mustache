test:
	find . \( -name '*.rkt' -or -name '*.yml' \) -exec raco test {} \+

test-specs:
	find . -name '*.yml' -exec raco test {} \+
