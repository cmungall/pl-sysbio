
all: tests

TESTS = apoptosis

tests: $(patsubst %,test-%,$(TESTS))

test-%:
	swipl -g "[t/$*],run_tests -> halt(0); halt(1)"
