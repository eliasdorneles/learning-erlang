.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean

MODS = ringbenchmark

all: test

compile: ${MODS:%=%.beam}

test: compile
	@# ${ERL} -noshell -s eachcons test -s init stop
	@echo no tests to execute

clean:
	$(RM) *.beam erl_crash.dump

