.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean

MODS = procreation

all: test

compile: ${MODS:%=%.beam}

test: compile
	${ERL} -noshell -s procreation max 100000 -s init stop
	@echo no tests to execute

clean:
	$(RM) *.beam erl_crash.dump

