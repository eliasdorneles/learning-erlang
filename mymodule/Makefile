.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean

MODS = eachcons

all: test

compile: ${MODS:%=%.beam}

test: compile
	# ${ERL} -s eachcons test
	${ERL} -noshell -s eachcons test -s init stop

clean:
	$(RM) *.beam erl_crash.dump

