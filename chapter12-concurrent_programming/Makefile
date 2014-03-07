.SUFFIXES: .erl .beam .yrl
NUM_PROCESSES=10000

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean

MODS = ringbenchmark

all: test

compile: ${MODS:%=%.beam}

test: compile
	@echo Running message through ring of ${NUM_PROCESSES} processes
	@${ERL} -noshell -s ringbenchmark time_a_ring ${NUM_PROCESSES} -s init stop

clean:
	$(RM) *.beam erl_crash.dump

