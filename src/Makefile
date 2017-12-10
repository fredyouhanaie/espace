# stop gap fix until rebar3 is sorted!

.SUFFIXES: .erl .beam

.erl.beam:
	erlc $<

BEAMS=$(patsubst %.erl, %.beam, $(wildcard *.erl))

all: $(BEAMS)
