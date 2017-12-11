# stop gap fix until rebar3 is sorted!

.SUFFIXES: .erl .beam

.erl.beam:
	erlc $<

BEAMS=$(patsubst %.erl, %.beam, $(wildcard *.erl))

all: $(BEAMS) espace.app

espace.app: espace.app.src
	cp -vp espace.app.src espace.app

.PHONY: clean
clean:
	rm -f *.beam espace.app

.PHONY: run
run:
	erl -boot start_sasl -s observer
