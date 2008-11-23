ESDLDIR=/usr/lib/erlang/lib/esdl-0.96.0626

all: compile

compile:
	@mkdir -p ebin
	@erl -make

clean:
	rm -f ebin/*.beam

run:
	erl -sname 'terltris' -pa $(ESDLDIR)/ebin ebin -eval 'gui:init(10,20).'
