objects = common.beam loop.beam main.beam nets.beam publish.beam renren.beam


all: $(objects)

$(objects): %.beam: %.erl
	erlc $<

clean:
	rm -rvf *.beam *.pyc

.PHONY: clean compile
