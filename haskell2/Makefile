SOURCES_BASE = Readline.hs Types.hs Reader.hs Printer.hs
SOURCES_LISP = Env.hs Core.hs step9_try.hs
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

#####################

SRCS = $(wildcard *.hs)
OTHER_SRCS = $(wildcard core/*.hs)
BINS = $(SRCS:%.hs=%)

#####################

all: $(BINS)

dist: mal

mal: $(word $(words $(BINS)),$(BINS))
	cp $< $@

$(BINS): %: %.hs $(OTHER_SRCS)
	ghc --make $< $(OTHER_SRCS) -o $@

clean:
	rm -f $(BINS) mal *.hi *.o

.PHONY: stats stats-lisp tests $(TESTS)

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*--|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*--|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
