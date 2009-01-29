DIRS=jscomp

all:
	for dir in $(DIRS); do \
		$(MAKE) -C $$dir all || exit; \
	done

clean:
	for dir in $(DIRS); do \
		$(MAKE) -C $$dir clean || exit; \
	done
