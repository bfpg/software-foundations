all:
	$(MAKE) -C sf all

pdf:
	$(MAKE) -C sf all.pdf
	mv sf/all.pdf software-foundations.pdf
