IMAGES := testimg

.PHONY: default
default: clean
	@find testimg -printf "%P\n" -type f -o -type l -o -type d | tar -czf testimg.tar.gz --no-recursion -C testimg -T -

.PHONY: clean
clean:
	@rm -f *.tar.gz