name=aspeclg

install:
	mkdir -p ~/.vim/syntax && cp syntax/$(name).vim ~/.vim/syntax/$(name).vim
	mkdir -p ~/.vim/ftdetect && cp ftdetect/$(name).vim ~/.vim/ftdetect/$(name).vim
.PHONY: install

uninstall:
	rm -f ~/.vim/syntax/$(name).vim 
	rm -f ~/.vim/ftdetect/$(name).vim
.PHONY: uninstall