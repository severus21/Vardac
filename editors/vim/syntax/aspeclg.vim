" Vim syntax file
" Language: aspeclg
" Maintainer: Laurent Prosperi
" Latest Revision: 1/02/2022 

if exists("b:current_syntax")
  finish
endif

" Single-line comments.
syntax keyword Todo contained TODO FIXME NOTE
syntax region Comment start="//\($\|[^/]\)" end="$" contains=Todo

" Documentation comments (FIXME).
syntax include @markdown syntax/markdown.vim
syntax region Comment start="(\*" end="\*)" contains=Comment,Todo,@markdown


" Natural number.
syntax match NaturalNumber "\<[0-9]\+\>"
highlight link NaturalNumber PreProc

" String literal.
syntax match StringLiteral "["][^"]*["]"
highlight link StringLiteral String

" Builtin type
syntax Type bool float int list string void 

" Keywords
syntax keyword Keyword inport
syntax keyword Keyword outport
syntax keyword Keyword eport 
syntax keyword Keyword type

" Special symbols.
syntax match Keyword "@"
syntax match Keyword ":"
syntax match Keyword ","
syntax match Keyword ";"
syntax match Keyword "_"

" Define the default highlighting.
hi def link Comment	Comment
hi def link Todo	Todo
hi def link Type	Type
hi def link Keyword	Keyword
hi def link Abstraction	Include