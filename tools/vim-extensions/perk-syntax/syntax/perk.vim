" Vim syntax file for Perk language
" Derived from VSCode grammar
" Maintainer: axdelafuen

if exists("b:current_syntax")
  finish
endif

" Keywords
syn keyword perkKeyword let import open extern archetype model struct private public fun for while do if then else return summon banish make cast of just nothing

" Types
syn keyword perkType int void uint8_t uint16_t uint32_t uint64_t float double bool char

" Comments
syn match   perkComment "//.*" contains=perkTodo
syn region  perkComment start="/\*" end="\*/" contains=perkTodo
syn match   perkTodo "\<TODO\>" contained

" Strings
syn region perkString start=+"+ skip=+\\\\.+ end=+"+ contains=perkEscape
syn region perkString start=+'+ skip=+\\\\.+ end=+'+ contains=perkEscape
syn match  perkEscape "\\\\." contained

" Numbers
syn match perkNumber "\<0x[0-9A-Fa-f]\+\>"
syn match perkNumber "\<0o[0-7]\+\>"
syn match perkNumber "\<0b[01]\+\>"
syn match perkFloat  "\<[0-9]\+\.[0-9]\+\>"
syn match perkNumber "\<[0-9]\+\>"

" Function names after 'fun'
syn match perkFunctionName /\<fun\s\+\zs\w\+/ containedin=perkKeyword

" Variable names after 'let'
syn match perkLetName /\<let\s\+\zs\w\+\ze\s*:/ containedin=perkKeyword

" Embedded C markers
syn keyword perkCMarker BEGIN_C END_C

" Default highlight links (can be overridden later)
hi def link perkComment Comment
hi def link perkTodo Todo
hi def link perkKeyword Keyword
hi def link perkType Type
hi def link perkString String
hi def link perkEscape SpecialChar
hi def link perkNumber Number
hi def link perkFloat Float
hi def link perkFunctionName Function
hi def link perkLetName Identifier
hi def link perkCMarker PreProc

let b:current_syntax = "perk"
