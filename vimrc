colorscheme base16-monokai

" Define leader mappings
let g:mapleader="\<Space>"
let g:maplocalleader=","

" Allow selfsigned SSL certs in restapi test
let g:http_client_verify_ssl=0

" Prevent tcomment from making a zillion mappings (we just want the operator).
let g:tcomment_mapleader1=''
let g:tcomment_mapleader2=''
let g:tcomment_mapleader_comment_anyway=''
let g:tcomment_textobject_inlinecomment=''
let g:tcomment_mapleader_uncomment_anyway='gu'

let s:ts_filetypes=[
      \   'typescript',
      \   'typescript.tsx',
      \   'typescript.jest',
      \   'typescript.jest.tsx'
      \ ]

let s:js_filetypes=[
      \   'javascript',
      \   'javascript.jsx',
      \   'javascript.jest',
      \   'javascript.jest.jsx'
      \ ]

if filereadable('/usr/local/bin/python3')
  " Avoid search, speeding up start-up.
  let g:python3_host_prog='/usr/local/bin/python3'
endif

packadd! fzf.vim

" Must come *after* the `:packadd!` calls above otherwise the contents of
" package "ftdetect" directories won't be evaluated.
filetype indent plugin on
syntax on

" After this file is sourced, plugin code will be evaluated.
" See ~/.vim/after for files evaluated after that.
