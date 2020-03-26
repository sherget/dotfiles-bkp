fun! Start()
  "Create a new unnamed buffer to display our splash screen inside of.
  enew

  " Set some options for this buffer to make sure that does not act like a
  " normal winodw.
  setlocal
        \ bufhidden=wipe
        \ buftype=nofile
        \ nobuflisted
        \ nocursorcolumn
        \ nocursorline
        \ nolist
        \ nonumber
        \ noswapfile
        \ norelativenumber
        \ syntax=off
        \ textwidth=500

  exec ":r ~/dotfiles/splash.vim"
  exec ":IndentLinesDisable"

  " When we are done writing out message set the buffer to readonly.
  setlocal
        \ nomodifiable
        \ nomodified
  exec ':execute "normal! gg"'

  " Just like with the default start page, when we switch to insert mode
  " a new buffer should be opened which we can then later save.
  nnoremap <buffer><silent> e :IndentLinesEnable <bar> enew<CR>
  nnoremap <buffer><silent> i :IndentLinesEnable <bar> enew <bar> startinsert<CR>
  nnoremap <buffer><silent> o :IndentLinesEnable <bar> enew <bar> startinsert<CR>
endfun

if argc() == 0
  autocmd VimEnter * call Start()
endif

colorscheme base16-monokai

" Allow selfsigned SSL certs in restapi test
let g:http_client_verify_ssl=1

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

" Define leader mappings
let g:mapleader="\<Space>"
let g:maplocalleader=","

" After this file is sourced, plugin code will be evaluated.
" See ~/.vim/after for files evaluated after that.
