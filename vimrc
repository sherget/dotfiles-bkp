colorscheme base16-monokai

let mapleader="\<Space>"
let maplocalleader=","

" Prevent tcomment from making a zillion mappings (we just want the operator).
let g:tcomment_mapleader1=''
let g:tcomment_mapleader2=''
let g:tcomment_mapleader_comment_anyway=''
let g:tcomment_textobject_inlinecomment=''
let g:tcomment_mapleader_uncomment_anyway='gu'

" Ferret
" File search(mnemonic: "[f]ind")
nmap <leader>f <Plug>(FerretAckWord)
" Multifile replace (mnemonic: "[r]eplace");
nmap <leader>r <Plug>(FerretAcks)

" Deoplete
let g:deoplete#enable_at_startup = 1

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

if &loadplugins
  if has('packages')
    packadd! base16-vim
    packadd! fzf.vim
    packadd! ferret
    packadd! indentLine
    packadd! tcomment_vim
    packadd! typescript-vim
    packadd! ultisnips
    packadd! vim-fugitive
    packadd! vim-git
    packadd! vim-javascript
    packadd! vim-json
    packadd! vim-repeat
    packadd! vim-speeddating
    packadd! vim-surround
endif

" Must come *after* the `:packadd!` calls above otherwise the contents of
" package "ftdetect" directories won't be evaluated.
filetype indent plugin on
syntax on

" After this file is sourced, plugin code will be evaluated.
" See ~/.vim/after for files evaluated after that.
