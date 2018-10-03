"------------------------ General --------------------------
set number 			" enable line numbers
syntax enable 			" enable syntax highlighting
set ruler
set ttyfast
set cursorline
set showmatch
set exrc

" set colorschemes

autocmd BufEnter * colorscheme up
autocmd BufEnter *.py colorscheme ir_black "solarized8_dark_low_shin, ir_black
autocmd BufEnter *.html colorscheme Chasing_Logic
autocmd Filetype scss,less,html,javascript setlocal ts=4 sts=4 sw=4

set encoding=utf-8

au BufNewFile,BufRead *.py
    \ set tabstop=4 |
    \ set softtabstop=4 |
    \ set shiftwidth=4 |
    \ set textwidth=79 |
    \ set expandtab |
    \ set autoindent |
    \ set fileformat=unix

"split navigations
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>
map <C-n> :NERDTreeToggle<CR>

" Enable folding
set foldmethod=indent
set foldlevel=99

nnoremap <space> za

let g:SimpylFold_docstring_preview=1
let g:ycm_autoclose_preview_window_after_completion=1
map <leader>g  :YcmCompleter GoToDefinitionElseDeclaration<CR>

let python_highlight_all=1

"-------------------- Vundle config ------------------------"

set nocompatible              " be iMproved, required
filetype off                  " required

"let &runtimepath.=',~/.vim/bundle/ale'

filetype plugin on

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

	Plugin 'VundleVim/Vundle.vim' 			" let Vundle manage Vundle, required

    "-------------------=== Code/Project navigation ===-------------
    Plugin 'scrooloose/nerdtree'                " Project and file navigation
    Plugin 'majutsushi/tagbar'                  " Class/module browser
    Plugin 'kien/ctrlp.vim'                     " Fast transitions on project files

    "-------------------=== Other ===-------------------------------
    Plugin 'bling/vim-airline'                  " Lean & mean status/tabline for vim
    Plugin 'vim-airline/vim-airline-themes'     " Themes for airline
    Plugin 'Lokaltog/powerline'                 " Powerline fonts plugin
    Plugin 'fisadev/FixedTaskList.vim'          " Pending tasks list
    Plugin 'rosenfeld/conque-term'              " Consoles as buffers
    Plugin 'tpope/vim-surround'                 " Parentheses, brackets, quotes, XML tags, and more
    Plugin 'flazz/vim-colorschemes'             " Colorschemes
    Plugin 'tmhedberg/SimpylFold'		" Folding Plugin
    Plugin 'altercation/vim-colors-solarized'	" Colortheme
    Plugin 'jnurmine/Zenburn'

    "-------------------=== Snippets support ===--------------------
    Plugin 'garbas/vim-snipmate'                " Snippets manager
    Plugin 'MarcWeber/vim-addon-mw-utils'       " dependencies #1
    Plugin 'tomtom/tlib_vim'                    " dependencies #2
    Plugin 'honza/vim-snippets'                 " snippets repo

    "-------------------=== Languages support ===-------------------
    Plugin 'tpope/vim-commentary'               " Comment stuff out
    Plugin 'mitsuhiko/vim-sparkup'              " Sparkup(XML/jinja/htlm-django/etc.) support
    Plugin 'Rykka/riv.vim'                      " ReStructuredText plugin
    Plugin 'Valloric/YouCompleteMe'             " Autocomplete plugin
    Plugin 'rust-lang/rust.vim'			" Rust support

    "-------------------=== Python  ===-----------------------------
    Plugin 'klen/python-mode'                   " Python mode (docs, refactor, lints...)
    Plugin 'scrooloose/syntastic'               " Syntax checking plugin for Vim
    Plugin 'vim-scripts/indentpython.vim'	" Codeformatter
    Plugin 'nvie/vim-flake8'			" Autocomplete

    "-------------------=== HTML/CSS/Less/Sass  ===-----------------------------
    Plugin 'mattn/emmet-vim'			" Emmet HTML/XML autocomplete
    Plugin 'cakebaker/scss-syntax.vim'		" Sass support

    "-------------------=== GIT  ===-----------------------------
    Plugin 'tpope/vim-fugitive'			" GIT wrapper

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

"-----------------------------------------------------------

"=====================================================
"" AirLine settings
"=====================================================
let g:airline_theme='badwolf'
let g:airline#extensions#tabline#enabled=1
let g:airline#extensions#tabline#formatter='unique_tail'
let g:airline_powerline_fonts=1

"=====================================================
"" TagBar settings
"=====================================================
"let g:tagbar_autofocus=0
"let g:tagbar_width=42
"autocmd BufEnter *.py :call tagbar#autoopen(0)
"autocmd BufWinLeave *.py :TagbarClose
