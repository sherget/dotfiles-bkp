set autoindent
set noexpandtab
set tabstop=4
set shiftwidth=4

au filetype go inoremap <buffer> . .<C-x><C-o>

" vim-go debugging delve config
let g:go_debug_windows = {
      \ 'vars':       'rightbelow 70vnew',
      \ 'stack':      'rightbelow 10new',
      \ 'goroutines': 'rightbelow 10new',
      \ }

nnoremap <F5> :GoDebugBreakpoint<cr>
nnoremap <F8> :GoDebugStart<cr>
nnoremap <F9> :GoDebugStop<cr>
let g:go_debug_mappings = {
      \ '(go-debug-continue)': {'key': 'c', 'arguments': '<nowait>'},
      \ '(go-debug-next)': {'key': 'n', 'arguments': '<nowait>'},
      \ '(go-debug-step)': {'key': 's'},
      \ '(go-debug-print)': {'key': 'p'},
  \}
