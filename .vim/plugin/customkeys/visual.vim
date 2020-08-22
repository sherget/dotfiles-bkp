" Visual mode mappings "

" Copy to system clipboard
vnoremap <leader>y "+y
vmap ga y'>p:'[,']-1s/$/+/\|'[,']+1j!<CR>:%s/\d\+\zs\s*//g<CR>Do<C-R>=<C-R>"<CR><ESC><ESC>
vmap gs y'>p:'[,']-1s/$/-/\|'[,']+1j!<CR>:%s/\d\-\zs\s*//g<CR>Do<C-R>=<C-R>"<CR><ESC><ESC>
