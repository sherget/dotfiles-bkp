" Normal mode mappings "
nnoremap <Left> <NOP>
nnoremap <Right> <NOP>
nnoremap <Up> <NOP>
nnoremap <Down> <NOP>

" Use ,+x to execute inline bash script
nnoremap <localleader>x :.!bash<CR>
nnoremap <localleader>p :.!python3<CR>
nnoremap <leader>p :terminal python3<CR>i
