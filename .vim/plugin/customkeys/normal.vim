" Tab movements
noremap <leader>q :q<cr>
noremap <leader>1 1gt
noremap <leader>2 2gt
noremap <leader>3 3gt
noremap <leader>4 4gt
noremap <leader>5 5gt
noremap <leader>6 6gt
noremap <leader>7 7gt
noremap <leader>8 8gt
noremap <leader>9 9gt
noremap <leader>0 :tablast<cr>

" Split movements
noremap <C-h> <C-w><C-h>
noremap <C-j> <C-w><C-j>
noremap <C-k> <C-w><C-k>
noremap <C-l> <C-w><C-l>

" Normal mode mappings "
nnoremap <Left> <NOP>
nnoremap <Right> <NOP>
nnoremap <Up> <NOP>
nnoremap <Down> <NOP>

" Use ,+x to execute inline bash script
nnoremap <localleader>x :.!bash<CR>
nnoremap <localleader>p :.!python3<CR>
nnoremap <leader>p :terminal python3<CR>i

" Open pydoc
nnoremap <buffer> H :<C-u>execute "!pydoc3 " . expand("<cword>")<CR>
