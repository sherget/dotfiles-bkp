"""""""""""
" Plugins "
"""""""""""

" FZF "
nnoremap <leader>p :Files<CR>
nnoremap <leader>. :Files ~<CR>
nnoremap <leader>c :Commits<CR>
nnoremap <leader>b :Buffers<CR>
nnoremap <leader>m :Marks<CR>

" [Buffers] Jump to the existing window if possible
let g:fzf_buffers_jump = 1
" [Tags] Command to generate tags file
let g:fzf_tags_command = 'ctags -R'
" [Commands] --expect expression for directly executing the command
let g:fzf_commands_expect = 'alt-enter,ctrl-x'
let $FZF_DEFAULT_COMMAND = 'rg --files --no-ignore-vcs --hidden'

" Tab movements
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
