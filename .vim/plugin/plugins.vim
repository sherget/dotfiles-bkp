"""""""""""
" Plugins "
"""""""""""

" FZF "
nnoremap <leader>t :Files<CR>
nnoremap <leader>th :Files ~<CR>
nnoremap <leader>tg :GFiles<CR>
nnoremap <leader>tc :Commits<CR>
nnoremap <leader>tc :Buffers<CR>
nnoremap <leader>tm :Marks<CR>
" [Buffers] Jump to the existing window if possible
let g:fzf_buffers_jump = 1
" [Tags] Command to generate tags file
let g:fzf_tags_command = 'ctags -R'
" [Commands] --expect expression for directly executing the command
let g:fzf_commands_expect = 'alt-enter,ctrl-x'
