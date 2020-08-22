"""""""""""
" Plugins "
"""""""""""

" Clear highlighting on escape in normal mode
nnoremap <esc> :noh<return><esc>

" Ferret
" use <leader>f (mnemonic: "[f]ind")
nmap <leader>f <Plug>(FerretAckWord)
" use <leader>r (mnemonic: "[r]eplace")
nmap <leader>r <Plug>(FerretAcks)

" quickfix navigation
nmap <Up> :cp<<cr>>
nmap <Down> :cp<<cr>>

" FZF "
noremap <leader>p :Files<cr>
noremap <leader>. :Files ~<cr>
noremap <leader>c :Commits<cr>
noremap <leader>b :Buffers<cr>
noremap <leader>m :Marks<cr>

" [Buffers] Jump to the existing window if possible
let g:fzf_buffers_jump = 1
" [Tags] Command to generate tags file
let g:fzf_tags_command = 'ctags -R'
" [Commands] --expect expression for directly executing the command
let g:fzf_commands_expect = 'alt-enter,ctrl-x'
let $FZF_DEFAULT_COMMAND = 'rg --files --no-ignore-vcs --hidden'

" Deoplete / Jedi
let deoplete#sources#jedi#show_docstring = 1
let g:jedi#completions_enabled = 0

" Lang servers
let g:LanguageClient_serverCommands = {
    \ 'rust': ['rustup', 'run', 'nightly', 'rls'],
    \ 'javascript': ['javascript-typescript-stdio'],
    \ 'javascript.jsx': ['javascript-typescript-stdio'],
    \ 'python': ['pyls', '-v']
    \ }

" Emmet
let g:user_emmet_leader_key='<C-X>'
