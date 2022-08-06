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
nnoremap <expr> <leader>p (len(system('git rev-parse')) ? ':Files' : ':GFiles --exclude-standard --others --cached')."\<cr>"
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
"let deoplete#sources#jedi#show_docstring = 1
let g:jedi#completions_enabled = 0

" Lang servers
let g:LanguageClient_serverCommands = {
    \ 'rust': ['rustup', 'run', 'nightly', 'rls'],
    \ 'javascript': ['javascript-typescript-stdio'],
    \ 'javascript.jsx': ['javascript-typescript-stdio'],
    \ 'python': ['pyls', '-v'],
    \ 'go': ['gopls']
    \ }

au filetype go inoremap <buffer> . .<C-x><C-o>
autocmd BufWritePre *.go :call LanguageClient#textDocument_formatting_sync()
let g:go_fmt_command = "goimports"    " Run goimports along gofmt on each save
let g:go_auto_type_info = 1           " Automatically get signature/type info for object under cursor
g:LanguageClient_completionPreferTextEdit = 1


" Emmet
let g:user_emmet_leader_key='<C-X>'

" vim-surround angular
" string interpolation
au BufReadPost,BufNewFile * let b:surround_115 = "{{ \r }}"
" ngDerictive eg [(ngModel)]
au BufReadPost,BufNewFile * let b:surround_110 = "[(\r)]"

" bracket replacement
au BufReadPost,BufNewFile * let b:surround_119 = "(\r)"
au BufReadPost,BufNewFile * let b:surround_87 = "( \r )"
au BufReadPost,BufNewFile * let b:surround_101 = "[\r]"
au BufReadPost,BufNewFile * let b:surround_69 = "[ \r ]"
au BufReadPost,BufNewFile * let b:surround_114 = "{\r}"
au BufReadPost,BufNewFile * let b:surround_82 = "{ \r }"
