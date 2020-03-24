set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath=&runtimepath
set rtp+=~/.fzf

function! SomeCheck()
	if filereadable("~/.vimrc")
		source ~/.vimrc
	endif
endfunction

let g:deoplete#enable_at_startup = 1
