#
# ~/.bashrc
#


./usr/lib/python3.5/site-packages/powerline/bindings/bash/powerline.sh

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'

screenfetch -D linux

powerline-daemon -q
POWERLINE_BASH_CONTINUATION=1
POWERLINE_BASH_SELECT=1
. /usr/lib/python3.5/site-packages/powerline/bindings/bash/powerline.sh


PS1="[\u${GREEN}@${RESET}\h${RESET}${GREEN}${RESET}][${GREEN}\W${RESET}][${GREEN}\j${RESET}]$ " 

