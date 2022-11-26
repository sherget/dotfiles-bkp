## LS_COLORS
LS_COLORS="rs=0:di=01;34:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:mi=00:su=37;41:sg=30;43:ca=30;41:tw=30;42:ow=34;100:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arc=01;31:*.arj=01;31:*.taz=01;31:*.lha=01;31:*.lz4=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.tzo=01;31:*.t7z=01;31:*.zip=01;31:*.z=01;31:*.dz=01;31:*.gz=01;31:*.lrz=01;31:*.lz=01;31:*.lzo=01;31:*.xz=01;31:*.zst=01;31:*.tzst=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.war=01;31:*.ear=01;31:*.sar=01;31:*.rar=01;31:*.alz=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.cab=01;31:*.wim=01;31:*.swm=01;31:*.dwm=01;31:*.esd=01;31:*.jpg=01;35:*.jpeg=01;35:*.mjpg=01;35:*.mjpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.webm=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.m4a=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.oga=00;36:*.opus=00;36:*.spx=00;36:*.xspf=00;36:";
export LS_COLORS
export PATH=${PATH}
export EDITOR=/usr/bin/vim
export VISUAL=/usr/bin/vim
export PATH=~/.npm-global/bin:$PATH
export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:${HOME}/.local/bin/:$PATH"
export WORKON_HOME=~/.virtualenvs
source /usr/bin/virtualenvwrapper_lazy.sh

## Options section
setopt extendedglob                                             # Extended globbing. Allows using regular expressions with *
setopt nocaseglob                                               # Case insensitive globbing
setopt rcexpandparam                                            # Array expension with parameters
setopt numericglobsort                                          # Sort filenames numerically when it makes sense
setopt nobeep                                                   # No beep
setopt appendhistory                                            # Immediately append history instead of overwriting
setopt hist_find_no_dups                                        # don't show dupes when searching
setopt autocd                                                   # if only directory path is entered, cd there.
setopt auto_param_slash                                         # tab completing directory appends a slash
setopt auto_pushd                                               # [default] cd automatically pushes old dir onto dir stack
setopt auto_resume                                              # allow simple commands to resume backgrounded jobs
setopt clobber                                                  # allow clobbering with >, no need to use >!
#setopt correct                                                  # [default] command auto-correction
#setopt correct_all                                              # [default] argument auto-correction
setopt no_flow_control                                          # disable start (C-s) and stop (C-q) characters
setopt ignore_eof                                               # [default] prevent accidental C-d from exiting shell
setopt interactive_comments                                     # [default] allow comments, even in interactive shells

# Speed up completions
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'       # Case insensitive tab completion
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"         # Colored completion (different colors for dirs/files/etc)
zstyle ':completion:*' rehash true                              # automatically find new executables in path
zstyle ':completion:*' accept-exact '*(N)'
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache
HISTFILE=~/.zhistory
HISTSIZE=1000
SAVEHIST=500
WORDCHARS=${WORDCHARS//\/[&.;]}                                 # Don't consider certain characters part of the word

## Keybindings section
# Enable Vim mode and get rid of mode switching lags
bindkey -v
export KEYTIMEOUT=1

# Make CTRL-Z background things and unbackground them.
function fg-bg() {
  if [[ $#BUFFER -eq 0 ]]; then
    fg
  else
    zle push-input
  fi
}
zle -N fg-bg

bindkey '^Z' fg-bg
bindkey '^R' history-incremental-pattern-search-backward

autoload -U edit-command-line
zle -N edit-command-line
bindkey '^x^x' edit-command-line

$(xrdb /home/shin/.Xresources)

## Alias section
if type nvim > /dev/null 2>/dev/null; then
	alias vim='nvim' # If nvim is installed alias vim to nvim
fi
alias ls='ls --color=tty -l'
alias ll='ls --color=tty -l'
alias la='ls --color=tty -la'
alias df='df -h'                                                # Human-readable sizes
alias free='free -m'                                            # Show sizes in MB

alias g++='g++ -pedantic-errors -Wall -Weffc++ -Wextra -Wsign-conversion -std=c++17 -Wc++17-extensions'

alias triplehead='bash ~/.screenlayout/monitormodes.sh'
alias sysupdate='pacman -Syu && xmonad --recompile'
alias currentgpu='lspci -vnnn | perl -lne "print if /^\d+\:.+(\[\S+\:\S+\])/" | grep VGA'
alias startxnvidia='prime-offload && optimus-manager --switch nvidia --no-confirm && sudo prime-switch'
alias startxintel='prime-offload && optimus-manager --switch intel --no-confirm && sudo prime-switch'

# Theming section
autoload -U compinit colors zcalc
compinit -d
colors

## SSH configurations
 if [[ -n $SSH_CONNECTION ]]; then
   export EDITOR='vim'
 else
   export EDITOR='vim'
 fi

## Prompt
# Modify the colors and symbols in these variables as desired.
GIT_PROMPT_SYMBOL="%{$fg[blue]%}"$(printf '%%F{white}\ue0a0%.0s%%f ' {1..$LVL})
GIT_PROMPT_PREFIX="%{$fg[white]%}[%{$reset_color%}"
GIT_PROMPT_SUFFIX="%{$fg[white]%}]%{$reset_color%}"
GIT_PROMPT_AHEAD="%{$fg[red]%}ANUM%{$reset_color%}"             # A"NUM"         - ahead by "NUM" commits
GIT_PROMPT_BEHIND="%{$fg[cyan]%}BNUM%{$reset_color%}"           # B"NUM"         - behind by "NUM" commits
GIT_PROMPT_MERGING="%{$fg_bold[magenta]%}⚡︎%{$reset_color%}"    # lightning bolt - merge conflict
GIT_PROMPT_UNTRACKED="%{$fg_bold[red]%}●%{$reset_color%}"       # red circle     - untracked files
GIT_PROMPT_MODIFIED="%{$fg_bold[yellow]%}●%{$reset_color%}"     # yellow circle  - tracked files modified
GIT_PROMPT_STAGED="%{$fg_bold[green]%}●%{$reset_color%}"        # green circle   - staged changes present = ready for "git push"

# Functions
parse_git_branch() {
  # Show Git branch/tag, or name-rev if on detached head
  ( git symbolic-ref -q HEAD || git name-rev --name-only --no-undefined --always HEAD ) 2> /dev/null
}

parse_git_state() {
  # Show different symbols as appropriate for various Git repository states
  # Compose this value via multiple conditional appends.
  local GIT_STATE=""
  local NUM_AHEAD="$(git log --oneline @{u}.. 2> /dev/null | wc -l | tr -d ' ')"
  if [ "$NUM_AHEAD" -gt 0 ]; then
    GIT_STATE=$GIT_STATE${GIT_PROMPT_AHEAD//NUM/$NUM_AHEAD}
  fi
  local NUM_BEHIND="$(git log --oneline ..@{u} 2> /dev/null | wc -l | tr -d ' ')"
  if [ "$NUM_BEHIND" -gt 0 ]; then
    GIT_STATE=$GIT_STATE${GIT_PROMPT_BEHIND//NUM/$NUM_BEHIND}
  fi
  local GIT_DIR="$(git rev-parse --git-dir 2> /dev/null)"
  if [ -n $GIT_DIR ] && test -r $GIT_DIR/MERGE_HEAD; then
    GIT_STATE=$GIT_STATE$GIT_PROMPT_MERGING
  fi
  if [[ -n $(git ls-files --other --exclude-standard 2> /dev/null) ]]; then
    GIT_STATE=$GIT_STATE$GIT_PROMPT_UNTRACKED
  fi
  if ! git diff --quiet 2> /dev/null; then
    GIT_STATE=$GIT_STATE$GIT_PROMPT_MODIFIED
  fi
  if ! git diff --cached --quiet 2> /dev/null; then
    GIT_STATE=$GIT_STATE$GIT_PROMPT_STAGED
  fi
  if [[ -n $GIT_STATE ]]; then
    echo "$GIT_STATE "
  fi
}

git_prompt_string() {
  local git_where="$(parse_git_branch)"

  # If inside a Git repository, print its branch and state
  [ -n "$git_where" ] && echo "$GIT_PROMPT_SYMBOL$GIT_PROMPT_PREFIX$(parse_git_state)%{$fg[blue]%}${git_where#(refs/heads/|tags/)}$GIT_PROMPT_SUFFIX"

  # If not inside the Git repo, print exit codes of last command (only if it failed)
  [ ! -n "$git_where" ] && echo "%{$fg[red]%} %(?..[%?])"
}

setopt PROMPT_SUBST
RPROMPT_BASE="${vcs_info_msg_0_}%F{blue}%~%f"

# set dual monitors
dual () {
  if [[ $(xrandr | grep "DP-3") =~ .*"DP-3 connected".* ]]; then
    xrandr --output eDP-1 --off --output DP-3 --primary --left-of DP-1-2 --output DP-1-2 --auto
  fi
}

# set single monitor
single () {
  if [[ $(xrandr | grep "DP-3") =~ .*"DP-3 connected".* ]]; then
    xrandr --output eDP-1 --off --output DP-3 --primary --output DP-1-2 --off
  fi
}

# Anonymous function to avoid leaking variables.
function () {
  if [[ $EUID -eq 0 ]]; then
    local SUFFIX='%F{blue}%n%f '$(printf '%%F{red}\u276f%.0s%%f' {1..$LVL})
  else
    local SUFFIX=''
  fi
  export PS1=$(printf '%%F{white}\u6c23%.0s%%f' {1..$LVL})"[%F{green}${SSH_TTY:+%n@%m}%f%B${SSH_TTY:+:}%b%F{blue}%B%1~%b%F{red}%B%(1j.*.)%(?..!)%b%f%B${SUFFIX}%b]$ "
}

export RPROMPT='$(git_prompt_string) $RPROMPT_BASE'
export SPROMPT="zsh: correct %F{red}'%R'%f to %F{red}'%r'%f [%B%Uy%u%bes, %B%Un%u%bo, %B%Ue%u%bdit, %B%Ua%u%bbort]? "

# Plugins
plugins=(git git-prompt colored-man-pages z)

# Manual Plugins
source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
source ~/.zshprofile
source /usr/share/z/z.sh

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

