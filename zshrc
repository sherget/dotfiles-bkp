if [ "$TERM" = "xterm" ]; then
    export TERM=xterm-256color
fi
if [ "$TERM" = "screen" -o "$TERM" = "screen-256color" ]; then
    export TERM=screen-256color
    unset TERMCAP
fi

bindkey -v

HISTFILE=$HOME/.zsh_history
HISTFILESIZE=500
HISTSIZE=500

autoload -Uz compinit promptinit
autoload -Uz colors && colors
compinit
promptinit

zstyle ':completion:*' menu select

setopt COMPLETE_ALIASES

#automatic terminal reset
ttyctl -f

##################################################################
#			 Terminal Colors 			 #	
##################################################################

# ls colored output
alias ls="ls -F --color=auto"

LS_COLORS='rs=0:di=01;38;5;33:ln=01;36:mh=00:pi=40;33:so=01;38;5;59:do=01;38;5;59:bd=40;33;01:cd=40;33;01:or=40;31;01:mi=00:su=37;41:sg=30;43:ca=30;41:tw=30;42:ow=34;42:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arc=01;31:*.arj=01;31:*.taz=01;31:*.lha=01;31:*.lz4=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.tzo=01;31:*.t7z=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.dz=01;31:*.gz=01;31:*.lrz=01;31:*.lz=01;31:*.lzo=01;31:*.xz=01;31:*.zst=01;31:*.tzst=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.war=01;31:*.ear=01;31:*.sar=01;31:*.rar=01;31:*.alz=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.cab=01;31:*.wim=01;31:*.swm=01;31:*.dwm=01;31:*.esd=01;31:*.jpg=01;38;5;59:*.jpeg=01;38;5;59:*.mjpg=01;38;5;59:*.mjpeg=01;38;5;59:*.gif=01;38;5;59:*.bmp=01;38;5;59:*.pbm=01;38;5;59:*.pgm=01;38;5;59:*.ppm=01;38;5;59:*.tga=01;38;5;59:*.xbm=01;38;5;59:*.xpm=01;38;5;59:*.tif=01;38;5;59:*.tiff=01;38;5;59:*.png=01;38;5;59:*.svg=01;38;5;59:*.svgz=01;38;5;59:*.mng=01;38;5;59:*.pcx=01;38;5;59:*.mov=01;38;5;59:*.mpg=01;38;5;59:*.mpeg=01;38;5;59:*.m2v=01;38;5;59:*.mkv=01;38;5;59:*.webm=01;38;5;59:*.ogm=01;38;5;59:*.mp4=01;38;5;59:*.m4v=01;38;5;59:*.mp4v=01;38;5;59:*.vob=01;38;5;59:*.qt=01;38;5;59:*.nuv=01;38;5;59:*.wmv=01;38;5;59:*.asf=01;38;5;59:*.rm=01;38;5;59:*.rmvb=01;38;5;59:*.flc=01;38;5;59:*.avi=01;38;5;59:*.fli=01;38;5;59:*.flv=01;38;5;59:*.gl=01;38;5;59:*.dl=01;38;5;59:*.xcf=01;38;5;59:*.xwd=01;38;5;59:*.yuv=01;38;5;59:*.cgm=01;38;5;59:*.emf=01;38;5;59:*.ogv=01;38;5;59:*.ogx=01;38;5;59:*.aac=00;38;5;131:*.au=00;38;5;131:*.flac=00;38;5;131:*.m4a=00;38;5;131:*.mid=00;38;5;131:*.midi=00;38;5;131:*.mka=00;38;5;131:*.mp3=00;38;5;131:*.mpc=00;38;5;131:*.ogg=00;38;5;131:*.ra=00;38;5;131:*.wav=00;38;5;131:*.oga=00;38;5;131:*.opus=00;38;5;131:*.spx=00;38;5;131:*.xspf=00;38;5;131:*.html=00;38;5;94:*.xml=00;38;5;94:*.htm=00;38;5;94:*.css=00;38;5;117:*.less=01;38;5;25:*.scss=00;38;5;169:*.sass=00;38;5;169:*.py=00;38;5;64:*.rb=01;38;5;88:*.gem=00;38;5;88:*.js=00;38;5;81:*.c=01;38;5;7:*.h=01;38;5;8:*.java=00;38;5;57:*.php=00;38;5;184:';
export LS_COLORS

# colored less output
export LESS=-R
export LESS_TERMCAP_mb=$'\E[1;31m'     # begin bold
export LESS_TERMCAP_md=$'\E[1;36m'     # begin blink
export LESS_TERMCAP_me=$'\E[0m'        # reset bold/blink
export LESS_TERMCAP_so=$'\E[01;44;33m' # begin reverse video
export LESS_TERMCAP_se=$'\E[0m'        # reset reverse video
export LESS_TERMCAP_us=$'\E[1;32m'     # begin underline
export LESS_TERMCAP_ue=$'\E[0m'        # reset underline

# colored man pages
man() {
    LESS_TERMCAP_md=$'\e[01;31m' \
    LESS_TERMCAP_me=$'\e[0m' \
    LESS_TERMCAP_se=$'\e[0m' \
    LESS_TERMCAP_so=$'\e[01;44;33m' \
    LESS_TERMCAP_ue=$'\e[0m' \
    LESS_TERMCAP_us=$'\e[01;32m' \
    command man "$@"
}

##################################################################

DIRSTACKFILE="$HOME/.cache/zsh/dirs"
if [[ -f $DIRSTACKFILE ]] && [[ $#dirstack -eq 0 ]]; then
  dirstack=( ${(f)"$(< $DIRSTACKFILE)"} )
  [[ -d $dirstack[1] ]] && cd $dirstack[1]
fi
chpwd() {
  print -l $PWD ${(u)dirstack} >$DIRSTACKFILE
}

DIRSTACKSIZE=20

setopt AUTO_PUSHD PUSHD_SILENT PUSHD_TO_HOME

## Remove duplicate entries
setopt PUSHD_IGNORE_DUPS

## This reverts the +/- operators.
setopt PUSHD_MINUS

## i3 fix gnome-keyring
dbus-update-activation-environment --systemd DISPLAY
eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK

export PATH="${PATH}:/home/shin/Applications"
export PATH="${PATH}:/usr/.local/bin"
export PATH="${PATH}:/home/shin/.cargo/bin"
export PATH="${PATH}:/home/shin/.local/bin"
export PATH="${PATH}:/home/shin/.local/lib/python3.6/site-packages"
export EDITOR=vim

alias wipe-cache-dotfiles="find ~/dotfiles -name '.swp' -delete"
alias syu="sudo pacman -Syu"
alias yt-getsong="youtube-dl --extract-audio --audio-format mp3"
alias yt-getlist="youtube-dl --extract-audio --audio-format mp3 -i -o '%(title)s.%(ext)s'"
alias shincode="ssh sven@www.shincode.de -p 2201"
alias collidex="ssh devextern01@10.10.0.10 -p 22"

powerline-daemon -q
POWERLINE_BASH_CONTINUATION=1
POWERLINE_BASH_SELECT=1
 . /usr/lib/python3.7/site-packages/powerline/bindings/zsh/powerline.zsh

source /home/shin/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
