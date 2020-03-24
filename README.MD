# Config files for rapid system setup
*TODO: Put this stuff in ansible or something similiar*

## Overview:
1. zsh
2. vim/neovim

## Manual installation for Debian and Arch based Distros
**Clone the repo and set capslock to escape**
```bash
$ git clone --recurse-submodules -j8 https://github.com/sherget/dotfiles
$ setxkbmap -option caps:escape
```

### 1. Zsh + oh-my-zsh
*Debian:*
```bash
$ sudo apt install zsh
$ sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
```
*Arch:*
```bash
$ sudo pacman -S zsh
$ sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
```

### 2. VIM / Neovim
*Debian:*
```bash
$ sudo add-apt-repository ppa:neovim-ppa/stable
$ sudo apt update && sudo apt upgrade
$ sudo apt install neovim python python-pip
$ sudo pip3 install pynvim
// for upgrading pynvim
// python3 -m pip install --user --upgrade pynvim
```
*Arch:*
```bash
$ sudo pacman -Syu
$ sudo pacman -S neovim python python-pip
$ sudo pip3 install pynvim
// for upgrading pynvim
// python3 -m pip install --user --upgrade pynvim
```

### 3. Symlinks
```bash
$ ln -s ~/dotfiles/zshrc ~/.zshrc
$ ln -s ~/dotfiles/vimrc ~/.vimrc
$ ln -s ~/dotfiles/.vim ~/.vim
$ ln -s ~/dotfiles/init.vim ~/.config/nvim/init.vim
$ ln -s ~/dotfiles/.fzf ~/.fzf
```

### Submodules
* git submodule add https://github.com/chriskempson/base16-vim ~/dotfiles/.vim/pack/bundle/start/base16-vim
* git submodule add https://github.com/tpope/vim-repeat ~/dotfiles/.vim/pack/bundle/start/vim-repeat
* git submodule add https://github.com/tpope/vim-speeddating ~/dotfiles/.vim/pack/bundle/start/vim-speeddating
* git submodule add https://github.com/aquach/vim-http-client ~/dotfiles/.vim/pack/bundle/start/vim-http-client
* git submodule add https://github.com/tpope/vim-surround ~/dotfiles/.vim/pack/bundle/start/vim-surround
* git submodule add https://github.com/tpope/vim-git ~/dotfiles/.vim/pack/bundle/start/vim-git
* git submodule add https://github.com/tpope/vim-fugitive ~/dotfiles/.vim/pack/bundle/start/vim-fugitive
* git submodule add https://github.com/tomtom/tcomment_vim ~/dotfiles/.vim/pack/bundle/start/tcomment_vim
* git submodule add https://github.com/Yggdroot/indentLine ~/dotfiles/.vim/pack/bundle/start/indentLine
* git submodule add https://github.com/leafgarland/typescript-vim ~/dotfiles/.vim/pack/bundle/start/typescript-vim
* git submodule add https://github.com/elzr/vim-json ~/dotfiles/.vim/pack/bundle/start/vim-json
* git submodule add https://github.com/pangloss/vim-javascript ~/dotfiles/.vim/pack/bundle/start/vim-javascript
* git submodule add https://github.com/wincent/ferret ~/dotfiles/.vim/pack/bundle/start/ferret
* git submodule add https://github.com/junegunn/fzf.vim ~/dotfiles/.vim/pack/bundle/start/fzf.vim
* git submodule add https://github.com/SirVer/ultisnips ~/dotfiles/.vim/pack/bundle/start/ultisnips
* git submodule add https://github.com/andymass/vim-matchup ~/dotfiles/.vim/pack/bundle/start/vim-matchup
* git submodule add https://github.com/Shougo/deoplete.nvim ~/dotfiles/.vim/pack/bundle/start/deoplete.nvim
* git submodule add https://github.com/junegunn/fzf ~/dotfiles/.fzf

A big thank you to everyone who created or contributed to one of the projects!
