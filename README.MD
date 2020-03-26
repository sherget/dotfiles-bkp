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
```
*Arch:*
```bash
$ sudo pacman -S zsh
```

### 2. VIM / Neovim
*Debian:*
```bash
$ sudo add-apt-repository ppa:neovim-ppa/stable
$ sudo apt update && sudo apt upgrade
$ sudo apt install neovim python python-pip
$ pip3 install pynvim && pip install pynvim
$ pip3 install requests && pip install requests
// to upgrade pynvim
// python3 -m pip install --user --upgrade pynvim
```
*Arch:*
```bash
$ sudo pacman -Syu
$ sudo pacman -S neovim python python-pip
$ sudo pip3 install pynvim
// to upgrade pynvim
// python3 -m pip install --user --upgrade pynvim
```

### 3. Install riggrep (https://github.com/BurntSushi/ripgrep)
If your Ubuntu version is earlier than 18.10 go to the most recent release on
https://github.com/BurntSushi/ripgrep/releases, scroll to the bottom and download the desired package.
Then install with:
```bash
$ sudo dpkg -i ripgrep_11.0.2_amd64.deb
```

### 4. Install fancy font
*Debian&Arch:*
```bash
$ git clone https://github.com/powerline/fonts
$ cd fonts && ./install.sh
$ cd .. && sudo rm -r fonts
```
### 5. Symlinks
```bash
$ ln -s ~/dotfiles/zshrc ~/.zshrc
$ ln -s ~/dotfiles/vimrc ~/.vimrc
$ ln -s ~/dotfiles/.vim ~/.vim
$ ln -s ~/dotfiles/init.vim ~/.config/nvim/init.vim
$ ln -s ~/dotfiles/.fzf ~/.fzf
$ ln -s ~/dotfiles/rgignore ~/.rgignore
$ ln -s ~/dotfiles/.zsh ~/.zsh
```

### Submodules
* git submodule add https://github.com/chriskempson/base16-vim .vim/pack/bundle/start/base16-vim
* git submodule add https://github.com/tpope/vim-repeat .vim/pack/bundle/start/vim-repeat
* git submodule add https://github.com/tpope/vim-speeddating .vim/pack/bundle/start/vim-speeddating
* git submodule add https://github.com/aquach/vim-http-client .vim/pack/bundle/start/vim-http-client
* git submodule add https://github.com/tpope/vim-surround .vim/pack/bundle/start/vim-surround
* git submodule add https://github.com/tpope/vim-git .vim/pack/bundle/start/vim-git
* git submodule add https://github.com/tpope/vim-fugitive .vim/pack/bundle/start/vim-fugitive
* git submodule add https://github.com/tomtom/tcomment_vim .vim/pack/bundle/start/tcomment_vim
* git submodule add https://github.com/Yggdroot/indentLine .vim/pack/bundle/start/indentLine
* git submodule add https://github.com/leafgarland/typescript-vim .vim/pack/bundle/start/typescript-vim
* git submodule add https://github.com/elzr/vim-json .vim/pack/bundle/start/vim-json
* git submodule add https://github.com/pangloss/vim-javascript .vim/pack/bundle/start/vim-javascript
* git submodule add https://github.com/wincent/ferret .vim/pack/bundle/start/ferret
* git submodule add https://github.com/junegunn/fzf.vim .vim/pack/bundle/start/fzf.vim
* git submodule add https://github.com/SirVer/ultisnips .vim/pack/bundle/start/ultisnips
* git submodule add https://github.com/andymass/vim-matchup .vim/pack/bundle/start/vim-matchup
* git submodule add https://github.com/Shougo/deoplete.nvim .vim/pack/bundle/start/deoplete.nvim
* git submodule add https://github.com/junegunn/fzf .fzf
* git submodule add https://github.com/zsh-users/zsh-syntax-highlighting .zsh/zsh-syntax-highlighting
* git submodule add https://github.com/zsh-users/zsh-autosuggestions .zsh/zsh-autosuggestions

A big thank you to everyone who created or contributed to one of the projects!
