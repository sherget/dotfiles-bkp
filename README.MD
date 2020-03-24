# Config files for rapid system setup
*TODO: Put this stuff in ansible or something similiar*

## Overview:
1. zsh
2. vim/neovim

## Manual installation for Debian and Arch based Distros
### 1. Zsh + oh-my-zsh
*Debian:*
```bash
$ sudo apt install zsh
$ sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
$ cd && git clone https://github.com/sherget/dotfiles && cp dotfiles/.zshrc ~/.zshrc
```
*Arch:*
```bash
$ sudo pacman -S zsh
$ sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
$ cp dotfiles/.zshrc ~/.zshrc
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
