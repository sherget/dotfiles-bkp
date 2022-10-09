#!/bin/bash
dotfiles=('.vimrc' '.vim' '.fzf' '.config/neovim/init.vim' '.gitignore_global'
  '.rgignore' '.zshrc' '.zsh' '.xmonad/xmonad.hs' '.xmonad/xmobarrc'
  '.config/rofi/config.rasi', '.xinitrc')
symlinks=('ln -s ~/dotfiles/zshrc ~/.zshrc'
'ln -s ~/dotfiles/vimrc ~/.vimrc'
'ln -s ~/dotfiles/.vim ~/.vim'
'ln -s ~/dotfiles/init.vim ~/.config/neovim/init.vim'
'ln -s ~/dotfiles/.fzf ~/.fzf'
'ln -s ~/dotfiles/rgignore ~/.rgignore'
'ln -s ~/dotfiles/.zsh ~/.zsh'
'ln -s ~/dotfiles/gitignore_global ~/.gitignore_global'
'ln -s ~/dotfiles/xmonad/xmonad.hs ~/.xmonad/xmonad.hs'
'ln -s ~/dotfiles/xmonad/xmobarrc ~/.xmonad/xmobarrc'
'ln -s ~/dotfiles/rofi/config.rasi ~/.config/rofi/config.rasi'
'ln -s ~/dotfiles/picom/picom.conf ~/.config/picom.conf'
'ln -s ~/dotfiles/xinitrc ~/.xinitrc'
)
for dotfile in "${dotfiles[@]}"
do
  mv ~/${dotfile} ~/dotfiles/backup/
done

for symlink in "${symlinks[@]}"
do
  bash -c ${symlink}
done
