#!/bin/bash
dotfiles=('.vimrc' '.vim' '.fzf' '.config/neovim/init.vim' '.rgignore' '.zshrc' '.zsh')
symlinks=('ln -s ~/dotfiles/zshrc ~/.zshrc'
'ln -s ~/dotfiles/vimrc ~/.vimrc'
'ln -s ~/dotfiles/.vim ~/.vim'
'ln -s ~/dotfiles/init.vim ~/.config/neovim/init.vim'
'ln -s ~/dotfiles/.fzf ~/.fzf'
'ln -s ~/dotfiles/rgignore ~/.rgi'
'ln -s ~/dotfiles/.zsh ~/.zsh'
)
for dotfile in "${dotfiles[@]}"
do
  mv ~/${dotfile} ~/dotfiles/backup/
done

for symlink in "${symlinks[@]}"
do
  bash -c ${symlink}
done
