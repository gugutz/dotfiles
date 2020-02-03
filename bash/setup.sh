#!/bin/bash

DOTFILES_FOLDER="~/dotfiles"
DOTFILES_BACKUP="~/dotfiles/backup"

echo "# install bash completion"
sudo pacman -S --noconfirm bash-completion

echo "# creating symbolic links"

# bashrc
if [ -e $HOME/.bashrc ]; then
    mv -v  $HOME/.bashrc $DOTFILES_BACKUP/bash/bashrc-bkp
fi
ln -sf $DOTFILES_FOLDER/bash/bashrc ~/.bashrc

# bash_profile
if [ -e $HOME/.bash_profile ]; then
    mv -v  $HOME/.bash_profile $DOTFILES_BACKUP/bash/bash_profile-bkp
fi
ln -sf $DOTFILES_FOLDER/bash/bash_profile ~/.bash_profile

# profile
if [ -e $HOME/.profile ]; then
    mv -v  $HOME/.profile $DOTFILES_BACKUP/bash/profile-bkp
fi
ln -sf $DOTFILES_FOLDER/bash/profile ~/.profile


