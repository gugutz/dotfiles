#!/bin/bash

# Create symbolic link to my i3 config
mv ~/.config/i3/config ~/.config/i3/config-bkp
ln -s ~/taunix/i3/config ~/.config/i3/config

# Create symbolic link to bash settings
mv ~/.bashrc ~/.bashrc-bkp
mv ~/.profile ~/.profile-bkp
mv ~/.bash_profile ~/.bash_profile-bkp
ln -s ~/taunix/bash/bashrc ~/.bashrc
ln -s ~/taunix/bash/profile ~/.profile
ln -s ~/taunix/bash/bash_profile ~/.bash_profile

# Create symbolic link to my zsh config
mv ~/.zshrc ~/.zshrc-bkp
ln -s ~/taunix/zsh/zshrc ~/.zshrc


# Create symbolic link to my tmux config
mv ~/.tmux.conf ~/.tmux.conf.bkp
ln -s ~/taunix/tmux/tmux.conf ~/.tmux.conf

# Create symbolic link to my vim config
mv ~/.vimrc ~/.vimrc-bkp
ln -s ~/taunix/vim/tau.vim ~/.vimrc


