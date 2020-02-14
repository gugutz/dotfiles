###############################
# My personal .profile
###############################
#
# this file gets loaded by
# login shells at every
# session startup

# source .bashrc if it exists
if [ -f ~/.bashrc ]; then
    echo ".profile is sourcing " ~/.bashrc
    source ~/.bashrc
fi

# source .bashrc if it exists
# if [ -f ~/.zshrc ]; then
#    echo ".profile is sourcing " ~/.zshrc
#    source ~/.zshrc
# fi



###########################################
# EXPORT ENVIRONMENT VARIABLES
###########################################

#########################################
# Re-setting PATH
# add ~/bin to PATH
export PATH=$HOME/bin:/usr/local/bin:$PATH
# add /usr/local to PATH
export PATH=/usr/local/bin:$PATH
# add /opt to PATH
export PATH=/opt:$PATH

#########################################
# ZSH

# Export a envvar for ZSH
export ZSH=$HOME/.oh-my-zsh

#########################################
# ROFI

# export variable used by ROFI
export XDG_USER_CONFIG_DIR=$HOME/.config

#########################################
# ssh
export SSH_KEY_PATH="$HOME/.ssh/id_rsa"

#########################################
# Setting xterm to use 256 colors
export TERM=xterm-256color

#########################################
# Preferred editor for local and remote sessions
# Defining default Unix text editor and command line text viewr
if [ -n "$SSH_CONNECTION" ] || [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
  export EDITOR='vim'
else
  export EDITOR='emacs'
fi
export PAGER=/usr/bin/less

###########################################
# Manjaro i3 default .profile settings

export QT_QPA_PLATFORMTHEME="qt5ct"
export EDITOR=/usr/bin/vim
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"
# fix "xdg-open fork-bomb" export your preferred browser from here
export BROWSER=/usr/bin/firefox

###########################################
# Applications specific settings
###########################################

# NVM
export NVM_DIR="$HOME/.nvm"

# Yarn
export YVM_DIR=$HOME/.yvm

# Defining npm PATH and prefix (folder) to install global packages
PATH="$HOME/.node_modules/bin:$PATH"
export PATH=$PATH:/home/tau/.nvm/versions/node/v10.12.0/bin
# commented line bellow because NVM complains about it
# export npm_config_prefix=~/.node_modules

# Add gradle in PATH
export PATH=$PATH:/opt/gradle/gradle-4.8/bin

# add spring boot in path
if [ -f ~/.bashrc ]; then
    echo ".profile is sourcing " ~/.bashrc
    export PATH=$PATH:/opt/spring
fi
# Add $GOPATH env var
# export GOPATH=/srv/tau/code/go
# export PATH=$PATH:$GOPATH/bin
# export GOPATH=$(go env GOPATH)

# Load rbenv automatically
# eval "$(rbenv init -)"

# Load pyenv automatically
# export PATH="/home/tau/.pyenv/bin:$PATH"
# eval "$(pyenv init -)"
# eval "$(pyenv virtualenv-init -)"
