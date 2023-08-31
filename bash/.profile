# Observations about profile:
# When you open a new terminal session, bash is run as an interactive non-login shell.
# Because .profile is run only for a non-interactive login shell, starting a terminal session does not run it.

###########################################
# MY PERSONAL .PROFILE
###########################################
#
# this file gets loaded by login shells at every session startup

# UPDATE: appearantly bash and zsh are considered login shells,
# beucase they would source profile on every enter
# ... so i dont need to source this anymore.

# source .bashrc if it exists
# if [ -f ~/.bashrc ]; then
#     echo ".profile is sourcing " ~/.bashrc
#     source ~/.bashrc
# fi

# source .bashrc if it exists
# if [ -f ~/.zshrc ]; then
#    echo ".profile is sourcing " ~/.zshrc
#    source ~/.zshrc
# fi

# prints message to indicate sourcing of this file
echo ".profile is being sourced."

#########################################
# XDG BASE DIRECTORIES SETUP
#########################################

# user directories
export XDG_USER_CONFIG_DIR="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_RUNTIME_DIR="$HOME/runtimedir"
export XDG_CONFIG_HOME="$HOME/.config"

# system directories
export XDG_DATA_DIRS=/usr/local/share/:/usr/share/
export XDG_CONFIG_DIRS=/etc/xdg

###########################################
# PATH SETUP
###########################################

# add several usefull folders to executable path
PATH=$PATH:$HOME/bin
PATH=$PATH:$HOME/.local/bin
PATH=$PATH:/usr/local/bin
PATH=$PATH:$HOME/.dotnet/tools
PATH=$PATH:$HOME/.asdf/shims

# add /opt to PATH
PATH=$PATH:/opt
PATH=$PATH:/opt/bin
PATH=$PATH:/opt/dotnet

export PATH

# Setting xterm to use 256 colors
export TERM=xterm-256color

###########################################
# BSPWM RELATED
###########################################

export BSPWM_SOCKET="/tmp/bspwm-socket"

# add my bspwm scripts folder to path
export PATH=$PATH:"$XDG_CONFIG_HOME/bspwm/scripts"
# add my sxhkd scripts folder to path
export PATH=$PATH:"$XDG_CONFIG_HOME/sxhkd/scripts"

export PATH=$PATH:"$XDG_CONFIG_HOME/nvm/versions/node/v14.15.1/lib/node_modules"

# fix idea opening stuck on grey screen in arch + bspwm
# source: https://stackoverflow.com/questions/33424736/intellij-idea-14-on-arch-linux-opening-to-grey-screen/34419927
export _JAVA_AWT_WM_NONREPARENTING=1

#########################################
# DEFAULT APPLICATIONS ENVS
#########################################

export TERMINAL=/usr/bin/alacritty
# export EDITOR=/usr/bin/vim
export EDITOR=/usr/bin/emacs
export VISUAL=/usr/bin/vim
export GUI_EDITOR=/usr/bin/micro
export QT_QPA_PLATFORMTHEME="qt5ct" export GTK2_RC_FILES="$HOME/.gtkrc-2.0"
# fix "xdg-open fork-bomb" export your preferred browser from here
# export BROWSER=/usr/bin/google-chrome-stable
# export BROWSER=/usr/bin/firefox
export BROWSER=/usr/bin/google-chrome-stable
# export BROWSER=/usr/bin/opera
export PAGER=/usr/bin/less

# if bat is installed, use it also for colored man pages
[ -x "$(command -v bat)" ] && export MANPAGER="sh -c 'col -bx | bat -l man -p'"

#-----------------------------------------
# ssh
export SSH_KEY_PATH="$HOME/.ssh/id_rsa"

#########################################
# OTHER APPLICATIONS ENVS
#########################################

# NVM
export NVM_DIR="$HOME/.nvm"

# Yarn
export YVM_DIR="$HOME/.yvm"

# FZF default options for all terminals
export FZF_DEFAULT_OPTS='--height 40% --layout=reverse --border'

export PATH="$HOME/.poetry/bin:$PATH"
