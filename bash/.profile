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
###########################################
# SYSTEM WIDE ENV VARS
###########################################

# add ~/bin to PATH
export PATH=$PATH:$HOME/bin

# add ~/.local/bin to PATH
export PATH=$PATH:$HOME/.local/bin

# add /usr/local to PATH
export PATH=$PATH:/usr/local/bin

# add /opt to PATH
export PATH=$PATH:/opt

#-----------------------------------------
# DEFAULT PROGRAMS

export EDITOR=/usr/bin/vim
export QT_QPA_PLATFORMTHEME="qt5ct"
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"
# fix "xdg-open fork-bomb" export your preferred browser from here
export BROWSER=/usr/bin/google-chrome-stable

#-----------------------------------------
# ssh
export SSH_KEY_PATH="$HOME/.ssh/id_rsa"

# Setting xterm to use 256 colors
export TERM=xterm-256color

export PAGER=/usr/bin/less

# NVM
export NVM_DIR="$HOME/.nvm"

# Yarn
export YVM_DIR="$HOME/.yvm"

# gradle
export PATH=$PATH:/opt/gradle/gradle-4.8/bin
# spring boot
export PATH=$PATH:/opt/spring

# if bat is installed, use it also for colored man pages
[ -x "$(command -v bat)" ] && export MANPAGER="sh -c 'col -bx | bat -l man -p'"


#########################################
# XDG Base Directories
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
