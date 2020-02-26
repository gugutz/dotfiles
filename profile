###########################################
# MY PERSONAL .PROFILE
###########################################
#
# this file gets loaded by
# login shells at every
# session startup

# UPDATE: appearantly bash and zsh are considered login shells, beucase they would source profile on every enter
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

###########################################
# SYSTEM WIDE ENV VARS
###########################################

# add ~/bin to PATH
export PATH=$PATH:$HOME/bin

# add /usr/local to PATH
export PATH=$PATH:/usr/local/bin

# add /opt to PATH
export PATH=$PATH:/opt

#-----------------------------------------
# EDITOR

# If Neovim is available, make it the default editor. Else use vim
if [ -f "/usr/bin/nvim"]; then
  export EDITOR=/usr/bin/nvim
else
  export EDITOR=/usr/bin/vim
fi
#-----------------------------------------

# Preferred editor for local and remote sessions
if [ -n "$SSH_CONNECTION" ] || [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
  export EDITOR='vim'
fi
#-----------------------------------------

# Manjaro i3 default .profile settings

export QT_QPA_PLATFORMTHEME="qt5ct"
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"

# fix "xdg-open fork-bomb" export your preferred browser from here
export BROWSER=/usr/bin/firefox
#-----------------------------------------

# ssh
export SSH_KEY_PATH="$HOME/.ssh/id_rsa"

#-----------------------------------------

# Setting xterm to use 256 colors
export TERM=xterm-256color
#-----------------------------------------

# not sure what this is for
export PAGER=/usr/bin/less


###########################################
# APPLICATIONS SPECIFIC ENV VARS
###########################################

#-----------------------------------------
# ZSH

export ZSH=$HOME/.oh-my-zsh
#-----------------------------------------

# NVM
export NVM_DIR="$HOME/.nvm"
#-----------------------------------------

# Yarn
export YVM_DIR=$HOME/.yvm
#-----------------------------------------

# Defining npm PATH and prefix (folder) to install global packages
PATH="$HOME/.node_modules/bin:$PATH"
export PATH=$PATH:/home/tau/.nvm/versions/node/v10.12.0/bin
# commented line bellow because NVM complains about it
# export npm_config_prefix=~/.node_modules
#-----------------------------------------

# Add gradle in PATH
export PATH=$PATH:/opt/gradle/gradle-4.8/bin
#-----------------------------------------

# add spring boot in path
if [ -f ~/.bashrc ]; then
    echo ".profile is sourcing " ~/.bashrc
    export PATH=$PATH:/opt/spring
fi
#-----------------------------------------

# Golang
# ps: this is done automatically if using asdf

# export GOPATH=/srv/tau/code/go
# export PATH=$PATH:$GOPATH/bin
# export GOPATH=$(go env GOPATH)
#-----------------------------------------

# Load rbenv automatically
# eval "$(rbenv init -)"
#-----------------------------------------

# Load pyenv automatically
# export PATH="/home/tau/.pyenv/bin:$PATH"
# eval "$(pyenv init -)"
# eval "$(pyenv virtualenv-init -)"


#########################################
# XDG Base Directories
#########################################

export XDG_USER_CONFIG_DIR=$HOME/.config

# defines the base directory relative to which user specific data files should be stored. If $XDG_DATA_HOME is either not set or empty, a default equal to $HOME/.local/share should be used.
export XDG_DATA_HOME=$HOME/.local/share

# defines the base directory relative to which user specific configuration files should be stored.
# if $XDG_CONFIG_HOME is either not set or empty, a default equal to $HOME/.config should be used
# If $XDG_DATA_DIRS is either not set or empty, a value equal to /usr/local/share/:/usr/share/ should be used.
export XDG_CONFIG_HOME=$HOME/.config

# Defines the preference-ordered set of base directories to search for data files in addition to the $XDG_DATA_HOME base directory
# The directories in $XDG_DATA_DIRS should be seperated with a colon ':'.
export XDG_DATA_DIRS=/usr/local/share/:/usr/share/

# $XDG_CONFIG_DIRS defines the preference-ordered set of base directories to search for configuration files in addition to the $XDG_CONFIG_HOME base directory. The directories in $XDG_CONFIG_DIRS should be seperated with a colon ':'.
# If $XDG_CONFIG_DIRS is either not set or empty, a value equal to /etc/xdg should be used.
export XDG_CONFIG_DIRS=/etc/xdg

# The order of base directories denotes their importance; the first directory listed is the most important. When the same information is defined in multiple places the information defined relative to the more important base directory takes precedent. The base directory defined by $XDG_DATA_HOME is considered more important than any of the base directories defined by $XDG_DATA_DIRS. The base directory defined by $XDG_CONFIG_HOME is considered more important than any of the base directories defined by $XDG_CONFIG_DIRS.

# $XDG_CACHE_HOME defines the base directory relative to which user specific non-essential data files should be stored.
# If $XDG_CACHE_HOME is either not set or empty, a default equal to $HOME/.cache should be used.
    export XDG_CACHE_HOME=$HOME/.cache

# $XDG_RUNTIME_DIR defines the base directory relative to which user-specific non-essential runtime files and other file objects (such as sockets, named pipes, ...) should be stored.
# The directory MUST be owned by the user, and he MUST be the only one having read and write access to it. Its Unix access mode MUST be 0700.

# The lifetime of the directory MUST be bound to the user being logged in. It MUST be created when the user first logs in and if the user fully logs out the directory MUST be removed. If the user logs in more than once he should get pointed to the same directory, and it is mandatory that the directory continues to exist from his first login to his last logout on the system, and not removed in between. Files in the directory MUST not survive reboot or a full logout/login cycle.

# The directory MUST be on a local file system and not shared with any other system. The directory MUST by fully-featured by the standards of the operating system. More specifically, on Unix-like operating systems AF_UNIX sockets, symbolic links, hard links, proper permissions, file locking, sparse files, memory mapping, file change notifications, a reliable hard link count must be supported, and no restrictions on the file name character set should be imposed. Files in this directory MAY be subjected to periodic clean-up. To ensure that your files are not removed, they should have their access time timestamp modified at least once every 6 hours of monotonic time or the 'sticky' bit should be set on the file.

# If $XDG_RUNTIME_DIR is not set applications should fall back to a replacement directory with similar capabilities and print a warning message. Applications should use this directory for communication and synchronization purposes and should not place larger files in it, since it might reside in runtime memory and cannot necessarily be swapped out to disk.
export XDG_RUNTIME_DIR=$HOME/runtimedir