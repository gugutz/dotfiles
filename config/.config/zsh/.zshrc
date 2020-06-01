
#########################################
# If you come from bash you might have to change your $PATH.
# check if exporting this line in .profile is okay
# if not, uncomment it
export PATH=$PATH


# ZDOTDIR envvar is set with $HOME/.zshenv

#########################################
# my aliases
[[ -e $HOME/.config/aliases ]] && source $HOME/.config/aliases
[[ -e $HOME/.profile ]] && source $HOME/.profile

#########################################
# COMPLETION SYSTEM

# enable zsh internal completion system
# im adding this to try to autocomplete command flags like fish does
autoload -U compinit && compinit

# For autocompletion of command line switches for aliases
setopt COMPLETE_ALIASES

# For autocompletion with an arrow-key driven interface, add the following to:
# PS: To activate the menu, press Tab twice.
zstyle ':completion:*' menu select

# bash compatibility mode (to use bash asdf completion)
autoload bashcompinit
bashcompinit
# testes
autoload -U +X bashcompinit && bashcompinit
autoload -U +X compinit && compinit
############################################
# THEME AND UI
############################################

# enable colors
autoload -U colors && colors

# set theme
# Theme: PowerLevel10K

POWERLEVEL10K_DIR=$ZDOTDIR/themes/powerlevel10k
if [ ! -d "$POWERLEVEL10K_DIR" ]; then
    echo "## installing PowerLevel10K theme for zsh"
    git clone --depth=1 https://github.com/romkatv/powerlevel10k.git $POWERLEVEL10K_DIR
    # npm install --global pure-prompt
fi

[ -d "$POWERLEVEL10K_DIR" ] && source $POWERLEVEL10K_DIR/powerlevel10k.zsh-theme

# p10k config. To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ -f $ZDOTDIR/.p10k.zsh ]] && source $ZDOTDIR/.p10k.zsh

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi


# pure prompt

# add pure prompt to zsh path
#fpath+=("$HOME/.zsh/pure")

# # start prompt plugin
#autoload -U promptinit; promptinit
#prompt pure > /dev/null

#----------------------
# PYWAL

# Import colorscheme from 'wal' asynchronously
# &   # Run the process in the background.
# ( ) # Hide shell job control messages.
[[ -f ~/.cache/wal/sequences ]] && (cat ~/.cache/wal/sequences &)

# Alternative (synchronous - blocks terminal for 0-3ms)
# [[ -f ~/.cache/wal/sequences ]] && cat ~/.cache/wal/sequences

# To add support for TTYs this line can be optionally added.
[[ -f ~/.cache/wal/colors-tty ]] && source ~/.cache/wal/colors-tty.sh

############################################
# Functions
############################################

# Fancy cd that can cd into parent directory, if trying to cd into file.
# useful with ^F fuzzy searcher.
# source: https://github.com/slashbeast/conf-mgmt/blob/master/roles/home_files/files/DOTzshrc
cd() {
    if (( $+2 )); then
        builtin cd "$@"
        return 0
    fi

    if [ -f "$1" ]; then
        echo "${yellow}cd ${1:h}${NC}" >&2
        builtin cd "${1:h}"
    else
        builtin cd "${@}"
    fi
}

# Fancy progress function from Landley's Aboriginal Linux.
# Useful for long rm, tar and such.
# Usage:
#     rm -rfv /foo | dot_progress
dot_progress() {
    local i='0'
    local line=''

    while read line; do
        i="$((i+1))"
        if [ "${i}" = '25' ]; then
            printf '.'
            i='0'
        fi
    done
    printf '\n'
}

# Control-x-e to open current line in $EDITOR, awesome when writting functions or editing multiline commands.
autoload -U edit-command-line
zle -N edit-command-line
bindkey '^x^e' edit-command-line

#########################################
# PLUGINS SETTINGS

# vim mode plugin settings

MODE_CURSOR_VICMD="green block"
MODE_CURSOR_VIINS="#20d08a blinking bar"
MODE_CURSOR_SEARCH="#ff00ff steady underline"


#########################################
# ZSH settings

# wait only 10ms for key sequences
KEYTIMEOUT=1


# enable command auto-correction.
ENABLE_CORRECTION="true"

# display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# disable marking untracked files under VCS as dirty.
# This makes repository status check for large repositories much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# save history (show previous used commands)
HISTSIZE=10000
SAVEHIST=10000
[ ! -d $HOME/.cache/zsh ] && mkdir -p $HOME/.cache/zsh
[ ! -f $HOME/.cache/zsh/zsh_history ] && touch $HOME/.cache/zsh/zsh_history
HISTFILE=$HOME/.cache/zsh/zsh_history
# Ignore duplicate in history.
setopt hist_ignore_dups


#########################################

# fzf
[ -f $HOME/.fzf.zsh ] && source $HOME/.fzf.zsh

#########################################
# bind home/end keys
bindkey "${terminfo[khome]}" beginning-of-line
bindkey "${terminfo[kend]}" end-of-line

#########################################
# Tmux
ZSH_TMUX_AUTOSTART="true"
ZSH_TMUX_AUTOSTART_ONCE="true"

# if [ -z "$TMUX" ]
# then
#     tmux attach -t TMUX || tmux new -s TMUX
# fi
#
if [ -x "$(command -v tmux)" ]
then
    tmux new-session
fi

# THIS WAS GIVING ME ERRORS WITH EMACS PACKAGE `exec-path-from-shell`
# if [[ $DISPLAY ]]; then
#   [[ $- != *i* ]] && return
#   [[ -z "$TMUX" ]] && tmux -e zsh
# fi

#########################################
# FZF
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

#########################################
# asdf
[ -s "$HOME/.asdf/asdf.sh" ] && source $HOME/.asdf/asdf.sh
[ -s "$HOME/.asdf/completions/asdf.bash" ] && source $HOME/.asdf/completions/asdf.bash

#########################################
# nvm

# first the NVM_DIR env var is exported in .profile
# export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && source "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && source "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# Calling nvm use automatically in a directory with a .nvmrc file
# place this after nvm initialization!
autoload -U add-zsh-hook
load-nvmrc() {
  local node_version="$(nvm version)"
  local nvmrc_path="$(nvm_find_nvmrc)"

  if [ -n "$nvmrc_path" ]; then
    local nvmrc_node_version=$(nvm version "$(cat "${nvmrc_path}")")

    if [ "$nvmrc_node_version" = "N/A" ]; then
      nvm install
    elif [ "$nvmrc_node_version" != "$node_version" ]; then
      nvm use
    fi
  elif [ "$node_version" != "$(nvm version default)" ]; then
    echo "Reverting to nvm default version"
    nvm use default
  fi
}
add-zsh-hook chpwd load-nvmrc
load-nvmrc

#########################################
# Yarn

# first the YARN_DIR is exported in .profile
# export YVM_DIR=$HOME/.yvm
[ -r $YVM_DIR/yvm.sh ] && . $YVM_DIR/yvm.sh




#########################################
# Emacs mode

# bindkey -e

# VI mode
bindkey -v

# change cursor shape according to vi mode
# since zsh doenst use readline's inputrc file, but its own ZLE instaed, this has to be here

function zle-keymap-select zle-line-init
{
    # change cursor shape in iTerm2
    case $KEYMAP in
        vicmd)      print -n -- "\E]50;CursorShape=0\C-G";;  # block cursor
        viins|main) print -n -- "\E]50;CursorShape=1\C-G";;  # line cursor
    esac

    zle reset-prompt
    zle -R
}

function zle-line-finish
{
    print -n -- "\E]50;CursorShape=0\C-G"  # block cursor
}

zle -N zle-line-init
zle -N zle-line-finish
zle -N zle-keymap-select

#########################################
#
# My Binding and Macros

# zjsh does not use readline, instead it uses its own and more powerful zle. It does not read /etc/inputrc or ~/.inputrc. Zle has an emacs mode and a vi mode. By default, it tries to guess whether emacs or vi keys from the $EDITOR environment variable are desired. If it is empty, it will default to emacs. Change this with bindkey -e or bindkey -v respectively for emacs mode or vi mode.

# For a complete list of codes to use for each key, visit: http://zshwiki.org/home/zle/bindkeys
# You may want to call different history search commands, e.g.
# down-line-or-history or down-line-or-search (and up-*)
bindkey '^P' history-search-backward
bindkey '^N' history-search-forward
bindkey '\e[A' history-search-backward
bindkey '\e[B' history-search-forward
# The others should work already


#########################################
# plugins manual install


# colors for echo commands using tput command
# color must be reseted at the end of the line or the entire file will use previous used color

red=`tput setaf 1`
green=`tput setaf 2`
matrix_green=`tput setaf 46`
reset_color=`tput sgr0`
bg_white=`tput setab 7`


[ -d $ZDOTDIR/plugins ] && mkdir -p $ZDOTDIR/plugins

ZSH_PLUGINS=$ZDOTDIR/plugins

# my personal install_plugin function
install_plugin () {
  # limitation: only installs from github for now
  if [ -z $1 ]; then
    echo "Usage: $0 <args>"
    exit 1
  fi

  # $PLUGIN_DIR=$ZSH_PLUGINS/$1
  if [ ! -d "$ZSH_PLUGINS/$1" ]; then
    echo "${matrix_green}## installing $1 plugin${reset_color}" | pv -p -e -t -a -r
    git clone https://github.com/$2 $ZSH_PLUGINS/$1
  fi
}

# theme pure
# install_plugin pure sindresorhus/pure.git

install_plugin zsh-vim-mode softmoth/zsh-vim-mode.git
source $ZSH_PLUGINS/zsh-vim-mode/zsh-vim-mode.plugin.zsh

install_plugin you-should-use MichaelAquilina/zsh-you-should-use.git
source $ZSH_PLUGINS/you-should-use/you-should-use.plugin.zsh

install_plugin k supercrabtree/k.git
source $ZSH_PLUGINS/k/k.sh

install_plugin zsh-autosuggestions zsh-users/zsh-autosuggestions.git
source $ZSH_PLUGINS/zsh-autosuggestions/zsh-autosuggestions.zsh

install_plugin zsh-syntax-highlighting zsh-users/zsh-syntax-highlighting.git
source $ZSH_PLUGINS/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
