
#########################################
# If you come from bash you might have to change your $PATH.
# check if exporting this line in .profile is okay
# if not, uncomment it
export PATH=$PATH

# ZDOTDIR envvar is set with $HOME/.zshenv, but just in case...
[[ -z "${DEPLOY_ENV}" ]] && ZDOTDIR="$HOME/.config/zsh"

#########################################
# my aliases
[[ -e $HOME/.config/aliases ]] && source $HOME/.config/aliases
[[ -e $HOME/.profile ]] && source $HOME/.profile

# import neoway aliases
[[ -e $HOME/.neoway-aliases ]] && source $HOME/.neoway-aliases

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
POWERLEVEL10K=$ZDOTDIR/themes/powerlevel10k/powerlevel10k.zsh-theme
if [ ! -f "$POWERLEVEL10K" ]; then
    echo "## installing PowerLevel10K theme for zsh"
    git clone --depth=1 https://github.com/romkatv/powerlevel10k.git $POWERLEVEL10K_DIR
    # npm install --global pure-prompt
fi

[ -f "$POWERLEVEL10K" ] && source $POWERLEVEL10K

# p10k config. To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ -f $ZDOTDIR/.p10k.zsh ]] && source $ZDOTDIR/.p10k.zsh

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
    source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi



# first check if the command is installed
if ! command -v "fuck" >/dev/null; then
    echo "Installing fuck..."
    pip3 install thefuck --user
fi

# first check if the command is installed
if ! command -v "bat" >/dev/null; then
    echo "Installing bat..."
    sudo apt install bat
    mkdir -p ~/.local/bin
    ln -s /usr/bin/batcat ~/.local/bin/bat
fi


# first check if the command is installed
if ! command -v "jdupes" >/dev/null; then
    echo "Installing jdupes..."
    sudo apt install jdupes
fi

# first check if the command is installed
if ! command -v "tldr" >/dev/null; then
    echo "Installing tldr..."
    sudo apt install tldr
fi


# # first check if the command is installed
# if ! command -v "exa" >/dev/null; then
#     echo "Installing exa..."
#     sudo apt install exa
# fi

# # first check if the command is installed
# if ! command -v "duf" >/dev/null; then
#     echo "Installing duf..."
#     sudo apt install duf
# fi




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

####################################################
# Functions
####################################################

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

####################################################
# PLUGINS SETTINGS
####################################################

# vim mode plugin settings

MODE_CURSOR_VICMD="green block"
MODE_CURSOR_VIINS="#20d08a blinking bar"
MODE_CURSOR_SEARCH="#ff00ff steady underline"



####################################################
# ZSH settings
####################################################

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



####################################################
# fzf
####################################################

# Source for most functions and bindings:  http://owen.cymru/fzf-ripgrep-navigate-with-bash-faster-than-ever-before/

# Bindings:
# Ctrl + r - search through bash history with fzf
# Ctrl + p - edit a file in vim from fzf
# mv dir/** - expand a directory with (**) and select from fzf
# Alt + c - change directory from fzf - see the update at the bottom for faster search with bfs.
# Ctrl + t - insert file from fzf into command

export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow -g "!{.git,node_modules}/*" 2> /dev/null'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
# export FZF_ALT_C_COMMAND="bfs -type d -nohidden"
export FZF_ALT_C_COMMAND="cd ~/; bfs -type d -nohidden | sed s/^\./~/"
export FZF_DEFAULT_OPTS='--bind J:down,K:up --reverse --ansi --multi'

[ -f $HOME/.fzf.zsh ] && source $HOME/.fzf.zsh


# install fzf if its not installed
if [ ! -x "$(command -v fzf)"  ]; then
    sudo apt install fzf
fi

# enable fzf keybindings and fuzzy auto-completion for zsh
# default keybindings>: (CTRL-T / CTRL-R / ALT-C)
if [ -x "$(command -v fzf)"  ]; then
    source /usr/share/doc/fzf/examples/key-bindings.zsh
    source /usr/share/doc/fzf/examples/completion.zsh
fi


# calls the fzf_log functions bellow
alias log="fzf_log"


sf() {
  if [ "$#" -lt 1 ]; then echo "Supply string to search for!"; return 1; fi
  printf -v search "%q" "$*"
  include="tsx,vim,ts,yml,yaml,js,json,php,md,styl,pug,jade,html,config,py,cpp,c,go,hs,rb,conf,fa,lst,graphql"
  exclude=".config,.git,node_modules,vendor,build/,yarn.lock,*.sty,*.bst,*.coffee,dist"
  rg_command='rg --column --line-number --no-heading --fixed-strings --ignore-case --no-ignore --hidden --follow --color "always" -g "*.{'$include'}" -g "!{'$exclude'}/*"'
  files=`eval $rg_command $search | fzf --ansi --multi --reverse | awk -F ':' '{print $1":"$2":"$3}'`
  [[ -n "$files" ]] && ${EDITOR:-vim} $files
}

sfu() {
  if [ "$#" -lt 1 ]; then echo "Supply string to search for!"; return 1; fi
  printf -v search "%q" "$*"
  include="ts,yml,yaml,js,json,php,md,styl,pug,jade,html,config,py,cpp,c,go,hs,rb,conf,fa,lst"
  exclude=".config,.git,node_modules,vendor,build,yarn.lock,*.sty,*.bst,*.coffee,dist"
  rg_command='rg -m1 --column --line-number --no-heading --fixed-strings --ignore-case --no-ignore --hidden --follow --color "always" -g "*.{'$include'}" -g "!{'$exclude'}/*"'
  files=`eval $rg_command $search | fzf --ansi --multi --reverse | awk -F ':' '{print $1":"$2":"$3}'`
  [[ -n "$files" ]] && ${EDITOR:-vim} $files
}


fc() {
  hash=$(git log --color=always --format="%C(auto)%h%d %s %C(black)%C(bold)%cr" "$@" |  fzf | awk '{print $1}')
  git checkout $hash
}

gc() {
  hash=$(git log --color=always --format="%C(auto)%h%d %s %C(black)%C(bold)%cr" "$@" |  fzf | awk '{print $1}')
  gopen $hash
}

fzf_log() {
  hash=$(git log --color=always --format="%C(auto)%h%d %s %C(black)%C(bold)%cr" "$@" |  fzf | awk '{print $1}')
  echo $hash | xclip
  git showtool $hash
}

tm() {
  [[ -n "$TMUX" ]] && change="switch-client" || change="attach-session"
  if [ $1 ]; then
    tmux $change -t "$1" 2>/dev/null || (tmux new-session -d -s $1 && tmux $change -t "$1"); return
  fi
  session=$(tmux list-sessions -F "#{session_name}" 2>/dev/null | fzf --exit-0) &&  tmux $change -t "$session" || echo "No sessions found."
}

branch() {
  local branches branch
  branches=$(git for-each-ref --count=30 --sort=-committerdate refs/heads/ --format="%(refname:short)") &&
  branch=$(echo "$branches" |
           fzf-tmux -d $(( 2 + $(wc -l <<< "$branches") )) +m) &&
  git checkout $(echo "$branch" | sed "s/.* //" | sed "s#remotes/[^/]*/##")
}

fvim() {
  local IFS=$'\n'
  local files=($(fzf-tmux --query="$1" --multi --select-1 --exit-0))
  [[ -n "$files" ]] && ${EDITOR:-vim} "${files[@]}"
}

c() {
  local cols sep google_history open
  cols=$(( COLUMNS / 3 ))
  sep='{::}'

  if [ "$(uname)" = "Darwin" ]; then
    google_history="$HOME/Library/Application Support/Google/Chrome/Default/History"
    open=open
  else
    google_history="$HOME/.config/google-chrome/Profile 1/History"
    open=xdg-open
  fi
  cp -f "$google_history" /tmp/h
  sqlite3 -separator $sep /tmp/h \
    "select substr(title, 1, $cols), url
     from urls order by last_visit_time desc" |
  awk -F $sep '{printf "%-'$cols's  \x1b[36m%s\x1b[m\n", $1, $2}' |
  fzf --ansi --multi | sed 's#.*\(https*://\)#\1#' | xargs $open > /dev/null 2> /dev/null
}

gopen() {
    project=$(git config --local remote.origin.url | sed s/git@github.com\:// | sed s/\.git//)
    url="http://github.com/$project/commit/$1"
    xdg-open $url
}

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
# asdf
[ -s "$HOME/.asdf/asdf.sh" ] && source $HOME/.asdf/asdf.sh
[ -s "$HOME/.asdf/completions/asdf.bash" ] && source $HOME/.asdf/completions/asdf.bash

#########################################
# nvm

# first the NVM_DIR env var is exported in .profile
# export NVM_DIR="$HOME/.nvm"
export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm
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

# zsh does not use readline, instead it uses its own and more powerful zle.
# It does not read /etc/inputrc or ~/.inputrc. Zle has an emacs mode and a vi mode.
# By default, it tries to guess whether emacs or vi keys from the $EDITOR environment variable are desired.
# If it is empty, it will default to emacs.
# Change this with bindkey -e or bindkey -v respectively for emacs mode or vi mode.

# For a complete list of codes to use for each key, visit: http://zshwiki.org/home/zle/bindkeys
# You may want to call different history search commands, e.g.
# down-line-or-history or down-line-or-search (and up-*)
bindkey '^P' history-search-backward
bindkey '^N' history-search-forward
bindkey '\e[A' history-search-backward
bindkey '\e[B' history-search-forward

autoload -U history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^[[A" history-beginning-search-backward-end
bindkey "^[[B" history-beginning-search-forward-end
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

# install_plugin you-should-use MichaelAquilina/zsh-you-should-use.git
# source $ZSH_PLUGINS/you-should-use/you-should-use.plugin.zsh

install_plugin k supercrabtree/k.git
source $ZSH_PLUGINS/k/k.sh

install_plugin zsh-autosuggestions zsh-users/zsh-autosuggestions.git
source $ZSH_PLUGINS/zsh-autosuggestions/zsh-autosuggestions.zsh

install_plugin zsh-syntax-highlighting zsh-users/zsh-syntax-highlighting.git
source $ZSH_PLUGINS/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh




alias ls='ls --color=auto'
alias grep='grep --colour=auto'
alias egrep='egrep --colour=auto'
alias fgrep='fgrep --colour=auto'

# alias cat='batcat'
alias df='duf'
# alias ls='exa'




# set autoload path
fpath=(~/.config/zsh/functions "${fpath[@]}")
# autoload all files in fpath
# autoload -U $fpath[1]/*(.:t)

# move cursor to end of line after history search completion
autoload -Uz  kp fp ll
