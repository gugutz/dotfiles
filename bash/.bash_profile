#
# ~/.bash_profile
#

# source .profile and .bashrc (in that order), if exists
if [ -r ~/.profile ]; then . ~/.profile; fi;
if [ -r ~/.bashrc ]; then . ~/.bashrc; fi;

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

export YVM_DIR=/home/tau/.yvm
[ -r $YVM_DIR/yvm.sh ] && . $YVM_DIR/yvm.sh
