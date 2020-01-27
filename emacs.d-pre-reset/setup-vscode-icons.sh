#!/bin/bash

mkdir -p ~/dotfiles/emacs.d/elpa/treemacs-20191121.626/icons/default/vscode

cp ~/dotfiles/emacs.d/elpa/vscode-icon-20191102.2010/icons/23/* ~/dotfiles/emacs.d/elpa/treemacs-20191121.626/icons/default/vscode
cd ~/dotfiles/emacs.d/elpa/treemacs-20191121.626/icons/default/vscode/

yes | mmv file_type_\* \#1
