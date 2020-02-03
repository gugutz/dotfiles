TMUX_PLUGINS="~/.tmux/plugins"

echo "## setup tmux config"

<<<<<<< HEAD
echo "## install tmux"
sudo pacman -S --noconfirm --needed xclip

echo "## install tmux plugin manager"
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
=======
echo "## Create symbolic link to my tmux config"
# first test if config file exists and is a unix regular file
if [ -d ~/.tmux ]; then
    mv -v  ~/.tmux ~/.tmux-bkp
fi
ln -s ~/dotfiles/tmux ~/.tmux

echo "Tmux Plugin Manager"
TPM=$TMUX_PLUGINS/tpm
if [ ! -d "$TPM" ]; then
    echo "## install tmux plugin manager"
    git clone https://github.com/tmux-plugins/tpm $TMUX_PLUGINS/tpm
else
    echo "Tmux Plugin Manager already installed in $TPM"
fi
>>>>>>> c120ed187c6acace1ee93129809541974f23774a

echo "Tmux Prefix Highlight"
PREFIX_HIGHLIGHT=$TMUX_PLUGINS/tmux-prefix-highlight
if [ ! -d "$PREFIX_HIGHLIGHT" ]; then
    echo "## install tmux prefix highlight plugin"
    git clone https://github.com/tmux-plugins/tmux-prefix-highlight.git $TMUX_PLUGINS/tmux-prefix-highlight
else
    echo "Tmux Plugin Manager already installed in $PREFIX_HIGHLIGHT"
fi

echo "## install xclip"
sudo pacman -S --noconfirm --needed xclip

# TMUXINATOR
echo "## installing Tmuxinator"
sudo pacman -S --noconfirm --needed tmux
echo "## install tmuxinator"
gem install tmuxinator

echo "## Create symbolic link to tmuxinator projects"
# first test if config file exists and is a unix regular file
if [ ! -d ~/.config/tmuxinator ]; then
    mv -v  ~/.config/tmuxinator ~/.config/tmuxinator-bkp
fi
ln -s ~/dotfiles/tmuxinator ~/.config/tmuxinator
