echo "## setup tmux config"

echo "## install tmux plugin manager"
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

echo "## install tmux prefix highlight plugin"
git clone https://github.com/tmux-plugins/tmux-prefix-highlight.git ~/.tmux/plugins/tmux-prefix-highlight

echo "## install xclip"
sudo pacman -S --noconfirm xclip
