# Download vim-plug
curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

# fzf
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf && ~/.fzf/install

# ag
sudo apt install silversearcher-ag
git clone git@github.com:thiamsantos/dotfiles.git /path/to/repo/dotfiles

# create symbolic link to config in the system
ln -s /path/to/repo/dotfiles/vim/vimrc ~/.vimrc
