echo "## Create symbolic link to vim config"

if [ ! -d ~/.vim ]; then
    mv -v  ~/.vim ~/.vim-bkp
fi
ln -s ~/dotfiles/vim ~/.vim

# Download vim-plug
if [ ! -e ~/.vim/autoload/plug.vim ]; then
    echo "# downloading VimPlug"
    curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
else
    echo "# VimPlug already installed in this system"
fi


# fzf
if [ ! -d ~/.fzf ]; then
    echo "# installing fzf"
    git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf && ~/.fzf/install
else
    echo "fzf already installed"
fi

# install ag
sudo pacman -S --noconfirm --needed the_silver_searcher


# Create symbolic link to my vim config


echo "## installing vim plugins"
vim +'PlugInstall --sync' +qa

