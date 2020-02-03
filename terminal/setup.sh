TERMINAL_DIR="$HOME/dotfiles/terminal"
ST_DIR="$TERMINAL_DIR/st-0.8.2"


if [ -d "$ST_DIR" ]; then
    echo "## entering directory to download st stuff"
    cd $ST_DIR
else
    echo "## downloading st"
    wget -P $TERMINAL_DIR https://dl.suckless.org/st/st-0.8.2.tar.gz
    tar -xvf $TERMINAL_DIR/st-0.8.2.tar.gz --directory $TERMINAL_DIR
    cd $ST_DIR
fi


if [ ! -e "$ST_DIR/st-scrollback-0.8.2.diff" ]; then
    echo "## downloading and applying scrollback patch (Shift+PgUp/PgDown)"
    wget -P $ST_DIR https://st.suckless.org/patches/scrollback/st-scrollback-0.8.2.diff
fi
patch -Np1 -i st-scrollback-0.8.2.diff

if [ ! -e "$ST_DIR/st-scrollback-mouse-0.8.2.diff" ]; then
    echo "## downloading and applying mouse scrollback patch (Shift+WheelUp/WheelDown)"
    wget -P $ST_DIR https://st.suckless.org/patches/scrollback/st-scrollback-mouse-0.8.2.diff
fi
patch -Np1 -i st-scrollback-mouse-0.8.2.diff


if [ ! -e "$ST_DIR/st-clipboard-0.8.2.diff" ]; then
    echo "## downloading and applying clipboard patch"
    wget -P $ST_DIR https://st.suckless.org/patches/clipboard/st-clipboard-0.8.2.diff
fi
patch -Np1 -i st-clipboard-0.8.2.diff


if [ ! -e "$ST_DIR/st-rightclickpaste-0.8.2.diff" ]; then
    echo "## downloading and installing rightclickpaste patch"
    wget -P $ST_DIR https://st.suckless.org/patches/rightclickpaste/st-rightclickpaste-0.8.2.diff
fi
patch -Np1 -i st-rightclickpaste-0.8.2.diff

if [ ! -e "$ST_DIR/st-alpha-0.8.2.diff" ]; then
    echo "## downloading and installing alpha (transparency) patch"
    wget -P $ST_DIR https://st.suckless.org/patches/alpha/st-alpha-0.8.2.diff
fi
patch -Np1 -i st-alpha-0.8.2.diff


echo "# building st"
make install
