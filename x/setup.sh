echo "# symlinking xmodmap in $HOME"
if [ -e $HOME/.Xmodmap ]; then
    mv -v  $HOME/.Xmodmap $$HOME/.Xmodmap-bkp
fi
ln -sf $HOME/x/xmodmap ~/.Xmodmap
