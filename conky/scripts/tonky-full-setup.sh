sudo su

# installing hddtemp
pacman -S --noconfirm sudo chmod +s /usr/sbin/hddtemp
## giving permission to user to execute hddtemp to use in conky
echo "## install Conky"
sudo pacman -S conky
echo "## creating fonts directory to use with conky"
mkdir -p ~/.fonts
cd ~/.fonts
echo "## downloading font pack."
wget https://www.dropbox.com/s/vbc3ehoxkq42k27/Fonts_conky.tar.gz?dl=0 -O fonts-conky.tar.gz
tar -vzxf fonts-conky.tar.gz
echo "## cleaning downloaded files"
rm -fr Fonts_conky.tar.gz
echo "##updating font cache"
sudo fc-cache -vf
