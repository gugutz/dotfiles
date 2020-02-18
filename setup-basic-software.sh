## THIS IS SPECIFIC TO MY SETUP!!!!!
## INSTALLS THE LASTEST NVIDIA DRIVER WITH SUPPORT FOR FERMI BASED CARDS

echo "## install firefox"
sudo pacman -S --noconfirm --needed firefox

echo "## install google chrome"
yay -S --noconfirm --needed google-chrome


#echo "## install remmina remote desktop client"
#sudo pacman -S --noconfirm remmina
#echo "## install freerdp for Windows RDP connections on remmina"
#sudo pacman -S --noconfirm freerdp

echo "## instal telegram desktop"
yay -S --noconfirm --needed telegram-desktop

echo "## install whatsapp (nativefier)"
yay -S --noconfirm --needed whatsapp-nativefier

#echo "## install discord"
#echo "## creating symbolic link fqor discord appimage in local disk"
#sudo ln -sf ~/apps/discord-stable-0.0.5-x86_64.AppImage /usr/local/bin/discord
# if getting key error...
# gpg --recv-keys --keyserver hkp://pgp.mit.edu A2C794A986419D8A
# discord client
# yay -S --noconfirm discord

#echo "## install slack desktop"
#yay -S --noconfirm --needed slack-desktop

echo "## install peek gif recorder gui"
yay -S --noconfirm --needed peek
echo "## install ArandR GUI for XrandR, a great dual monitor manager for linux"
sudo pacman -S --noconfirm --needed arandr

#echo "## install virtual box"
#sudo pacman -S --noconfirm --needed virtualbox
#echo "## install linux 4.14 virtuabox host modules"
#sudo pacman -S --noconfirm linux414-virtualbox-host-modules
#sudo modprobe vboxdrv

#echo "## install obs studio"
#sudo pacman -S --noconfirm --needed obs-studio

