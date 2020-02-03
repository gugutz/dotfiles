## THIS IS SPECIFIC TO MY SETUP!!!!!
## INSTALLS THE LASTEST NVIDIA DRIVER WITH SUPPORT FOR FERMI BASED CARDS
echo "## install my gpu drivers"
sudo mhwd -i pci video-nvidia-390xx

echo "## install Yay AUR helper"
sudo pacman -S --noconfirm yay

echo "## install pi-hole ad blocker server"
echo "## this setup requests confirmations"
echo "## --noconfirm not allowed"
# yay -S pi-hole-server

echo "## install firefox"
# sudo pacman -S --noconfirm firefox


echo "## install remmina remote desktop client"
sudo pacman -S --noconfirn remmina
echo "## install freerdp for Windows RDP connections on remmina"
sudo pacman -S --noconfirm freerdp

echo "## instal telegram desktop"
yay -S --noconfirm telegram-desktop

echo "## install discord"
echo "## creating symbolic link for discord appimage in local disk"
sudo ln -sf ~/apps/discord-stable-0.0.5-x86_64.AppImage /usr/local/bin/discord
# if getting key error...
# gpg --recv-keys --keyserver hkp://pgp.mit.edu A2C794A986419D8A
# discord client
# yay -S --noconfirm discord

echo "## install slack desktop"
yay -S --noconfirm slack-desktop

echo "## install peek gif recorder gui"
yay -S --noconfirm peek
echo "## install ArandR GUI for XrandR, a great dual monitor manager for linux"
sudo pacman -S --noconfirm arandr

echo "## install virtual box"
sudo pacman -S --noconfirm virtualbox
echo "## install linux 4.14 virtuabox host modules"
sudo pacman -S --noconfirm linux414-virtualbox-host-modules
sudo modprobe vboxdrv

echo "## install obs studio"
sudo pacman -S --noconfirm obs-studio

