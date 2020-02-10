# install manjaro-printer software
sudo pacman -S --noconfirm --needed manjaro-printer

# add user to sys group
sudo gpasswd -a yourusername sys

# enable printing service
sudo systemctl enable --now org.cups.cupsd.service
