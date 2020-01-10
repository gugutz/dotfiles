echo "## installing samba"
sudo pacman -S --noconfirm samba nautilus-share gvfs-smb
echo "## creating usershare folder"
sudo mkdir -p /var/lib/samba/usershare
echo "## creating sambashare user group"
sudo groupadd sambashare
echo "## setting permissions"
sudo chown root:sambashare /var/lib/samba/usershare
sudo chmod 1770 /var/lib/samba/usershare

# create the user and add to group sambashare
# but dont create its home folder (-M parameter)
echo "## adding user tau-smb"
sudo useradd -M -g sambashare tau-smb
echo "## creating password for new user tau-smb"
sudo passwd tau-smb
echo "## activating smbd service"
sudo systemctl enable smb nmb
sudo systemctl start smb nmb
chmod 701 /home/tau
