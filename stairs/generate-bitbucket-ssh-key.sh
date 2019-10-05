
echo "## generate the key"
ssh-keygen 


echo "## starting procedure to add the generated key \
to the ssh-agent so i dont have to type the passphrase everytime"

echo "## start the ssh-agent"
eval `ssh-agent` 

ssh-add ~/.ssh/id_rsa 


echo "## now to go Bitbucket Settings > SSH Keys, and add the public key"
echo "## public key:"
cat ~/.ssh/id_rsa.pub
