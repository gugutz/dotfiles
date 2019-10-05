echo "## generating ssh key"
echo "## IMPORTANT: name the file id_rsa_github"
ssh-keygen -t rsa -b 4096 -C "gugutz@gmail.com"

eval "$(ssh-agent -s)"

echo "## adding the generated key to ssh"

ssh-add ~/.ssh/id_rsa_github

xclip -sel clip < ~/.ssh/id_rsa_github.pub
echo "## public key copied to clipboard! add it to github"
