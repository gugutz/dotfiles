echo "install dependencies for java"
sudo apt install jq

echo "setup java environment"
asdf plugin-add maven https://github.com/skotchpine/asdf-maven
asdf plugin-add gradle https://github.com/rfrancis/asdf-gradle

echo "# 1. Install Default OpenJDK"
sudo apt install default-jdk

echo "# Confirm the installation and check the version typing following command."
java -version



