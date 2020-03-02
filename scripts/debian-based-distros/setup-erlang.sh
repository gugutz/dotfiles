
echo "binary dependencies"
sudo apt install autoconf automake libncurses5-dev

echo "documentation dependencies"
sudo apt install xsltproc fop xmllint

echo "ODBC dependencies"
sudo apt install xsltproc fop xmllint


asdf plugin-add erlang
asdf install erlang 22.2
asdf global erlang 22.2

asdf plugin-add elixir
asdf install elixir 1.10.1
asdf global elixir 1.10.1

echo "Install or update hex package manager"
mix local.hex


echo "Install Phoenix Framework"
mix archive.install hex phx_new 1.4.13