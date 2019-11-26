#!/bin/bash

echo "Uncomment the following line on /etc/httpd/conf/httpd.conf"
echo "LoadModule cgi_module modules/mod_cgi.so"
read -rsp $'Press enter to load the file in Vim...\n'-n1 key
vim /etc/httpd/conf/httpd.conf

echo "add the following line at or near the end of httpd.conf:"
echo "Include conf/extra/httpd-zoneminder.conf"
read -rsp $'Press enter to load the file in Vim...\n'-n1 key
vim /etc/httpd/conf/httpd.conf



echo "Uncomment the following lines on /etc/php/php.ini"
echo "extension=apcu
     extension=ftp
     extension=gd
     extension=gettext
     extension=pdo_mysql
     extension=sockets
     extension=zip"

read -rsp $'Press enter to load the file in Vim...\n'-n1 key
vim /etc/php/php.ini

echo "Also change the timezone to:"
echo "date.timezone 'America/Sao_Paulo'"
echo "PS: use Double Quotes"

read -rsp $'Press enter to load the file in Vim...\n'-n1 key
vim /etc/php/php.ini
