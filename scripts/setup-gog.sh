echo "Instalando a versão stable do programa"

sudo snap install gog-galaxy-wine

sudo snap connect gog-galaxy-wine:hardware-observe

sudo snap connect gog-galaxy-wine:process-control

echo "Instalando a versão candidate do programa"

sudo snap install gog-galaxy-wine --candidate

sudo snap connect gog-galaxy-wine:hardware-observe

sudo snap connect gog-galaxy-wine:process-control

echo "Instalando a versão beta do programa"

sudo snap install gog-galaxy-wine --beta

sudo snap connect gog-galaxy-wine:hardware-observe

sudo snap connect gog-galaxy-wine:process-control

echo "Instalando a versão edge do programa"

sudo snap install gog-galaxy-wine --edge

sudo snap connect gog-galaxy-wine:hardware-observe

sudo snap connect gog-galaxy-wine:process-control


