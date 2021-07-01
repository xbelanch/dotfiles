#!/bin/bash

if [ -f "/etc/arch-release" ]; then
    echo "Arch update packages"
    sudo pacman -Syyuu && yay && npm update
else
    echo "Debian update packages"
    sudo apt-get update && sudo apt-get -y upgrade && npm update
fi
