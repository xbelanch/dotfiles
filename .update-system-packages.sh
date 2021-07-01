#!/bin/bash

upgrade_arch_system() {
    printf "\nUpgrading Arch system with YAY...\n"
    yay -Yc
    yay -Syu --devel --timeupdate
    yay -Ps
    printf "...Done upgrading the system\n"
}

if [ -f "/etc/arch-release" ]; then
    upgrade_arch_system
    npm outdated -g
else
    echo "Debian update packages"
    sudo apt-get update && sudo apt-get -y upgrade && npm outdated -g
fi
