#!/bin/bash

upgrade_arch_system() {
    printf "\nUpgrading Arch system with YAY...\n"
    yay -Yc
    yay -Syu --devel --timeupdate
    yay -Ps
    printf "...Done upgrading the system\n"
}

upgrade_debian_system() {
    printf "\nUpgrading Debian system with Apt...\n"
    sudo apt-get update && sudo apt-get -y upgrade
    printf "...Done upgrading the system\n"
}

check_outdated_npm_packages() {
    if which npm 2>&1 >/dev/null; then
        printf "\nCheck outdated global npm installed packages\n"
        npm outdated -g
    else
           printf "\nNPM is not installed on your system\n"
    fi
}

# Update system
if [ -f "/etc/arch-release" ]; then
    upgrade_arch_system
else
    upgrade_debian_system
fi

# Check outdated npm installed global packages
check_outdated_npm_packages
