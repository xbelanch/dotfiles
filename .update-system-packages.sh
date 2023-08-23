#!/bin/bash

bold=$(tput bold)
purple=$(tput setaf 5)
red=$(tput setaf 1)
yellow=$(tput setaf 3)
green=$(tput setaf 2)
blue=$(tput setaf 4)
normal=$(tput sgr0)

upgrade_arch_system() {
    printf "\n${bold}${green}=====================================${normal}\n"
    printf "${bold}${green}Upgrading Arch system with YAY...${normal}\n"
    printf "${bold}${green}=====================================${normal}\n"
    yay -Yc
    yay -Syu --devel --timeupdate
    yay -Ps
    printf "${bold}${green}=====================================${normal}\n"
    printf "${bold}${green}...Done upgrading the system${normal}\n"
    printf "${bold}${green}=====================================${normal}\n"
}

remove_arch_cache() {
    printf "\n${bold}${red}=====================================${normal}\n"
    printf "${bold}${red}Removing all the unnecessary stuff...${normal}\n"
    printf "${bold}${red}=====================================${normal}\n"
    yes | LC_ALL=en_US.UTF-8 yay -Scc
    printf "\n${bold}${red}=====================================${normal}\n"
    printf "${bold}${red}Done${normal}\n"
    printf "${bold}${red}=====================================${normal}\n"
}

upgrade_debian_system() {
    printf "\nUpgrading Debian system with Apt...\n"
    sudo apt-get update && sudo apt-get -y upgrade
    printf "...Done upgrading the system\n"
}

check_outdated_rubygems() {
    if command -v gem &>/dev/null; then
        printf "\n${bold}${green}Update ruby gems${normal}\n"
        gem update
    else
        printf "\n${bold}${red}Ruby gems is not installed on your system${normal}\n"
    fi
}

check_outdated_npm_packages() {
    if command -v npm &>/dev/null; then
        printf "\n${bold}${yellow}Check outdated global npm installed packages${normal}\n"
        npm outdated -g
    else
           printf "\n${bold}${red}NPM is not installed on your system${normal}\n"
    fi
}

# Update system
if [ -f "/etc/arch-release" ]; then
    upgrade_arch_system
    remove_arch_cache
else
    upgrade_debian_system
fi

# Check outdated ruby gems and npm packages
check_outdated_rubygems
check_outdated_npm_packages
