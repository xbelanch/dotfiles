#!/bin/bash

purple=$(tput setaf 13)
red=$(tput setaf 9)
yellow=$(tput setaf 11)
green=$(tput setaf 10)
blue=$(tput setaf 5)
normal=$(tput sgr0)

upgrade_arch_system() {
    printf "${yellow}Upgrading Arch system with YAY...\n"
    yay -Yc
    yay -Syu --devel --timeupdate
    yay -Ps
    printf "...Done upgrading the system${normal}\n"
}

remove_arch_cache() {
    printf "\n${red}Removing all the unnecessary stuff...\n"
    yay -Scc
    printf "\nDone${normal}"
}

upgrade_debian_system() {
    printf "\nUpgrading Debian system with Apt...\n"
    sudo apt-get update && sudo apt-get -y upgrade
    printf "...Done upgrading the system\n"
}

check_outdated_rubygems() {
    if which gem 2>&1 >/dev/null; then
        printf "\n${purple}Update ruby gems${normal}\n"
        gem update
    else
        printf "${red}Ruby gems is not installed on your system${normal}\n"
    fi
}

check_outdated_npm_packages() {
    if which npm 2>&1 >/dev/null; then
        printf "\nCheck outdated global npm installed packages\n"
        npm outdated -g
    else
           printf "${red}NPM is not installed on your system${normal}\n"
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
