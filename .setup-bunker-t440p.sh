#!/bin/bash

# TODO: How to deal with multiples hostnames instead of static one
if [ `hostname` = "bunker-T440p" ]; then
    # Working at bunker
    sudo ip link set dev enp0s25 up
    # flush interface
    # ip addr flush dev enp0s25
    sudo ip address add 192.168.1.211/24 dev enp0s25
    sudo ip route add default via 192.168.1.1
    xrandr --output eDP1 --off --output HDMI1 --auto
else
    # Working at home
    xrandr --output eDP1 --auto
fi
