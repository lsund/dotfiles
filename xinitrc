#!/bin/bash

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

export LC_ALL=en_US.UTF-8

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

# use .Xresources
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

# cursor
xsetroot -cursor_name left_ptr

# DVORAK and compose key
setxkbmap dvorak -option compose:caps

# color
xsetroot -solid grey

#Screen lock
# xautolock -locker slimlock -time 20 &
#'disable' key repeat
# xset r rate 5000

# Unset beep
xset b off

# xscreensaver &

xmonad --recompile

redshift -l 51.2:6.7 -t 5500:3400 -g 0.8 -m randr &

xinput --set-prop "DELL08AF:00 06CB:76AF Touchpad" "libinput Tapping Enabled" 1

source ~/.bash_profile

### XMONAD
exec xmonad
