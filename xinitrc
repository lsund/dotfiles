#!/bin/bash

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

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

# compose key
if [ "$HOSTNAME" = dennis ]; then
    setxkbmap dvorak -option compose:caps
else
    setxkbmap -option compose:caps
fi

# color
xsetroot -solid grey

#Screen lock
# xautolock -locker slimlock -time 20 &

source ~/.bash_profile

### XMONAD
exec xmonad

### AWESOME
#exec awesome

### BSPWM
#sxhkd &
#panel &
#compton &
#feh --bg-scale ~/.wallpaper.jpg &
#exec bspwm

### TWM
#xsetroot -solid grey
#twm &
#conky &
#exec xterm -fg white -bg black -geometry 187x48+94+0 -name login -fa 'SourceCodePro' -fs 10

