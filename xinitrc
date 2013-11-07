#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

setxkbmap -layout se -option ctrl:nocaps &
urxvtd -f -o &
python2 /home/ess/dropbox.py start &
synclient TapButton2=2 &
synclient TapButton3=3 &
xflux -l 58 -g 14 &
exec i3
