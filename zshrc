if [ -n "$INSIDE_EMACS" ]; then
  chpwd() { print -P "\033AnSiTc %d" }
  print -P "\033AnSiTu %n"
  print -P "\033AnSiTc %d"
fi

HISTFILE=~/.histfile
HISTSIZE=3000
SAVEHIST=3000
setopt autocd beep nomatch
unsetopt notify
bindkey -e
zstyle :compinstall filename '/home/ess/.zshrc'

autoload -Uz compinit promptinit
compinit
promptinit

prompt walters

setopt completealiases

export PATH=$PATH:/home/ess/.gem/ruby/1.9.1/bin:/home/ess/.gem/ruby/2.0.0/bin:/home/ess/bin
export ALTERNATE_EDITOR=""
export SUDO_EDITOR="emacsclient -t"
export EDITOR="emacsclient -t"

alias E="SUDO_EDITOR=\"emacsclient -t\" sudo -e"

eval "$(fasd --init auto)"
alias e='f -e emacsclient -t'
alias m='f -e mpv'
alias o='a -e xdg-open'

alias ls='ls --color=auto'
alias la='ls -a'
alias ll='ls -l'
alias lla='ls -la'

alias installed='cat /var/log/pacman.log | grep installed'

alias burncue='cdrdao write -n --eject --device /dev/sr0'
alias playcd='mplayer -cdrom-device /dev/sr0 cdda:// -cache 5000'

alias ec='emacsclient -n'

alias -g ND='*(/om[1])'
alias -g NF='*(.om[1])'

alias mountphone='sudo mtpfs -o allow_other /media/android'
alias umountphone='sudo umount /media/android'

alias espgaluda='sdlmame espgal'
alias teensy-ergo='teensy-loader-cli -mmcu=atmega32u4 -w -v'

timer() { for ((i=$*;i>=0;i--));do echo -ne "\r$(date -d"0+$i sec" +%H:%M:%S)";sleep 1;done }

upl-imgur() {
curl -# -F image=@"$1" -F "key=1913b4ac473c692372d108209958fd15" \
http://api.imgur.com/2/upload.xml | grep -Eo "<original>(.)*</original>" \
| grep -Eo "http://i.imgur.com/[^<]*"
}

monitor-multi() {
    xrandr --output LVDS-0 --off
    xrandr --output DP-2 --mode 1920x1080
    xrandr --output DP-1 --mode 1920x1080 --rotate left --left-of DP-2
    feh --bg-scale /home/ess/wallpapers/hedgehog.jpg
}

monitor-laptop() {
    xrandr --output DP-2 --off
    xrandr --output LVDS-0 --mode 1600x900
    xrandr --output DP-1 --off
    feh --bg-scale /home/ess/wallpapers/hedgehog.jpg
}

monitor-desktop() {
    xrandr --output LVDS-0 --off
    xrandr --output DP-2 --mode 1920x1080
    feh --bg-scale /home/ess/wallpapers/hedgehog.jpg
}
