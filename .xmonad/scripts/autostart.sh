#!/bin/bash
function run {
  if ! pgrep $1 ;
  then
    $@&
  fi
}

feh --bg-scale /home/kwamedat/Nextcloud/Photos/Wallpapers/general/0001.jpg &
############## Keyboard Tweaks ##################
# Load my custom keyboard binding
# xmodmap ~/.config/.Xmodmap &


# Cursor active at boot
xsetroot -cursor_name left_ptr &

############## Trackpad ########################
xinput --set-prop "SYNA2393:00 06CB:7A13 Touchpad" "libinput Tapping Enabled" 1 &
xinput --set-prop "SYNA2393:00 06CB:7A13 Touchpad" "libinput Natural Scrolling Enabled" 1 &
xinput --set-prop "SYNA2393:00 06CB:7A13 Touchpad" "libinput Accel Speed" 1 &

# Increase keyboard key repeat
xset r rate 300 80 &
## make CapsLock behave like Ctrl:
setxkbmap -option ctrl:nocaps &
## make super key behave like macos
setxkbmap -option altwin:swap_lalt_lwin &
setxkbmap -option altwin:swap_ralt_rwin &
# make short-pressed Ctrl behave like Escape:
xcape -e 'Control_L=Escape' &


(sleep 2; run $HOME/.config/polybar/launch.sh) &

################ Start Utility Apps At Boot Time ##########
run /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1
run dunst
run picom --experimental-backends
# run xbindkeys -f ~/.xbindkeysrc
run variety
run caffeine -a
run nm-applet
################ Start Apps At Boot Time ###################
run /usr/bin/emacs --daemon
run solaar -w hide
run nextcloud
run redshift
