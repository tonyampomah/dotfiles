#!/bin/bash
function run {
  if ! pgrep $1 ;
  then
    $@&
  fi
}

feh --bg-scale /home/tonyampomah/Nextcloud/Photos/Wallpapers/general/0001.jpg &
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
# make short-pressed Ctrl behave like Escape:
xcape -e 'Control_L=Escape' &
picom --config $HOME/.config/qtile/scripts/picom.conf &
# run sxhkd -c ~/.config/sxhkd/sxhkdrc &

################ Start Utility Apps At Boot Time ##########
run variety &
run nm-applet &
#run pamac-tray &
run xfce4-power-manager &
numlockx on &
blueberry-tray &
picom --config $HOME/.config/qtile/scripts/picom.conf &
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
/usr/lib/xfce4/notifyd/xfce4-notifyd &
run nextcloud &
################ Start Apps At Boot Time ###################
