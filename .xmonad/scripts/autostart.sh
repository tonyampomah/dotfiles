#!/bin/bash
function run {
  if ! pgrep $1 ;
  then
    $@&
  fi
}

feh --bg-fill /home/tonyampomah/Nextcloud/Photos/Luka-photoshoot/BabyLuka25Jan25-1.jpg &
############## Keyboard Tweaks ##################
# Load my custom keyboard binding
# xmodmap ~/.config/.Xmodmap &


# Cursor active at boot
xsetroot -cursor_name left_ptr &

############## Trackpad ########################
xinput --set-prop "DLL096D:01 06CB:CDE6 Touchpad" "libinput Tapping Enabled" 1 &
xinput --set-prop "DLL096D:01 06CB:CDE6 Touchpad" "libinput Natural Scrolling Enabled" 1 &
# xinput --set-prop "DLL096D:01 06CB:CDE6 Touchpad" "libinput Accel Speed" 1 &


# Increase keyboard key repeat
xset r rate 300 80 &
## keyboard tweaks

# laptop_keyboard_id=$(xinput list --id-only 'AT Translated Set 2 keyboard')
# setxkbmap -device "$laptop_keyboard_id" -option ctrl:nocaps,altwin:swap_lalt_lwin,altwin:swap_ralt_rwin &

external_keyboard_id=$(xinput list --id-only 'keyboard:Keychron  Keychron Link  Keyboard')
setxkbmap -device "$external_keyboard_id" -layout gb -option -variant mac ctrl:nocaps &

# make short-pressed Ctrl behave like Escape:
xcape -e 'Control_L=Escape' &

(sleep 2; run $HOME/.config/polybar/launch.sh) &

################ Start Utility Apps At Boot Time ##########
run /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1
run dunst
run picom
# run variety
run nm-applet
################ Start Apps At Boot Time ###################
run emacs
run nextcloud
run redshift
