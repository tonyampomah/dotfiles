#!/bin/bash

INTERNAL_DISPLAY="eDP-1"
EXTERNAL_DISPLAY="eDP-1"  # Adjust to match your external monitor

if [ "$1" = "close" ]; then
  # Lid is closed: disable internal display
  hyprctl dispatch dpms off $INTERNAL_DISPLAY
  hyprctl keyword monitor $INTERNAL_DISPLAY,disable
elif [ "$1" = "open" ]; then
  # Lid is opened: re-enable internal display
  hyprctl keyword monitor $INTERNAL_DISPLAY,preferred,auto,1
  hyprctl dispatch dpms on $INTERNAL_DISPLAY
fi
