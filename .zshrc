# Prevent ZSH completion warnings
ZSH_DISABLE_COMPFIX=true

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="robbyrussell"

plugins=(git docker aws kubectl laravel ansible wp-cli vagrant terraform)

# source $ZSH/oh-my-zsh.sh

# COMPOSER LINUX
export PATH="$HOME/.config/composer/vendor/bin:$PATH"

# COMPOSER MAC
export PATH="$HOME/.composer/vendor/bin:$PATH"

# MPD
export MPD_HOST="localhost"
export MPD_PORT="6600"

# EMACS
export VISUAL="emacsclient -c"
export EDITOR=emacsclient

# JAVA
export _JAVA_AWT_WM_NONREPARENTING=1
export AWT_TOOLKIT=MToolkit

# NODE
PATH="$HOME/.node_modules/bin:$PATH"
export npm_config_prefix=~/.node_modules

# ANDROID STUDIO
export _JAVA_AWT_WM_NONREPARENTING=1
export STUDIO_JDK=/usr/lib/jvm/java-14-openjdk

if [ -d "$HOME/.bin" ] ;
  then PATH="$HOME/.bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ] ;
  then PATH="$HOME/.local/bin:$PATH"
fi

if [ -e $HOME/.aliases ];
  then source $HOME/.aliases
fi

source ~/.bin/key-bindings.zsh

export PATH="$HOME/Library/Python/3.9/bin:$PATH"

export PATH=/opt/homebrew/bin:$PATH
export PATH="/opt/homebrew/opt/node@18/bin:$PATH"
