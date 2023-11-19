# Prevent ZSH completion warnings
ZSH_DISABLE_COMPFIX=true

# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load --- if set to "random", it will
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git docker aws kubectl laravel ansible wp-cli vagrant terraform)

source $ZSH/oh-my-zsh.sh

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
