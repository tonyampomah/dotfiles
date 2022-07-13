#
# ~/.bashrc
#
export OSH=/home/kwamedat/.oh-my-bash

if [ -d "$HOME/.bin" ] ;
  then PATH="$HOME/.bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ] ;
  then PATH="$HOME/.local/bin:$PATH"
fi

if [ -e $HOME/.bash_aliases ];
  then source $HOME/.bash_aliases
fi

# Node
# PATH="$HOME/.node_modules/bin:$PATH"
# export npm_config_prefix=~/.node_modules
eval "$(fnm env --use-on-cd)"

# COMPOSER
export PATH="$HOME/.config/composer/vendor/bin:$PATH"

export MPD_HOST="localhost"
export MPD_PORT="6600"

export VISUAL="emacsclient -c"
export EDITOR=emacsclient

# JAVA
export _JAVA_AWT_WM_NONREPARENTING=1
export AWT_TOOLKIT=MToolkit

# For Android Studio
export _JAVA_AWT_WM_NONREPARENTING=1
export STUDIO_JDK=/usr/lib/jvm/java-14-openjdk

# For Gradle
export JAVA_HOME=/usr/lib/jvm/java-8-openjdk

export MONITOR_COUNT=$(xrandr -q | grep ' connected' | wc -l)
if [ "$DESKTOP_SESSION" == "xmonad" ] && [ "$MONITOR_COUNT" == 1 ] ; then
  ## GDK
  export GDK_SCALE=2
  export GDK_DPI_SCALE=0.5
  export QT_AUTO_SCREEN_SET_FACTOR=0
  export QT_SCALE_FACTOR=2
  export QT_FONT_DPI=96
fi

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export HISTCONTROL=ignoreboth:erasedups

OSH_THEME="bakke"

completions=(
  git
  composer
  ssh
  docker
  vagrant
  make
  vagrant
  docker-compose
)

aliases=(
  general
)

plugins=(
  git
  bashmarks
)

source $OSH/oh-my-bash.sh


PS1='[\u@\h \W]\$ '


#ignore upper and lowercase when TAB completion
bind "set completion-ignore-case on"


# # ex = EXtractor for all kinds of archives
# # usage: ex <file>
ex ()
{
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1   ;;
      *.tar.gz)    tar xzf $1   ;;
      *.bz2)       bunzip2 $1   ;;
      *.rar)       unrar x $1   ;;
      *.gz)        gunzip $1    ;;
      *.tar)       tar xf $1    ;;
      *.tbz2)      tar xjf $1   ;;
      *.tgz)       tar xzf $1   ;;
      *.zip)       unzip $1     ;;
      *.Z)         uncompress $1;;
      *.7z)        7z x $1      ;;
      *.deb)       ar x $1      ;;
      *.tar.xz)    tar xf $1    ;;
      *.tar.zst)   unzstd $1    ;;
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

source /usr/share/fzf/key-bindings.bash
source /usr/share/fzf/completion.bash
# neofetch
