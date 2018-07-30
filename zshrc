. $HOME/.aliases


# ###########################################################################
# Extend path


export PATH=$PATH:$HOME/.bin:/usr/local/bin
export PATH=$PATH:$HOME/Documents/scripts
export PATH=$PATH:$HOME/.local/bin
export PATH=$PATH:$HOME/.cabal/bin


# ###########################################################################
# Custom env variables

export MYBLOG=$HOME/Documents/tech/repos/myblog

export DOTFILES=$HOME/Documents/dotfiles
export SCRIPTS=$HOME/Documents/scripts

export LW=$HOME/Documents/tech/repos/lambdawerk
export WIKI=$HOME/Documents/wiki

export JAVA_HOME=/usr/lib/jvm/default

export ZSH=$HOME/.oh-my-zsh
export FZF_DEFAULT_COMMAND='fd --type f'

: ${SCRIPTS?"Warning: env SCRIPTS is not set. Everything will break down"}

# ###########################################################################
# Custom shell functions

# cd override
function cd() {
    builtin cd "$@" && ls;
}

# Start ssh-agent
if ! pgrep -u "$USER" ssh-agent > /dev/null; then
    ssh-agent > ~/.ssh-agent-thing
fi
if [[ "$SSH_AGENT_PID" == "" ]]; then
    eval "$(<~/.ssh-agent-thing)"
fi

# Check if identity not already added. In that case, add the identity
ssh-add -l > /dev/null
if [[ $? == "1" ]]; then
    echo "Adding id_rsa.pub..."
    ssh-add
    if [[ -e ~/.ssh/id_rsa_gmail ]]; then
        echo "Adding id_rsa_gmail..."
        ssh-add ~/.ssh/id_rsa_gmail
    fi
    echo "Done."
fi

# ############################################################################
# Fuzzy finding

[[ -e /usr/share/fzf/key-bindings.zsh ]] && . /usr/share/fzf/key-bindings.zsh
[[ -e /usr/share/fzf/completion.zsh ]]   && . /usr/share/fzf/completion.zsh

[[ -e $HOME/.fzf/shell/key-bindings.zsh ]] &&  . $HOME/.fzf/shell/key-bindings.zsh
[[ -e $HOME/.fzf/shell/completion.zsh ]] &&  . $HOME/.fzf/shell/completion.zsh

# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Case sensitive auto completion
# CASE_SENSITIVE="true"

# _ and - interchangable. Case sensitive must be off
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='nvim'
else
  export EDITOR='nvim'
fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
export SSH_KEY_PATH="~/.ssh/rsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

export NNN_USE_EDITOR=1

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

