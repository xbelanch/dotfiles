# Gruber ZSH Theme

# Color shortcuts
RED=$fg[red]
YELLOW=$fg[yellow]
GREEN=$fg[green]
WHITE=$fg[white]
BLUE=$fg[blue]
RED_BOLD=$fg_bold[red]
YELLOW_BOLD=$fg_bold[yellow]
GREEN_BOLD=$fg_bold[green]
WHITE_BOLD=$fg_bold[white]
BLUE_BOLD=$fg_bold[blue]
RESET_COLOR=$reset_color

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg_bold[white]%}(%{$fg_bold[green]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX=""
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[red]%}✘%{$fg[green]%}%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_CLEAN="%{$reset_color%}:"

ZSH_THEME_GIT_PROMPT_SHA_BEFORE="%{$WHITE%}%{$YELLOW_BOLD%}"
ZSH_THEME_GIT_PROMPT_SHA_AFTER="%{$WHITE%}) "

#PROMPT='[$fg_bold[cyan]%T$reset_color]%n@%m:%~%(!.#.$)$(git_prompt_info) '
PROMPT='$(git_prompt_info)$(git_prompt_short_sha)$fg_bold[white]%n@%m:$fg_bold[blue]%~$reset_color%(!.#.$) '