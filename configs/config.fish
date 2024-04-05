if status is-interactive
    function fish_prompt
        echo -n (set_color blue)(date +%H:%M)(set_color cyan)(fish_git_prompt) (set_color magenta)(pwd)(set_color normal)' '
    end

    alias ls "exa --icons"
    alias ll "ls -l"
    alias l "ls"
    alias e $EDITOR
    function gs
        guix shell $argv -- fish
    end
end
