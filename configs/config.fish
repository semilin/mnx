if status is-interactive
    function fish_prompt
        echo -n (set_color blue)(date +%H:%M)(set_color cyan)(fish_git_prompt) (set_color magenta)(pwd)(set_color normal)' '
    end

    function fish_greeting
    end

    alias ls "exa --icons"
    alias ll "ls -l"
    alias l "ls"
    alias e $EDITOR
    alias first "head -1"
    alias last "tail -1"
    alias sortmod "ls -s modified"
    function gs
        guix shell $argv -- fish
    end
end
