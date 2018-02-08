
source ~/Documents/dotfiles/vim/plugin/plugin.vim
source ~/Documents/dotfiles/vim/include/bindings.vim
source ~/Documents/dotfiles/vim/include/functions.vim
source ~/Documents/dotfiles/vim/include/settings.vim

let g:format_paragraph = 0
autocmd bufread,bufnewfile *.elm set ft=elm
autocmd bufread,bufnewfile *.pl set ft=prolog
au BufRead /tmp/mutt-* set tw=72
au BufRead /tmp/mutt-* call clearmatches()

