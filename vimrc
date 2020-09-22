
source ~/Documents/dotfiles/vim/plugin/plugin.vim
source ~/Documents/dotfiles/vim/include/bindings.vim
source ~/Documents/dotfiles/vim/include/functions.vim
source ~/Documents/dotfiles/vim/include/settings.vim

let g:format_paragraph = 0
autocmd bufread,bufnewfile *.elm set ft=elm
autocmd bufread,bufnewfile *.pl set ft=prolog
au BufRead /tmp/mutt-* set tw=72
au BufRead /tmp/mutt-* call clearmatches()

let g:user_emmet_leader_key='<Tab>'

let g:user_emmet_settings = {
  \  'javascript.jsx' : {
    \      'extends' : 'jsx',
    \  },
  \}

let g:ale_linters = {
        \ 'haskell': ['hlint', 'stack_build'],
        \ }

let g:ale_fix_on_save = 1
let g:deoplete#enable_at_startup = 1

let g:LanguageClient_serverCommands = {
    \ 'rust': ['~/.cargo/bin/rustup', 'run', 'stable', 'rls'],
    \ 'reason': ['/home/lsund/.bin/reason-language-server']
    \ }

