au BufRead,BufNewFile *.ex,*.exs set filetype=elixir
au BufRead,BufNewFile *.eex,*.heex,*.leex,*.sface,*.lexs set filetype=eelixir
au BufRead,BufNewFile mix.lock set filetype=elixir

augroup FormatAuGroupElixir
  autocmd!
  autocmd BufWritePost *.ex FormatWrite
augroup END

au FileType elixir map <leader>ii
    \ i# ###################################################################
    \###########<CR># <ESC>o<ESC>kA
