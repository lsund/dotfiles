au BufRead rebar.* :set ft=erlang
" au FileType erlang setlocal indentexpr=

au FileType erlang map <leader>ii i%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%<CR><space>

augroup FormatAutogroup
  autocmd!
  autocmd BufWritePost *.erl FormatWrite
augroup END
