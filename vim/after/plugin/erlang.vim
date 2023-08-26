
augroup FormatAutogroup erlanggroup
  autocmd!
  autocmd BufWritePost *.erl FormatWrite
augroup END

au BufRead rebar.* :set ft=erlang
" au FileType erlang setlocal indentexpr=

au FileType erlang map <leader>ii i%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%<CR><space>

