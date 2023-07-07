
au FileType python map <leader>ii
    \ i# ###################################################################
    \###########<CR># <ESC>o<ESC>kA

nmap <leader>ff :Format<CR>


augroup FormatAutogroup pythongroup
  autocmd!
  autocmd BufWritePost *.py FormatWrite
augroup END
