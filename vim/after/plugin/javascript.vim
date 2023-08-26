au FileType javascript map <leader>ii i///////////////////////////////////////////////////////////////////////////////<CR><space>

au FileType sjx setlocal commentstring=\{/*\ %s\ */\}

augroup FormatAutogroup javascriptgroup
  autocmd!
  autocmd BufWritePost *.js FormatWrite
augroup END

