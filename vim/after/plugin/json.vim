" autocmd BufWrite *.json execute 'normal ma' | execute '%!python -m json.tool' | execute 'normal `a'

" nnoremap <leader>bd :%!python -m json.tool<CR>
