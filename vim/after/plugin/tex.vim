
au Filetype tex nnoremap <leader>p :w \| :call CompileLatex()<CR>
au Filetype plaintex nnoremap <leader>p :w \| :call CompileLatex()<CR>

au Filetype plaintex setlocal ft=tex
" Autoformat paragraph
au BufWrite *.tex silent! call FormatParagraph()

au FileType tex map <leader>ii
    \ i% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    \%%%%%%%%%%<CR>%<space>
