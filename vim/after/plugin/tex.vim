
au Filetype tex nnoremap <leader>p :w \| :call CompileLatex()<CR>

" Autoformat paragraph
au BufWrite *.tex execute "normal mavapgq`a"
" Replace 2 spaces between two words with one on write
au BufWrite *.tex silent! :%s/\([^\ \\]\)  \([^\ \\]\)/\1 \2/
