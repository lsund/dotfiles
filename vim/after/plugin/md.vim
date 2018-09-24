
" Autoformat paragraph
au BufWrite *.md silent! call FormatParagraph()
autocmd InsertEnter,WinLeave * setlocal foldmethod=manual
