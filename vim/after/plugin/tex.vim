
au Filetype tex nnoremap <leader>rr :w \| :call CompileLatex()<CR>
au Filetype plaintex nnoremap <leader>rr :w \| :call CompileLatex()<CR>

let b:ale_linters = {'tex': [''], 'plaintex': ['']}

autocmd FileType plaintex :let g:format_paragraph=0
autocmd FileType tex :let g:format_paragraph=0
" autocmd FileType plaintex :let g:format_paragraph=1
" autocmd FileType tex :let g:format_paragraph=1

autocmd FileType plaintex :let g:rtd='report.tex'
autocmd FileType tex :let g:rtd='report.tex'

au Filetype plaintex setlocal ft=tex
" Autoformat paragraph
au BufWrite *.tex silent! call FormatParagraph()

au FileType tex map <leader>ii
    \ i% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    \%%%%%%%%%%<CR>%<space>

" Do not indent anything
set autoindent&
set cindent&
set smartindent&
set indentexpr&
