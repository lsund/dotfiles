
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%#warningmsg#
set statusline+=%*
let g:syntastic_always_populate_loc_list=1
let g:syntastic_auto_loc_list=1
let g:syntastic_check_on_open=1
let g:syntastic_check_on_wq=0
let g:syntastic_rst_checkers=['']
let g:syntastic_haskell_checkers=['hlint']
let g:syntastic_scala_checkers=['']
" let g:syntastic_clojure_checkers=['eastwood']
let g:syntastic_c_checkers=['']
let g:syntastic_mode_map =
            \{
            \'mode': 'active',
            \'active_filetypes': [],
            \'passive_filetypes': []
            \}

" syntastic
map <leader>st :SyntasticToggleMode<CR>
nnoremap <leader>ch :SyntasticCheck<CR>
nnoremap <leader>co :lopen<CR>

