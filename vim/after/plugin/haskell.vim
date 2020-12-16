"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Haskell Import

au FileType haskell map <silent> ,hil :call HaskellImport('list')<CR>
au FileType haskell map <silent> ,hip :call HaskellImport('protolude')<CR>
au FileType haskell map <silent> ,his :call HaskellImport('set')<CR>
au FileType haskell map <silent> ,hiv :call HaskellImport('vector')<CR>
au FileType haskell map <silent> ,him :call HaskellImport('map')<CR>
au FileType haskell map <silent> ,hiM :call HaskellImport('matrix')<CR>

au FileType haskell map <leader>p :call StyleHaskell()<CR>
au FileType haskell map <leader>ii i-------------------------------------------------------------------------------<CR><C-w><C-w>--<space>

tnoremap <ESC> <C-\><C-n><C-w><C-p>

tnoremap <A-h> <C-\><C-n><C-w>h
tnoremap <A-j> <C-\><C-n><C-w>j
tnoremap <A-k> <C-\><C-n><C-w>k
tnoremap <A-l> <C-\><C-n><C-w>l

nnoremap <leader>bd :Hindent<CR>

let g:hindent_on_save = 0

" stack install hlint hindent apply-refact
