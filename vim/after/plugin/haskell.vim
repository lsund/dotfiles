"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Haskell Import

au FileType haskell map <silent> ,hil :call HaskellImport('list')<CR>
au FileType haskell map <silent> ,hip :call HaskellImport('protolude')<CR>
au FileType haskell map <silent> ,his :call HaskellImport('set')<CR>
au FileType haskell map <silent> ,hiv :call HaskellImport('vector')<CR>
au FileType haskell map <silent> ,him :call HaskellImport('map')<CR>
au FileType haskell map <silent> ,hiM :call HaskellImport('matrix')<CR>

au FileType haskell map <leader>p :call StyleHaskell()<CR>
au FileType haskell map <leader>ii i-------------------------------------------------------------------------------<CR><space>

" See https://github.com/neovimhaskell/haskell-vim for more details
let g:haskell_indent_disable = 1

tnoremap <ESC> <C-\><C-n><C-w><C-p>

tnoremap <A-h> <C-\><C-n><C-w>h
tnoremap <A-j> <C-\><C-n><C-w>j
tnoremap <A-k> <C-\><C-n><C-w>k
tnoremap <A-l> <C-\><C-n><C-w>l
" nnoremap <A-h> <C-w>h
" nnoremap <A-j> <C-w>j
" nnoremap <A-k> <C-w>k
" nnoremap <A-l> <C-w>l
