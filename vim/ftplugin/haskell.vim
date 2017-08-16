map <leader>ch :GhcModCheckAndLintAsync<CR>

map <silent> ,ht :GhcModType<CR>
map <silent> ,hl :call ApplyOneSuggestion()<CR>
map <silent> ,hL :call ApplyAllSuggestion()<CR>
map <silent> ,hh :Hoogle<CR>
map ,hH :Hoogle 
map <silent> ,hc :HoogleClose<CR>
map <silent> ,hil :call HaskellImport('list')<CR>
map <silent> ,hip :call HaskellImport('protolude')<CR>
map <silent> ,his :call HaskellImport('set')<CR>
map <silent> ,hiv :call HaskellImport('vector')<CR>
map <silent> ,him :call HaskellImport('map')<CR>
map <silent> ,hiM :call HaskellImport('matrix')<CR>

nnoremap <leader>nh :nohlsearch<CR>:GhcModTypeClear<CR>

map <leader>ii 
    \ i--------------------------------------------------------------------
    \--------<CR> 

