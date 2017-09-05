nmap <leader>r  :Require<CR>
nmap <leader>x  :Eval<CR>
nmap <leader>gq <Plug>Csurround()vap==}vapgq
nmap gq <Plug>Csurround()==

map <leader>ii
    \ i;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    \;;;;;;;;<CR>;; 

nmap <Plug>BreakLine Elxi<CR><ESC>
\:call repeat#set("\<Plug>BreakLine", v:count)<CR>
nmap <leader>b <Plug>BreakLine

nmap <Plug>BreakLineAndIndent <Plug>BreakLine<leader>gq
\:call repeat#set("\<Plug>BreakLineAndIndent", v:count)<CR>
nmap <leader>B <Plug>BreakLineAndIndent

autocmd BufWritePost * let saved_pos = getpos('.')
nmap u u:call setpos('.', saved_pos)<CR>
nmap <C-R> <C-R>:call setpos('.', saved_pos)<CR>
