"  Require current namespace
nmap <leader>r  :Require<CR>
"  Evalueate buffer
nmap <leader>x  :Eval<CR>
"  Manual formating
"
nmap <leader>gq <Plug>Csurround()vap==}vapgq
nmap gq <Plug>Csurround()==
"
map <leader>ii
    \ i;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    \;;;;;;;;<CR>;; "Title goes here

"  Breaking a clojure form over differint lines form
nmap <Plug>BreakLine Elxi<CR><ESC>
\:call repeat#set("\<Plug>BreakLine", v:count)<CR>
nmap <leader>b <Plug>BreakLine
"
nmap <Plug>BreakLineAndIndent <Plug>BreakLine<leader>gq
\:call repeat#set("\<Plug>BreakLineAndIndent", v:count)<CR>
nmap <leader>B <Plug>BreakLineAndIndent
"
"
"  Make sure CljFmt does not f**k up cursor position
autocmd BufReadPre,FileReadPre * let saved_pos = getpos('.')
autocmd BufWritePost * let saved_pos = getpos('.')
nmap u u:call setpos('.', saved_pos)<CR>
nmap <C-R> <C-R>:call setpos('.', saved_pos)<CR>
