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
autocmd BufWritePost * call ClojureWrite()
" nnoremap u :call ClojureUndo()<CR>
nmap <C-R> :call ClojureRedo()<CR>

let g:saved_cursor_pos = getpos('.')

function ClojureWrite()
    silent! undojoin | :Cljfmt
    let g:saved_cursor_pos = getpos('.')
endfunction

function ClojureRedo()
    redo
    call setpos('.', g:saved_cursor_pos)
endfunction
