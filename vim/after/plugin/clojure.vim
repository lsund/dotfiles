
"  Require current namespace
au FileType clojure nmap <leader>r  :Require<CR>
au FileType clojure nmap <leader>x  :Eval<CR>
"  Manual formating
"
au FileType clojure nmap <leader>gq <Plug>Csurround()vap==}vapgq
au FileType clojure nmap gq <Plug>Csurround()==
"
au FileType clojure map <leader>ii
    \ i;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    \;;;;;;;;<CR>;; "Title goes here

"  Breaking a clojure form over differint lines form
au FileType clojure nmap <Plug>BreakLine Elxi<CR><ESC>
\:call repeat#set("\<Plug>BreakLine", v:count)<CR>
au FileType clojure nmap <leader>b <Plug>BreakLine
"
au FileType clojure nmap <Plug>BreakLineAndIndent <Plug>BreakLine<leader>gq
\:call repeat#set("\<Plug>BreakLineAndIndent", v:count)<CR>
au FileType clojure nmap <leader>B <Plug>BreakLineAndIndent

au FileType clojure nmap gce :call CommentElement()<CR>
au FileType clojure nmap gcl :call CommentList()<CR>

"  Make sure CljFmt does not f**k up cursor position
" au FileType clojure autocmd BufWritePost * call ClojureWrite()
" nnoremap u :call ClojureUndo()<CR>
" au FileType clojure nmap <C-R> :call ClojureRedo()<CR>

" let g:saved_cursor_pos = getpos('.')

" function! ClojureWrite()
"     silent! undojoin | :Cljfmt
"     let g:saved_cursor_pos = getpos('.')
" endfunction

" function! ClojureRedo()
"     redo
"     call setpos('.', g:saved_cursor_pos)
" endfunction

function! CommentElement()
    normal B
    let l:symb = matchstr(getline('.'), '\%' . col('.') . 'c.')
    if l:symb == '#'
        normal xx
    else
        normal i#_
    endif
endfunction

function! CommentList()
    normal (F#
    let l:symb = matchstr(getline('.'), '\%' . col('.') . 'c.')
    if l:symb == '#'
        normal xx
    else
        normal i#_
    endif
endfunction
