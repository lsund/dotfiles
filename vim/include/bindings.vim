
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Navigation

" Command line bindings
cnoremap <C-G> <C-C>
cnoremap <C-A> <Home>
cnoremap <C-B> <Left>
cnoremap <C-F> <Right>

" Insert mode command line bindings
inoremap <C-e> <C-o>$
inoremap <C-a> <C-o>0
inoremap <C-b> <C-o>h
inoremap <C-f> <C-o>l

vnoremap <C-G> <C-C>

" Make Y behave as D, C etc
noremap Y y$

" Fixed scroll
nmap <S-j> M4jzz''4j
nmap <S-k> M4kzz''4k

nnoremap j gj
nnoremap k gk

" Push line up/down
nnoremap <C-j> 0i<CR><ESC>
nnoremap <C-k> 0i<BS><ESC>

" Disable backspace
inoremap <C-Backspace> <Backspace>
inoremap <Backspace> <Nop>

" Use Control g as escape
inoremap <C-g> <ESC>
nnoremap <C-g> <ESC>

" Toggle spell, paste or list
nmap <silent> <leader>ss :call Toggle('spell')<CR>
nmap <silent> <leader>sp :call Toggle('paste')<CR>
nmap <silent> <leader>sl :call Toggle('list')<CR>
nmap <silent> <leader>sw :call Toggle('whitespace')<CR>
nmap <silent> <leader>sf :call Toggle('paragraph')<CR>

" Write root file as non-root
cmap w!! w !sudo tee > /dev/null %

" List buffers
nmap <leader>y :Buffers<CR>

" Go to Last buffer
nmap <leader><S-y> :b#<CR>

" Kill buffer
nmap <leader>' :bp <BAR> bd #<CR>
" Kill every buffer except this one
nmap <leader>" :BufOnly<CR>

" Remove whitespace at end of line on write.
" (Also remember position and jump back)

au BufWrite * if g:delete_trailing_witespace == 1 |  execute "normal ma" | silent! %s/\s\+$// | execute "normal `a" | endif

nnoremap <f4> :let g:myhighlight = !get(g:, 'myhightlight', 1)<cr>

" Write file
nmap <leader>, :w<CR>
nmap <leader>< :wall<CR>

" Open file
nmap <leader>o :args<space>
nmap <leader>e <C-p>
nmap <leader>j :Files<space>

" Edit history
map <leader>c q:

" Exit
noremap Z! :q!<CR>

"Split
map <leader>\| :vsplit<CR>
map <leader>- :split<CR>

" Clear highlight
noremap <leader>cr :call clearmatches()<CR>
nnoremap <leader>nh :nohlsearch<CR>

" Insert increasing counters
vmap <leader>icn :I<CR>
vmap <leader>ica :IA<CR>

nnoremap <leader>aa :Rg<CR>

autocmd FileType qf nnoremap <buffer> O <Enter> :call DeleteEmptyBuffers()<CR>

autocmd FileType qf nnoremap <buffer> <leader>\| <C-w><Enter><C-w>L :call DeleteEmptyBuffers()<CR>

nmap gbl :Gblame<CR>
