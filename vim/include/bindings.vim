
" Navigation
" =============================================================================

nmap <S-j> M4jzz''4j
nmap <S-k> M4kzz''4k

nnoremap <leader>h <C-w>h
nnoremap <leader>l <C-w>l
nnoremap j gj
nnoremap k gk
nnoremap <C-j> 0i<CR><ESC>
nnoremap <C-k> 0i<BS><ESC>


inoremap <C-Backspace> <Backspace>
inoremap <Backspace> <Nop>

map <C-w>s <C-w>l

nmap <leader>1 <Plug>AirlineSelectTab1
nmap <leader>2 <Plug>AirlineSelectTab2
nmap <leader>3 <Plug>AirlineSelectTab3
nmap <leader>4 <Plug>AirlineSelectTab4
nmap <leader>5 <Plug>AirlineSelectTab5
nmap <leader>6 <Plug>AirlineSelectTab6
nmap <leader>7 <Plug>AirlineSelectTab7
nmap <leader>8 <Plug>AirlineSelectTab8
nmap <leader>9 <Plug>AirlineSelectTab9

" Buffer management
" =============================================================================

cmap w!! w !sudo tee > /dev/null %
nmap <leader>y :bnext<CR>
nmap <leader><S-y> :bprevious<CR>
nmap <leader>' :bp <BAR> bd #<CR>
nmap <leader>nn :NERDTree<CR>
nmap <leader>, :w<CR>
nmap <leader>< :wall<CR>
nmap <leader>o :args 
nmap <leader>e <C-p>
nmap <leader>v :e ~/.vimrc<CR>
nmap <leader>" :BufOnly<CR>
" nmap <leader>r :Rename
nmap <leader>bl :e #<CR>

" Splits
" =============================================================================

map <leader>\| :vsplit<CR>
map <leader>- :split<CR>


" Plugins
" =============================================================================

map <leader>c q:
" easymotion
map <leader>mt <Plug>(easymotion-j)
map <leader>mn <Plug>(easymotion-k)

" syntastic
map <leader>st :SyntasticToggleMode<CR>
nnoremap <leader>ch :SyntasticCheck<CR>
nnoremap <leader>co :lopen<CR>

" tabularize
vmap a= :Tabularize /=<CR>
vmap a=> :Tabularize /=><CR>
vmap a+ :Tabularize /+<CR>
vmap a:: :Tabularize /::<CR>
vmap a: :Tabularize /:<CR>
vmap a- :Tabularize /-><CR>
vmap a\| :Tabularize /\|<CR>
vmap a<Space> :Tabularize /\ <CR>

" vim-easy-align
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

" vim-fugitive
map <leader>gs :Gstatus<CR>
map <leader>gc :Gcommit<CR>
map <leader>gp :Gpush<CR>
map <leader>gl :Git log<CR>
map <leader>gd :Gdiff master<CR>
map <leader>gaa :Git add -A<CR>

" ag
nnoremap <leader>a :Ack<Space>

" tagbar
map <leader>t :TagbarToggle<CR>

" Other mappings
" =============================================================================

imap <C-g> <Esc>

vmap <leader>icn :I<CR>
vmap <leader>ica :IA<CR>

noremap Z! :q!<CR>

noremap Y y$
inoremap <C-e> <C-o>$
inoremap <C-a> <C-o>0
inoremap <C-b> <C-o>h
inoremap <C-f> <C-o>l

nnoremap <Leader>rc :%s/\<<C-r><C-w>\>/
vnoremap <Leader>cc y:%s/<C-r>"/<C-r>"

nnoremap <leader>p :w \| :call CompileLatex()<CR>

nmap <silent> <leader>ss :call Toggle('spell')<CR>
nmap <silent> <leader>sp :call Toggle('paste')<CR>
nmap <silent> <leader>sl :call Toggle('list')<CR>

cnoremap <C-B> <Left>
cnoremap <C-F> <Right>

nmap <silent> --h "=HaskellModuleHeader()<CR>:0put =<CR>
nmap <silent> --s "=HaskellModuleSection()<CR>gp

noremap <leader>cr :call clearmatches()<CR>
nnoremap <leader>nh :nohlsearch<CR>

autocmd Filetype vim map <leader>ii 
    \ i"  --------------------------------------------------------------------
    \---------<CR>
