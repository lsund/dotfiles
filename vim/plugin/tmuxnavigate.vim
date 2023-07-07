
let g:tmux_navigator_no_mappings = 1

" Use alt keys to navigate buffers
nnoremap <silent> <A-h> :TmuxNavigateLeft<cr>
nnoremap <silent> <A-j> :TmuxNavigateDown<cr>
nnoremap <silent> <A-k> :TmuxNavigateUp<cr>
nnoremap <silent> <A-l> :TmuxNavigateRight<cr>
nnoremap <silent> <A-y> :TmuxNavigatePrevious<cr>
inoremap <A-h> <C-o>:TmuxNavigateLeft<cr>
inoremap <A-j> <C-o>:TmuxNavigateDown<cr>
inoremap <A-k> <C-o>:TmuxNavigateUp<cr>
inoremap <A-l> <C-o>:TmuxNavigateRight<cr>
inoremap <A-y> <C-o>:TmuxNavigatePrevious<cr>
