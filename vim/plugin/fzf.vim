" fzf
map <leader>e :FZF<CR>

autocmd! FileTYpe fzf tnoremap <buffer> <esc> <c-c>

let g:fzf_layout = {'window': { 'width': 1.0, 'height': 0.7, 'relative': v:true, 'yoffset': 1.0 }}
let g:fzf_history_dir = '~/.fzf/history'

let g:fzf_preview_window = []

