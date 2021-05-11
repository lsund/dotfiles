" fzf
map <leader>e :FZF<CR>

autocmd! FileTYpe fzf tnoremap <buffer> <esc> <c-c>

let g:fzf_layout = {'window': { 'width': 1.0, 'height': 0.7, 'relative': v:true, 'yoffset': 1.0 }}
