" javascript workflow:
" Install prettier, eslint, ale
" ale_fix_on_save will automatically indent and eslint warnings will be displayed. If it
" 'doesn't work', check that the filetype is actually javascript. E.g. vue and react files are
" dispatched differently.

let g:ale_fix_on_save = 1

au FileType javascript map <leader>ii i///////////////////////////////////////////////////////////////////////////////<CR><space>

au FileType sjx setlocal commentstring=\{/*\ %s\ */\}

set tabstop=2
set softtabstop=2
set shiftwidth=2
