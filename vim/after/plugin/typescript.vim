let g:ale_sign_error = '●' " Less aggressive than the default '>>'
let g:ale_sign_warning = '.'
let g:ale_lint_on_enter = 0 " Less distracting when opening a new file

let g:ale_fix_on_save = 1

let g:ale_fixers = {
 \ 'javascript': ['prettier'],
 \ 'javascript.jsx': ['prettier'],
 \ 'jsx': ['prettier'],
 \ 'typescript': ['prettier'],
 \ 'typescript.tsx': ['prettier'],
 \ 'json': ['prettier'],
 \ }

au FileType typescript map <leader>ii i///////////////////////////////////////////////////////////////////////////////<CR><space>
au FileType typescript.tsx map <leader>ii i///////////////////////////////////////////////////////////////////////////////<CR><space>

set re=0
