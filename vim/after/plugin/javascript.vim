let g:user_emmet_settings = {
  \  'javascript.jsx' : {
    \      'extends' : 'jsx',
    \  },
  \}

let g:ale_sign_error = '●' " Less aggressive than the default '>>'
let g:ale_sign_warning = '.'
let g:ale_lint_on_enter = 0 " Less distracting when opening a new file

let g:ale_fix_on_save = 1

let g:ale_fixers = {
 \ 'javascript': ['prettier'],
 \ 'javascript.jsx': ['prettier'],
 \ 'jsx': ['prettier']
 \ }

au FileType javascript map <leader>ii i///////////////////////////////////////////////////////////////////////////////<CR><space>
