let g:user_emmet_settings = {
  \  'javascript.jsx' : {
    \      'extends' : 'jsx',
    \  },
  \}

let g:ale_sign_error = '●'
let g:ale_sign_warning = '.'
let g:ale_lint_on_enter = 0

let g:ale_fix_on_save = 1

let g:ale_fixers = {
 \ 'javascript': ['prettier'],
 \ 'javascript.jsx': ['prettier'],
 \ 'jsx': ['prettier'],
 \ 'typescript': ['prettier'],
 \ 'typescript.tsx': ['prettier'],
 \ 'json': ['prettier'],
 \ }

au FileType javascript map <leader>ii i///////////////////////////////////////////////////////////////////////////////<CR><space>

au FileType sjx setlocal commentstring=\{/*\ %s\ */\}

set tabstop=2
set softtabstop=2
set shiftwidth=2
