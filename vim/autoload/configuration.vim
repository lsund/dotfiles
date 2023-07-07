"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Configuration

let g:python3_host_prog = '/usr/bin/python3'

let g:rtd = "unset"

let g:delete_trailing_whitespace = 1

let g:format_paragraph = 0

let g:user_emmet_leader_key='<Tab>'

let g:user_emmet_settings = {
  \  'javascript.jsx' : {
    \      'extends' : 'jsx',
    \  },
  \}

let g:deoplete#enable_at_startup = 1

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Ale (Syntax)

let g:ale_linters = {
        \ 'haskell': [''],
        \ 'java': [],
        \ 'python': ['flake8'],
        \ 'erlang': [],
        \ }

let g:ale_fix_on_save = 0
let g:ale_sign_error = '●'
let g:ale_sign_warning = '.'
let g:ale_lint_on_enter = 0

let g:ale_fixers = {
 \ 'javascript': ['prettier'],
 \ 'haskell': [],
 \ 'javascript.jsx': ['prettier'],
 \ 'jsx': ['prettier'],
 \ 'typescript': ['prettier'],
 \ 'typescript.tsx': ['prettier'],
 \ 'json': ['prettier'],
 \ 'typescriptreact': ['prettier'],
 \ 'vue': ['prettier'],
 \ }

" Prettify JSON
command JSONprettify %!python -m json.tool

