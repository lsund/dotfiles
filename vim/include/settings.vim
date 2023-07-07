" Globa Settings ====================================================

let g:delete_trailing_whitespace = 1

let g:format_paragraph = 0

let g:ycm_filetype_specific_completion_to_disable = {
      \ 'java': 1
      \}

let g:user_emmet_leader_key='<Tab>'

let g:user_emmet_settings = {
  \  'javascript.jsx' : {
    \      'extends' : 'jsx',
    \  },
  \}

let g:deoplete#enable_at_startup = 1

let g:ale_linters = {
        \ 'haskell': ['hlint'],
        \ 'java': [],
        \ 'python': ['flake8'],
        \ 'erlang': [],
        \ }

let g:ale_fix_on_save = 1
let g:ale_sign_error = '●'
let g:ale_sign_warning = '.'
let g:ale_lint_on_enter = 0

let g:LanguageClient_serverCommands = {
    \ 'rust': ['~/.cargo/bin/rustup', 'run', 'stable', 'rls'],
    \ 'reason': ['/home/lsund/.bin/reason-language-server'],
    \ 'java': ['/usr/bin/java-language-server']
    \ }

let g:LanguageClient_useVirtualText = 'No'


let g:ale_fixers = {
 \ 'javascript': ['prettier'],
 \ 'haskell': ['stylish-haskell'],
 \ 'javascript.jsx': ['prettier'],
 \ 'jsx': ['prettier'],
 \ 'typescript': ['prettier'],
 \ 'typescript.tsx': ['prettier'],
 \ 'json': ['prettier'],
 \ 'typescriptreact': ['prettier'],
 \ 'vue': ['prettier'],
 \ }


"  -----------------------------------------------------------------------------
"  General

filetype plugin indent on
set nocompatible
set nobackup
set noswapfile
set shell=bash
set history=1000
set hidden
" manpages inside vim
runtime! ftplugin/man.vim


" Indentation =================================================================

set tabstop=4
set softtabstop=4
set shiftwidth=4
set shiftround
set smartcase
set smarttab
set autoindent
set expandtab
set nocindent

" Formatting and Spelling ================================================================

set formatoptions+=tc
set linebreak
set spelllang=en_us
set encoding=utf-8
set fileencoding=utf-8
set visualbell t_vb=".
set tw=79
set wrap

" Visuals =====================================================================

syntax enable
set t_Co=256
set background=dark
colorscheme badwolf
set number
set list
set relativenumber
set cursorline
set lazyredraw
set showmatch
set showmode
set guifont=Nimbus\ Mono\ 12
set cmdheight=1
set previewheight=30
set listchars=tab:▸\ ,eol:¬
set ttyfast

set shortmess-=S
highlight OverLength ctermbg=red ctermfg=white guibg=#592929
match OverLength /\%81v.\+/

highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/

" Only when in normal mode
autocmd! InsertEnter * call clearmatches()
autocmd! InsertLeave * match ExtraWhitespace /\s\+$/

autocmd bufread,bufnewfile *.elm set ft=elm
autocmd bufread,bufnewfile *.pl set ft=prolog

" Search and matching =========================================================

set re=1
set incsearch
set hlsearch
set wildignore+=*\\tmp\\*,*.swp,*.swo,*.zip,.git,.cabal-sandbox
set wildmode=longest,list,full
set wildmenu
set completeopt+=longest

" Folding =====================================================================

set foldenable
