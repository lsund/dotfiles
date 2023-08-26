"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Settings

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

" Search and matching =========================================================

set re=0
set incsearch
set hlsearch
set wildignore+=*\\tmp\\*,*.swp,*.swo,*.zip,.git,.cabal-sandbox
set wildmode=longest,list,full
set wildmenu
set completeopt+=longest

" Folding =====================================================================

set foldenable

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Whitespace clearing

set shortmess-=S
highlight OverLength ctermbg=red ctermfg=white guibg=#592929
match OverLength /\%81v.\+/

highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/

au BufWrite * if g:delete_trailing_whitespace == 1 |  execute "normal ma" | silent! %s/\s\+$// | execute "normal `a" | endif

" Only when in normal mode
autocmd! InsertEnter * call clearmatches()
autocmd! InsertLeave * match ExtraWhitespace /\s\+$/

set termguicolors

hi DiagnosticError guifg=#ff9933
hi DiagnosticWarn guifg=#ff99ff
hi DiagnosticInfo guifg=#ffffff
hi DiagnosticHint guifg=#66ff66
