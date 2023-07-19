  filetype off

" Vim-plug
call plug#begin('~/.vim/plugged')
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Color, Appearance
Plug 'flazz/vim-colorschemes'
Plug 'ayu-theme/ayu-vim'
Plug 'lifepillar/vim-solarized8'
"
" Status line
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Rainbow-colored brackets
Plug 'kien/rainbow_parentheses.vim'
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Complete
"
" Autocomplete
Plug 'Valloric/YouCompleteMe'
"
" Syntax checker, linter
Plug 'dense-analysis/ale'

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Utility
"
" Fuzzy file finder
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
"
" Fuzzy keyword search
Plug 'mileszs/ack.vim'
"
" Directory overview
Plug 'scrooloose/nerdtree'
"
" Outline viewer :TagbarToggle
Plug 'majutsushi/tagbar'
"
" Move anywhere on the screen (<leader><leader>w)
Plug 'easymotion/vim-easymotion'
"
" Easier use of f and F hotkey
Plug 'unblevable/quick-scope'
"
" Navigate through vim and tmux seamlessly
Plug 'christoomey/vim-tmux-navigator'
"
" Automatic commenting
Plug 'tpope/vim-commentary'
"
" Easier character surround
Plug 'tpope/vim-surround'

" Easier navigation
Plug 'tpope/vim-unimpaired'
"
" Repeat a mapping eg surround.vim
Plug 'tpope/vim-repeat'
"
" Ascii tables
Plug 'dhruvasagar/vim-table-mode'
"
" Align lines according to symbol (<leader>a<symbol>)
Plug 'godlygeek/tabular'
"
" Git plugin
Plug 'tpope/vim-fugitive'
"
" Asyncronous execution, used by neovim-ghci
Plug 'neomake/neomake'

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Haskell

" Bindings for hlint automatic application
" try `to` or `ta`
Plug 'mpickering/hlint-refactor-vim'
"
"  Syntax highlightning for haskell
Plug 'neovimhaskell/haskell-vim'

Plug 'ndmitchell/ghcid', { 'rtp': 'plugins/nvim' }

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Clojure

" Live repl, dynamic expression evaluation
Plug 'tpope/vim-fireplace'

Plug 'tpope/vim-projectionist'

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Rust

" Error checking, formatting, integration
Plug 'rust-lang/rust.vim'

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Elm, Typescript, Purescript

Plug 'prettier/vim-prettier', { 'do': 'yarn install --frozen-lockfile --production' }

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Latex

" background compilation, completion, indentation, highlightning...
Plug 'LaTeX-Box-Team/LaTeX-Box'

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Markdown

Plug 'tpope/vim-markdown'

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" HTML, XML

" Html HTML
Plug 'mattn/emmet-vim'
Plug 'skywind3000/asyncrun.vim'

" XQuery
Plug 'jeroenp/vim-xquery-syntax'

" XML
Plug 'sukima/xmledit'

" Dhall
Plug 'vmchale/dhall-vim'

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Javascript

Plug 'pangloss/vim-javascript'

Plug 'ruanyl/vim-sort-imports'

" JSX jsx
" Plug 'MaxMEllon/vim-jsx-pretty'
Plug 'mxw/vim-jsx'

" Helpers
Plug 'Shougo/denite.nvim'

" Vue JS
Plug 'posva/vim-vue'

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Java

Plug 'uiiaoo/java-syntax.vim'

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Elixir

Plug 'https://github.com/elixir-editors/vim-elixir'


call plug#end()
