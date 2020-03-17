filetype off

" Vim-plug
call plug#begin('~/.vim/plugged')
"
"  Colors
Plug 'flazz/vim-colorschemes'

" Yanked highlightning
Plug 'machakann/vim-highlightedyank'

Plug 'ayu-theme/ayu-vim'
" Plug 'altercation/vim-colors-solarized'
" Plug 'frankier/neovim-colors-solarized-truecolor-only'
Plug 'lifepillar/vim-solarized8'
"
" Syntax highlightning for shakespeare templates
Plug 'pbrisbin/vim-syntax-shakespeare'
"
" Good looking status/tabline
"
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
"
" Omnicomplete for a bunch of languages
Plug 'Valloric/YouCompleteMe'
"
"  Syntax checker
" Plug 'scrooloose/syntastic'
Plug 'dense-analysis/ale'
"
" Fuzzy file finder
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
"
" Keyword search with ag
Plug 'mileszs/ack.vim'
"
" Tree view over directory
Plug 'scrooloose/nerdtree'
"
" Class outline viewer
Plug 'majutsushi/tagbar'
"
" Move anywhere on the screen
Plug 'easymotion/vim-easymotion'
"
" Easier use of f and F hotkey
Plug 'unblevable/quick-scope'
"
" Rainbow-colored paranthesis
Plug 'kien/rainbow_parentheses.vim'
"
" Navigate through vim and tmux seamlessly
Plug 'christoomey/vim-tmux-navigator'
"
" Automatic commenting
Plug 'tpope/vim-commentary'
"
" Easier character surround
Plug 'tpope/vim-surround'
"
" Repeat a mapping eg surround.vim
Plug 'tpope/vim-repeat'
"
" Automatic table creation
Plug 'dhruvasagar/vim-table-mode'
"
" Snippets
" Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
"
" Align lines according to symbol
Plug 'godlygeek/tabular'
"
" Easy align
Plug 'junegunn/vim-easy-align'
"
" Git
Plug 'tpope/vim-fugitive'
"
" Asyncronous execution, used by ghcmod
Plug 'Shougo/vimproc.vim'

" Asyncronous execution, used by neovim-ghci
Plug 'neomake/neomake'

" Plug 'eagletmt/ghcmod-vim'
" Bindings for hlint automatic application
Plug 'mpickering/hlint-refactor-vim'
"
"  Syntax highlightning for haskell
Plug 'neovimhaskell/haskell-vim'

"  Syntax hightlightning for purescript
Plug 'michaelficarra/purescript-vim'

" Intero
Plug 'parsonsmatt/intero-neovim'

" Live repl, dynamic expression evaluation
Plug 'tpope/vim-fireplace'

Plug 'tpope/vim-projectionist'

" Error checking, formatting, integration
Plug 'rust-lang/rust.vim'

" Elm elm
Plug 'ElmCast/elm-vim'
Plug 'lambdatoast/elm.vim'

" background compilation, completion, indentation, highlightning...
Plug 'LaTeX-Box-Team/LaTeX-Box'

" Markdown markdown
Plug 'tpope/vim-markdown'

" XQuery
Plug 'jeroenp/vim-xquery-syntax'

" XML editing
Plug 'sukima/xmledit'

" Dhall editing
Plug 'vmchale/dhall-vim'

" Javascript
Plug 'pangloss/vim-javascript'
Plug 'mxw/vim-jsx'

" Html
Plug 'mattn/emmet-vim'

Plug 'skywind3000/asyncrun.vim'

" call vundle#end()
call plug#end()
