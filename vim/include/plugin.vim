
"  -----------------------------------------------------------------------------
"  Standalone plugins
source ~/.vim/scripts/BufOnly.vim
source ~/.vim/scripts/Rename.vim

"  -----------------------------------------------------------------------------
"  Vundle
" set rtp+=~/.vim/bundle/Vundle.vim
" call vundle#begin()
" Plug 'VundleVim/Vundle.vim'

" Vim-plug
call plug#begin('~/.vim/plugged')


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Aestetics
"
"  Colors
Plug 'flazz/vim-colorschemes'
"
"  Good looking status/tabline
Plug 'vim-airline/vim-airline'
let g:airline#extensions#tabline#enabled=1
let g:airline#extensions#tabline#fnamenod=':t'
let g:airline#extensions#tabline#buffer_idx_mode = 1
let g:airline_powerline_fonts = 1
set laststatus=2

"  -----------------------------------------------------------------------------
"  Autocompletion, YouCompleteMe, you complete me
"
"  Omnicomplete for a bunch of languages
Plug 'Valloric/YouCompleteMe'
let g:ycm_python_binary_path = '/usr/bin/python3'
let g:ycm_global_ycm_extra_conf = '~/.ycm_extra_conf.py'
let g:ycm_autoclose_preview_window_after_completion = 1
let g:ycm_autoclose_preview_window_after_insertion = 1
let g:ycm_use_ultisnips_completer = 0
let g:ycm_key_list_select_completion = ['<C-t>']
let g:ycm_key_list_previous_completion = ['<C-n>']

"  -----------------------------------------------------------------------------
"  Syntax checker, Syntastic, syntastic
"
"  Built in checker for many languages
Plug 'scrooloose/syntastic'
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%#warningmsg#
set statusline+=%*
let g:syntastic_always_populate_loc_list=1
let g:syntastic_auto_loc_list=1
let g:syntastic_check_on_open=1
let g:syntastic_check_on_wq=0
let g:syntastic_rst_checkers=['']
let g:syntastic_haskell_checkers=['hlint']
let g:syntastic_clojure_checkers=['eastwood']
let g:syntastic_mode_map =
            \{
            \'mode': 'active',
            \'active_filetypes': [],
            \'passive_filetypes': []
            \}

"  -----------------------------------------------------------------------------
"  Search and View
"
"  File search with fuzzy finder fzf FZF Fzf
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
"
"  Keyword search with ag
Plug 'mileszs/ack.vim'
if executable('ag')
let g:ackprg = 'ag --vimgrep'
endif
"
"  Tree view over directory
Plug 'scrooloose/nerdtree'
let g:NERDTreeDirArrows=0
let NERDTreeMapOpenInTab='\t'
"
"  Class outline viewer
Plug 'majutsushi/tagbar'
let g:tagbar_type_rust = {
\ 'ctagstype' : 'rust',
\ 'kinds' : [
    \'T:types,type definitions',
    \'f:functions,function definitions',
    \'g:enum,enumeration names',
    \'s:structure names',
    \'m:modules,module names',
    \'c:consts,static constants',
    \'t:traits,traits',
    \'i:impls,trait implementations',
\]
\}

"  -----------------------------------------------------------------------------
"  Motion
"
"  Move anywhere on the screen
Plug 'easymotion/vim-easymotion'
"
"  Easier use of f and F hotkey
Plug 'unblevable/quick-scope'
"
" Rainbow-colored paranthesis
Plug 'kien/rainbow_parentheses.vim'
au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces

"  -----------------------------------------------------------------------------
"  Utilities
"
"  Automatic commenting
Plug 'tpope/vim-commentary'
"
"  Easier character surround
Plug 'tpope/vim-surround'
"
"  Repeat a mapping eg surround.vim
Plug 'tpope/vim-repeat'
"
"  Automatic table creation
Plug 'dhruvasagar/vim-table-mode'
"
"  Snippets
Plug 'SirVer/ultisnips'
let g:UltiSnipsExpandTrigger="<C-Space>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"
"
"  Align lines according to symbol
Plug 'godlygeek/tabular'
let g:haskell_tabular=1
"
"  Easy align
Plug 'junegunn/vim-easy-align'
let g:easy_align_delimiters = {
\ '(': { 'pattern': '[()]',
\        'left_margin' : 1,
\        'right_margin' : 0,
\        'stick_to_left' : 0 }
\ }
"  -----------------------------------------------------------------------------
"  Wrappers
"
"  Git
Plug 'tpope/vim-fugitive'

"  -----------------------------------------------------------------------------
"  Dependencies
"
" Asyncronous execution, used by ghcmod
Plug 'Shougo/vimproc.vim'

"  -----------------------------------------------------------------------------
"  Haskell
"
"  Type display, error/warning display, split function insert
Plug 'eagletmt/ghcmod-vim'
autocmd BufWritePost *.hs GhcModCheckAndLintAsync
let g:ghcmod_open_quickfix_function = 'GhcModQuickFix'
function! GhcModQuickFix()
    :botright cw
endfunction
"
"  Linting
Plug 'mpickering/hlint-refactor-vim'
let g:hlintRefactor#disableDefaultKeybindings = 1
"
"  Direct access to hoogle
Plug 'Twinside/vim-hoogle'
"
"  Syntax highlightning etc
Plug 'neovimhaskell/haskell-vim'
"
"  Automatic Indentation
" Plug 'alx741/vim-hindent'

"  -----------------------------------------------------------------------------
"  Clojure, clojure
"
"  Live repl, dynamic expression evaluation
Plug 'tpope/vim-fireplace'
"
"  Static vim support for Leiningen (and boot)
Plug 'tpope/vim-salve'
"
"  Dependencies of vim-salve
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-projectionist'
"
"  Editing S-expressions (Forked)
Plug 'lsund/vim-sexp'
"
" Plug 'tpope/vim-sexp-mappings-for-regular-people'
" let g:sexpr_insert_after_wrap=false
"
"  Linter (Not clojurescript)
Plug 'venantius/vim-eastwood'
"
"  Runtime files
Plug 'guns/vim-clojure-static'
Plug 'guns/vim-clojure-highlight'
"
"  Formatting
Plug 'venantius/vim-cljfmt'
let g:clj_fmt_autosave = 0

"  -----------------------------------------------------------------------------
"  Rust
"
"  Error checking, formatting, integration
Plug 'rust-lang/rust.vim'

"  -----------------------------------------------------------------------------
"  Elm
"
"  Syntax highlightning, indentation, completion etc
Plug 'ElmCast/elm-vim'
"
"  For compilation, evaluation, repl
Plug 'lambdatoast/elm.vim'

"  -----------------------------------------------------------------------------
"  Latex
"
"  background compilation, completion, indentation, highlightning...
Plug 'LaTeX-Box-Team/LaTeX-Box'

" call vundle#end()
call plug#end()
