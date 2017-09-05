
"  -----------------------------------------------------------------------------
"  Standalone plugins
source ~/.vim/scripts/BufOnly.vim
source ~/.vim/scripts/Rename.vim

"  -----------------------------------------------------------------------------
"  Vundle
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'

"  -----------------------------------------------------------------------------
"  Aesthetics
"
"  Colors
Plugin 'flazz/vim-colorschemes'
"
"  Good looking status/tabline
Plugin 'vim-airline/vim-airline'
let g:airline#extensions#tabline#enabled=1
let g:airline#extensions#tabline#fnamenod=':t'
let g:airline#extensions#tabline#buffer_idx_mode = 1
let g:airline_powerline_fonts = 1
set laststatus=2

"  -----------------------------------------------------------------------------
"  Autocompletion, YouCompleteMe, you complete me
"
"  Omnicomplete for a bunch of languages
Plugin 'Valloric/YouCompleteMe'
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
Plugin 'scrooloose/syntastic'
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%#warningmsg#
set statusline+=%*
let g:syntastic_always_populate_loc_list=1
let g:syntastic_auto_loc_list=1
let g:syntastic_check_on_open=1
let g:syntastic_check_on_wq=0
let g:syntastic_rst_checkers=['']
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
"  File search with fuzzy finder Ctrl-P
Plugin 'kien/ctrlp.vim'
let g:ctrlp_match_window='bottom,order:ttb'
let g:ctrlp_switch_buffer=0
let g:ctrlp_working_path_mode=0
let g:ctrlp_user_command='ag %s -l --nocolor --hidden -g ""'
let g:ctrlp_prompt_mappings = {
\ 'PrtBS()':              ['<bs>', '<c-d>'],
\ 'PrtDelete()':          ['<del>'],
\ 'PrtDeleteWord()':      ['<c-w>'],
\ 'PrtClear()':           ['<c-u>'],
\ 'PrtSelectMove("j")':   ['<c-n>', '<down>'],
\ 'PrtSelectMove("k")':   ['<c-p>', '<up>'],
\ 'PrtSelectMove("t")':   ['<Home>', '<kHome>'],
\ 'PrtSelectMove("b")':   ['<End>', '<kEnd>'],
\ 'PrtSelectMove("u")':   ['<PageUp>', '<kPageUp>'],
\ 'PrtSelectMove("d")':   ['<PageDown>', '<kPageDown>'],
\ 'PrtHistory(-1)':       ['<c-R>'],
\ 'PrtHistory(1)':        ['<c-r>'],
\ 'AcceptSelection("e")': ['<cr>', '<2-LeftMouse>'],
\ 'AcceptSelection("h")': ['<c-x>', '<c-cr>', '<c-s>'],
\ 'AcceptSelection("t")': ['<c-t>'],
\ 'AcceptSelection("v")': ['<c-v>', '<RightMouse>'],
\ 'ToggleFocus()':        ['<s-tab>'],
\ 'ToggleRegex()':        ['<c-r>'],
\ 'ToggleByFname()':      [],
\ 'ToggleType(1)':        ['<c-f>', '<c-up>'],
\ 'ToggleType(-1)':       ['<c-b>', '<c-down>'],
\ 'PrtExpandDir()':       ['<tab>'],
\ 'PrtInsert("c")':       ['<MiddleMouse>', '<insert>'],
\ 'PrtInsert()':          ['<c-\>'],
\ 'PrtCurStart()':        ['<c-a>'],
\ 'PrtCurEnd()':          ['<c-e>'],
\ 'PrtCurLeft()':         ['<c-h>', '<left>', '<c-^>'],
\ 'PrtCurRight()':        ['<c-l>', '<right>'],
\ 'PrtClearCache()':      ['<F5>'],
\ 'PrtDeleteEnt()':       ['<F7>'],
\ 'CreateNewFile()':      ['<c-y>'],
\ 'MarkToOpen()':         ['<c-z>'],
\ 'OpenMulti()':          ['<c-o>'],
\ 'PrtExit()':            ['<esc>', '<c-c>', '<c-g>'],
\ }
"
"  Keyword search with ag
Plugin 'mileszs/ack.vim'
if executable('ag')
let g:ackprg = 'ag --vimgrep'
endif
"
"  Tree view over directory
Plugin 'scrooloose/nerdtree'
let g:NERDTreeDirArrows=0
let NERDTreeMapOpenInTab='\t'
"
"  Class outline viewer
Plugin 'majutsushi/tagbar'
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
Plugin 'easymotion/vim-easymotion'
"
"  Easier use of f and F hotkey
Plugin 'unblevable/quick-scope'
"
" Rainbow-colored paranthesis
Plugin 'kien/rainbow_parentheses.vim'
au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces

"  -----------------------------------------------------------------------------
"  Utilities
"
"  Automatic commenting
Plugin 'tpope/vim-commentary'
"
"  Easier character surround
Plugin 'tpope/vim-surround'
"
"  Repeat a mapping eg surround.vim
Plugin 'tpope/vim-repeat'
"
"  Automatic table creation
Plugin 'dhruvasagar/vim-table-mode'
"
"  Snippets
Plugin 'SirVer/ultisnips'
let g:UltiSnipsExpandTrigger="<C-Space>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"
"
"  Align lines according to symbol
Plugin 'godlygeek/tabular'
let g:haskell_tabular=1
"
"  Easy align
Plugin 'junegunn/vim-easy-align'
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
Plugin 'tpope/vim-fugitive'

"  -----------------------------------------------------------------------------
"  Dependencies
"
" Asyncronous execution, used by ghcmod
Plugin 'Shougo/vimproc.vim'

"  -----------------------------------------------------------------------------
"  Haskell
"
"  Type display, error/warning display, split function insert
Plugin 'eagletmt/ghcmod-vim'
autocmd BufWritePost *.hs GhcModCheckAndLintAsync
let g:ghcmod_open_quickfix_function = 'GhcModQuickFix'
function! GhcModQuickFix()
    :botright cw
endfunction
"
"  Linting
Plugin 'mpickering/hlint-refactor-vim'
let g:hlintRefactor#disableDefaultKeybindings = 1
"
"  Direct access to hoogle
Plugin 'Twinside/vim-hoogle'
"
"  Syntax highlightning etc
Plugin 'neovimhaskell/haskell-vim'
"
"  Automatic Indentation
" Plugin 'alx741/vim-hindent'

"  -----------------------------------------------------------------------------
"  Clojure, clojure
"
"  Live repl, dynamic expression evaluation
Plugin 'tpope/vim-fireplace'
"
"  Static vim support for Leiningen (and boot)
Plugin 'tpope/vim-salve'
"
"  Dependencies of vim-salve
Plugin 'tpope/vim-dispatch'
Plugin 'tpope/vim-projectionist'
"
"  Editing S-expressions (Forked)
Plugin 'lsund/vim-sexp'
"
" Plugin 'tpope/vim-sexp-mappings-for-regular-people'
" let g:sexpr_insert_after_wrap=false
"
"  Linter (Not clojurescript)
Plugin 'venantius/vim-eastwood'
"
"  Runtime files
Plugin 'guns/vim-clojure-static'
"
"  Formatting
Plugin 'venantius/vim-cljfmt'


"  -----------------------------------------------------------------------------
"  Rust
"
"  Error checking, formatting, integration
Plugin 'rust-lang/rust.vim'

"  -----------------------------------------------------------------------------
"  Elm
"
"  Syntax highlightning, indentation, completion etc
Plugin 'ElmCast/elm-vim'
"
"  For compilation, evaluation, repl
Plugin 'lambdatoast/elm.vim'

"  -----------------------------------------------------------------------------
"  Latex
"
"  background compilation, completion, indentation, highlightning...
Plugin 'LaTeX-Box-Team/LaTeX-Box'

call vundle#end()
