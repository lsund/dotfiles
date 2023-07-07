"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Key bindings

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Navigation

nnoremap gt :bn<CR>
nnoremap gT :bp<CR>

" Command line bindings
cnoremap <C-G> <C-C>
cnoremap <C-A> <Home>
cnoremap <C-B> <Left>
cnoremap <C-F> <Right>

" Insert mode command line bindings
inoremap <C-e> <C-o>$
inoremap <C-a> <C-o>0
inoremap <C-b> <C-o>h
inoremap <C-f> <C-o>l

vnoremap <C-G> <C-C>

nnoremap <F1> <ESC>

" Make Y behave as D, C etc
noremap Y y$

" Fast scroll
" nnoremap J M4jzz''4j
" nnoremap K M4kzz''4k
nnoremap J 4jzz
nnoremap K 4kzz

nnoremap j gj
nnoremap k gk

" Push line up/down
nnoremap <C-j> 0i<CR><ESC>
nnoremap <C-k> 0i<BS><ESC>

" Use Control g as escape
inoremap <C-g> <ESC>
nnoremap <C-g> <ESC>
" Disable backspace
inoremap <C-Backspace> <Backspace>
inoremap <Backspace> <Nop>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Commands

" Write root file as non-root
cmap w!! w !sudo tee > /dev/null %

" List buffers
nmap <leader>y :Buffers<CR>

" Go to Last buffer
nmap <leader><S-y> :b#<CR>

" Kill buffer
nmap <leader>' :bp <BAR> bd #<CR>
" Kill every buffer except this one
nmap <leader>" :BufOnly<CR>

" Write file
nmap <leader>, :w<CR>
nmap <leader>< :wall<CR>

" Open file
nmap <leader>o :args<space>
nmap <leader>e <C-p>
nmap <leader>j :Files<space>

" Edit history
map <leader>c q:

" Exit
noremap Z! :q!<CR>

"Split
map <leader>\| :vsplit<CR>
map <leader>- :split<CR>

" Clear highlight
noremap <leader>cr :call clearmatches()<CR>
nnoremap <leader>nh :nohlsearch<CR>

" Insert increasing counters
vmap <leader>icn :I<CR>
vmap <leader>ica :IA<CR>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugin rebinindings
command! -nargs=* -bang Rg call RipgrepFzf(<q-args>, <bang>0)
nnoremap <leader>aa :Rg<CR>
nnoremap <leader>ak :call SearchKeywordUnderCursor()<CR>

nmap gbl :Git blame<CR>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Custom functions

autocmd FileType qf nnoremap <buffer> O <Enter> :call DeleteEmptyBuffers()<CR>

autocmd FileType qf nnoremap <buffer> <leader>\| <C-w><Enter><C-w>L :call DeleteEmptyBuffers()<CR>

nmap <silent> <leader>ss :call Toggle('spell')<CR>
nmap <silent> <leader>sp :call Toggle('paste')<CR>
nmap <silent> <leader>sl :call Toggle('list')<CR>
nmap <silent> <leader>sw :call Toggle('whitespace')<CR>
nmap <silent> <leader>sf :call Toggle('paragraph')<CR>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"

function! DeleteEmptyBuffers()
    let [i, n; empty] = [1, bufnr('$')]
    while i <= n
        if bufexists(i) && bufname(i) == ''
            call add(empty, i)
        endif
        let i += 1
    endwhile
    if len(empty) > 0
        exe 'bdelete' join(empty)
    endif
endfunction

function! Toggle(setting)
    if a:setting == 'spell'
        if &spell
            set nospell
        else
            set spell
        end
    elseif a:setting == 'paste'
        if &paste
            set nopaste
        else
            set paste
        end
    elseif a:setting == 'list'
        if &list
            set nolist
        else
            set list
        end
    elseif a:setting == 'whitespace'
        let g:delete_trailing_whitespace = !get(g:, 'delete_trailing_whitespace', 1)
        echo "Deleting whitespace:" g:delete_trailing_whitespace
    elseif a:setting == 'paragraph'
        let g:format_paragraph = !get(g:, 'format_paragraph', 1)
        echo "Format Paragraph:" g:format_paragraph
    endif
endfunction

function SearchKeywordUnderCursor()
  let l:wordUnderCursor = expand("<cword>")
  :execute "Rg" l:wordUnderCursor
endfunction

function! RipgrepFzf(query, fullscreen)
  let command_fmt = 'rg --column --line-number --no-heading --color=always --smart-case -- %s || true'
  let initial_command = printf(command_fmt, shellescape(a:query))
  let reload_command = printf(command_fmt, '{q}')
  let spec = {'options': ['--disabled', '--query', a:query, '--bind', 'change:reload:'.reload_command]}
  call fzf#vim#grep(initial_command, 1, spec, a:fullscreen)
endfunction

function! FormatParagraph(...)
    if g:format_paragraph
        :execute "normal mavapgq`a"
        :execute "normal ma"
        :%s/\([^\ \\]\)  \([^\ \\]\)/\1 \2/
        :execute "normal `a"
    endif
endfunction

function! StyleHaskell(...)
    :execute "normal ma"
    :%!stylish-haskell
    :execute "normal `a"
endfunction

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Haskell code generation

function! HaskellModuleSection(...)
    let name = 0 < a:0 ? a:1 : inputdialog("Section name: ")

    return  repeat('-', s:width) . "\n"
    \       . "--  " . name . "\n"
    \       . "\n"

endfunction

function! HaskellModuleHeader(...)
    let name = 0 < a:0 ? a:1 : inputdialog("Module: ")
    let note = 1 < a:0 ? a:2 : inputdialog("Note: ")
    let description = 2 < a:0 ? a:3 : inputdialog("Describe this module: ")

    return  repeat('-', s:width) . "\n"
    \       . "--   \n"
    \       . "-- Module      : " . name . "\n"
    \       . "-- Note        : " . note . "\n"
    \       . "-- \n"
    \       . "-- " . description . "\n"
    \       . "-- \n"
    \       . repeat('-', s:width) . "\n"
    \       . "\n"

endfunction

function! CompileLatex()
    if search("begin{document}", "n")
        execute '! lualatex -shell-escape %'
    elseif match(g:rtd, "unset") != 0
        execute '! lualatex -shell-escape' g:rtd
    else
        echo "This file is probably not the root tex file. I just set it to 'report.tex' for you"
        let g:rtd = 'report.tex'
    endif
endfunction

