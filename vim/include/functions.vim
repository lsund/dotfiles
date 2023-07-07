function! StyleHaskell(...)
    :execute "normal ma"
    :%!stylish-haskell
    :execute "normal `a"
endfunction

function! FormatParagraph(...)
    if g:format_paragraph
        :execute "normal mavapgq`a"
        :execute "normal ma"
        :%s/\([^\ \\]\)  \([^\ \\]\)/\1 \2/
        :execute "normal `a"
    endif
endfunction

" Prettify JSON
command JSONprettify %!python -m json.tool

" Text Insertion

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

" Compiling latex from vim =====================================================

let g:rtd = "unset"

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
