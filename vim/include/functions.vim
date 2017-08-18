
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

function! SetRootTexDocument(name)
    let g:rtd = a:name
endfunction

function! CompileLatex()
    if search("begin{document}", "n")
        execute '! pdflatex %'
    elseif match(g:rtd, "unset") != 0
        execute '! pdflatex' g:rtd
    else
        echo "This file is probably not the root tex file. Call SetRootTexDocument"
    endif
endfunction

" Toggle

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
    endif
endfunction

