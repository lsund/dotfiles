augroup XML
    autocmd!
    autocmd FileType xml setlocal foldmethod=indent foldlevelstart=999 foldminlines=0
augroup END

au BufNewFile *.xml 0r ~/.vim/skels/skel.xml
au BufNewFile *.xsl 0r ~/.vim/skels/skel.xsl

