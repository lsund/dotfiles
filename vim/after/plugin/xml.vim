" Populate new files with XML or XSLT header
au BufNewFile *.xml 0r ~/.vim/skels/skel.xml
au BufNewFile *.xsl 0r ~/.vim/skels/skel.xsl

" Use xmllint when formatting with =
au FileType xml setlocal equalprg=xmllint\ --format\ --recover\ -\ 2>/dev/null
au FileType xml setlocal foldmethod=indent foldlevelstart=99 foldminlines=0
