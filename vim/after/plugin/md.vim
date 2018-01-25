
" Replace 2 spaces between two words with one on write
" Autoformat paragraph
"
" au BufWrite *.md silent! :%s/\([^\ \\]\)  \([^\ \\]\)/\1 \2/
" au BufWrite *.md execute "normal mavapgq`a"
