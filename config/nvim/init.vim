set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath
source ~/.config/nvim/vimrc

lua require('plugins')
lua require('lsp')
lua require('autocmds')

