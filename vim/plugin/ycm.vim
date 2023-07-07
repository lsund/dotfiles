function! RunningOn(hostname)
  return match(hostname(), a:hostname) >= 0
endfunction

if RunningOn("N52930")
  let g:ycm_python_binary_path                        = '/usr/bin/python3'
  let g:ycm_server_python_interpreter                 = '/usr/bin/python3'
endif

if RunningOn("renewise")
  let g:ycm_python_binary_path                        = '/usr/local/opt/python@3.9/bin/python3.9'
  let g:ycm_server_python_interpreter                 = '/usr/local/opt/python@3.9/bin/python3.9'
endif

let g:ycm_global_ycm_extra_conf                     = '~/.ycm_extra_conf.py'
let g:ycm_key_list_select_completion                = ['<C-t>']
let g:ycm_key_list_previous_completion              = ['<C-n>']
let g:ycm_autoclose_preview_window_after_completion = 1
let g:ycm_autoclose_preview_window_after_insertion  = 1
let g:ycm_use_ultisnips_completer                   = 1
let g:ycm_confirm_extra_conf                        = 0

let g:ycm_filetype_blacklist = {}
