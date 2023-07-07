return {
  set_keymap = function(scope, from, to)
    vim.api.nvim_set_keymap(scope, from, to, {silent = true, noremap = true})
  end,
  has_words_before = function()
    local line, col = unpack(vim.api.nvim_win_get_cursor(0))
    return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match('%s') == nil
  end
}
