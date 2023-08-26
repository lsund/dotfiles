local ensure_packer = function()
  local fn = vim.fn
  local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
  if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
    vim.cmd [[packadd packer.nvim]]
    return true
  end
  return false
end

local PACKER_BOOTSTRAP = ensure_packer()

return require('packer').startup(function(use)
  use 'wbthomason/packer.nvim'


  use {
    "neovim/nvim-lspconfig",
    requires = {
      "williamboman/mason.nvim",
      "williamboman/mason-lspconfig.nvim",
      "hrsh7th/nvim-cmp",
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-vsnip",
      "hrsh7th/vim-vsnip",
      "hrsh7th/vim-vsnip-integ",
      'kosayoda/nvim-lightbulb',
    },
    config = function()
      require('nvim-lightbulb').setup({autocmd = {enabled = true}})
      -- All remaining in lsp.lua
    end
  }

  use {
    'mhartington/formatter.nvim',
    config = function()
      local get_current_buffer_file_path = function()
        return vim.api.nvim_buf_get_name(0)
      end

      local escape_path = function(arg)
        return vim.fn.shellescape(arg, true)
      end

      require('formatter').setup {
        logging = true,
        log_level = 2,
        filetype = {
          ["javascript.jsx"] = function()
            return {
              name = "prettier",
              exe = "prettier",
              args = {"--stdin-filepath",escape_path(get_current_buffer_file_path())},
              stdin = true,
              try_node_modules = false
          }
          end,
          haskell = function()
            return {
                name = "fourmolu",
                exe = "fourmolu",
                args = {"--stdin-input-file", "-"},
                stdin = true,
              }
            end,
          erlang = function()
            return {
                name = "erlfmt",
                exe = "escript",
                args = {
                  "rebar3",
                  "fmt",
                  "-",
                },
                stdin = true,
                ignore_exitcode = true
              }
            end,
          elixir = function()
            return {
                name = "mixformat",
                exe = "mix",
                args = {
                  "format",
                  "-",
                },
                stdin = true
              }
            end,
          python = require("formatter.filetypes.python").black,
          rust = function()
            return {
              name = "rustfmt",
              exe = "rustfmt",
              stdin = true,
              args = { "--edition", "2021" }
            }
          end,
          cs = require("formatter.filetypes.cs").dotnetformat,
          json = require("formatter.filetypes.json").jq,
        }
      }
      local set_keymap = require('util').set_keymap
      set_keymap('n', '<leader>F', ':FormatWrite<CR>')
    end
  }

  if PACKER_BOOTSTRAP then
    require('packer').sync()
  end

end)
