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

local dap = require('dap')
dap.adapters.debugerl = {
    type = 'executable';
    command = os.getenv('HOME') .. '/git/edb/_build/default/bin/edb';
    args = {'dap' };
  }

dap.configurations.erlang = {
      {
        type = 'debugerl';
        request = 'launch';
        name = "Launch Erlang application",
        runInTerminal =  {
        kind =  "integrated",
        cwd =  os.getenv("DAP_WORKDIR"),
        args =  { "rebar3", "as", "test", "shell", "--config", "config/simple.config", "--name", "undefined" }
      },
      config =  {
        nameDomain =  "longnames",
        timeout =  60
      }
        }
  }



vim.g.copilot_no_tab_map = true
vim.api.nvim_set_keymap("i", "<leader>i", 'copilot#Accept("<CR>")', { silent = true, expr = true })
vim.g.copilot_filetypes = {
      ["javascript"] = true,
      ["typescript"] = true,
      ["rust"] = true,
      ["haskell"] = true,
      ["erlang"] = true,
      ["elixir"] = true,
      ["python"] = true,
      ["c#"] = true,
      ["*"] = false,
}

return require('packer').startup(function(use)
  use 'wbthomason/packer.nvim'

  use 'github/copilot.vim'

  use 'ndmitchell/ghcid'

  use 'mfussenegger/nvim-dap'

  use {
    "neovim/nvim-lspconfig",
    requires = {
      "mason-org/mason.nvim",
      "mason-org/mason-lspconfig.nvim",
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

      local util = require "formatter.util"

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
          ["eelixir"] = function()
            return {
                exe = "mix",
                args = {
                  "format",
                  "--stdin-filename",
                  util.escape_path(util.get_current_buffer_file_path()),
                  "-"
                },
                stdin = true
              }
            end,
          elixir = function()
            return {
                exe = "mix",
                args = {
                  "format",
                  "--stdin-filename",
                  util.escape_path(util.get_current_buffer_file_path()),
                  "-"
                },
                stdin = true
              }
            end,
          python = function()
            return {
              name = "ruff",
              exe = "ruff",
              args = {
                "format",
                "-",
              },
              stdin = true
            }
          end,
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
