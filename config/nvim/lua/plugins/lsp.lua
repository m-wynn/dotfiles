return {
  {
    "stevearc/conform.nvim",
    opts = {
      formatters_by_ft = {
        ["php"] = { "php_cs_fixer", "phpcbf" },
      },
    },
  },
  {
    "mfussenegger/nvim-lint",
    opts = {
      linters_by_ft = {
        terraform = { "terraform_validate" },
        tf = { "terraform_validate" },
      },
    },
  },

  -- {
  --   "nvimtools/none-ls.nvim",
  --   dependencies = { "nvim-lua/plenary.nvim", "neovim/nvim-lspconfig" },
  --   opts = function(_, opts)
  --     local nls = require("null-ls")
  --     vim.list_extend(opts.sources, {
  --       nls.builtins.formatting.eslint_d,
  --       nls.builtins.formatting.fixjson,
  --       nls.builtins.formatting.nginx_beautifier,
  --       nls.builtins.formatting.phpcbf,
  --       nls.builtins.formatting.shellharden,
  --       nls.builtins.formatting.sqlformat,
  --       nls.builtins.formatting.terraform_fmt,
  --       nls.builtins.formatting.phpcsfixer,
  --       nls.builtins.formatting.isort,
  --       nls.builtins.diagnostics.shellcheck,
  --       nls.builtins.diagnostics.flake8,
  --       nls.builtins.formatting.prettier,
  --     })
  --   end,
  -- },
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      "Afourcat/treesitter-terraform-doc.nvim",
      init = function()
        require("lazyvim.util").lsp.on_attach(function(_, buffer)
          require("treesitter-terraform-doc").setup({
            command_name = "OpenDoc",
            url_opener_command = "!open",
          })
          vim.keymap.set("n", "<leader>do", ":OpenDoc<cr>", { desc = "OpenDoc", buffer = buffer })
        end)
      end,
    },
    opts = {
      servers = {
        gopls = {},
        -- terraformls = {},
        -- terraform_lsp = {},
        tflint = {},
        bashls = {},
        -- rust_analyzer = {},
        intelephense = {},
        -- pyright = {},
        docker_compose_language_service = {
          filetypes = { "dockerfile " },
        },
        helm_ls = {
          filetypes = { "helm" },
        },
      },
      -- setup = {
      --   helm_ls = function(server, server_opts)
      --     local configs = require("lspconfig.configs")
      --     local util = require("lspconfig.util")
      --     if not configs.helm_ls then
      --       configs.helm_ls = {
      --         default_config = {
      --           cmd = { "helm_ls", "serve" },
      --           filetypes = { "helm" },
      --           root_dir = function(fname)
      --             return (util.root_pattern("Chart.lock") or util.root_pattern("Chart.yaml"))(fname)
      --           end,
      --         },
      --       }
      --       require("lspconfig")[server].setup(server_opts)
      --     end
      --   end,
      -- },
    },
  },
}
