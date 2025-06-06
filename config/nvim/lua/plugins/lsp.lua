return {
  {
    "williamboman/mason.nvim",
    opts = function(_, opts)
      table.insert(opts.ensure_installed, "rustywind")
    end,
  },
  {
    "stevearc/conform.nvim",
    opts = {
      formatters_by_ft = {
        php = { "php_cs_fixer", "phpcbf" },
        typescriptreact = { "rustywind" },
      },
      formatters = {
        shfmt = {
          prepend_args = { "-i", "2", "-ci", "-sr" },
        },
      },
    },
  },
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
        local keys = require("lazyvim.plugins.lsp.keymaps").get()
        keys[#keys + 1] = { "<leader>cc", false }
        keys[#keys + 1] =
          { "<leader>ccl", vim.lsp.codelens.run, desc = "Run Codelens", mode = { "n", "v" }, has = "codeLens" }
      end,
    },
    opts = {
      servers = {
        astro = {},
        gopls = {},
        tflint = {},
        intelephense = {},
        nil_ls = { mason = false },
        prismals = {},
        docker_compose_language_service = {
          filetypes = { "dockerfile " },
        },
        helm_ls = {
          filetypes = { "helm" },
        },
        yamlls = {
          filetypes = { "yaml", "yaml.ghaction" },
        },
      },
    },
  },
}
