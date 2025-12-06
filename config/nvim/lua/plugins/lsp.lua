return {
  {
    "mason-org/mason.nvim",
    opts = function(_, opts)
      table.insert(opts.ensure_installed, "rustywind")
      table.insert(opts.ensure_installed, "actionlint")
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
