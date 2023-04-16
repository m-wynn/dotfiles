return {
  {
    "catppuccin/nvim",
    name = "catppuccin",
    opts = {
      flavour = "mocha",
      background = {
        light = "latte",
        dark = "mocha",
      },
      transparent_background = false,
      term_colors = true,
      dim_inactive = {
        enabled = true,
        percentage = 0.05,
      },
      integrations = {
        cmp = true,
        dap = {
          enabled = true,
          enable_ui = true,
        },
        gitsigns = true,
        lualine = true,
        mini = true,
        native_lsp = {
          enabled = true,
          virtual_text = {
            errors = { "italic" },
            hints = { "italic" },
            warnings = { "italic" },
            information = { "italic" },
          },
          underlines = {
            errors = { "underline" },
            hints = { "underline" },
            warnings = { "underline" },
            information = { "underline" },
          },
        },
        notify = true,
        nvimtree = true,
        telescope = true,
        treesitter = true,
        treesitter_context = true,
        lsp_trouble = true,
        which_key = true,
      },
    },
  },
  {
    "nvim-telescope/telescope-fzf-native.nvim",
    dependencies = { "nvim-telescope/telescope.nvim" },
    build = "make",
    config = function()
      require("telescope").load_extension("fzf")
    end,
  },
  {
    "akinsho/toggleterm.nvim",
    keys = {
      {
        "<leader>tt",
        function()
          require("toggleterm").exec("cd $(dirname " .. vim.fn.expand("%:p") .. ")/")
        end,
      },
    },
    opts = {
      size = 120,
      shade_terminals = false,
      direction = "vertical",
    },
  },
  "notjedi/nvim-rooter.lua",
  "tommcdo/vim-fubitive",
  "tpope/vim-fugitive",
  "tpope/vim-rhubarb",
  {
    "nvim-treesitter/nvim-treesitter",
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, {
          "bash",
          "c",
          "cmake",
          "comment",
          "cpp",
          "css",
          "devicetree",
          "dockerfile",
          "gitattributes",
          "gitignore",
          "go",
          "hcl",
          "hjson",
          "hocon",
          "html",
          "http",
          "java",
          "javascript",
          "jsdoc",
          "json",
          "json5",
          "jsonc",
          "kotlin",
          "latex",
          "llvm",
          "lua",
          "make",
          "markdown",
          "markdown_inline",
          "nix",
          "php",
          "phpdoc",
          "python",
          "regex",
          "ruby",
          "rust",
          "scss",
          "sql",
          "terraform",
          "tsx",
          "typescript",
          "vim",
          "vimdoc",
          "yaml",
        })
      end
    end,
  },
  { "epilande/vim-react-snippets" },
  { "honza/vim-snippets" },
  {
    "juliosueiras/vim-terraform-snippets",
    build = "rm -fr coq-user-snippets; mkdir coq-user-snippets && cat terraform/* >> coq-user-snippets/terraform.snip",
  },
  { "noahfrederick/vim-skeleton" },
  {
    "towolf/vim-helm",
    config = function()
      vim.api.nvim_create_autocmd({ "BufNewFile", "BufRead" }, {
        pattern = {
          "*/templates/*.yaml",
          "*/templates/*.yml",
          "*/templates/*.tpl",
          "*.gotmpl,helmfile*.yaml",
        },
        callback = function()
          vim.opt_local.filetype = "helm"
        end,
      })
    end,
  },
}
