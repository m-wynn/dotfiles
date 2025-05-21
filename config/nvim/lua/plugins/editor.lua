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
    },
  },
  {
    "folke/edgy.nvim",
    opts = {
      animate = {
        cps = 400,
      },
    },
  },
  {
    "nvim-neo-tree/neo-tree.nvim",
    branch = "main",
    opts = {
      auto_expand_width = false,
      filesystem = {
        filtered_items = {
          always_show = {
            ".github",
          },
        },
      },
    },
  },
  -- "notjedi/nvim-rooter.lua",
  "tommcdo/vim-fubitive",
  {
    "tpope/vim-fugitive",
    keys = {
      {
        "<leader>gbr",
        "<cmd>GBrowse<cr>",
        mode = "n",
        desc = "Open in browser",
      },
      {
        "<leader>gbr",
        ":GBrowse<cr>",
        mode = "v",
        desc = "Open in browser",
      },
      {
        "<leader>gbl",
        "<cmd>Git blame<cr>",
        mode = "n",
        desc = "Git blame",
      },
      {
        "<leader>gbl",
        ":Git blame<cr>",
        mode = "v",
        desc = "Git blame",
      },
      {
        "<leader>gw",
        "<cmd>Gw<cr>",
        desc = "Git write",
      },
      {
        "<leader>gfu",
        "<cmd>Git fixup<cr>",
        desc = "Git fixup",
      },
      {
        "<leader>gpf",
        "<cmd>Git! push --force-with-lease<cr>",
        desc = "Git push force (with lease)",
      },
      {
        "<leader>gds",
        "<cmd>Gdiffsplit<cr>",
        desc = "Git diff split",
      },
    },
  },
  "tpope/vim-rhubarb",
  {
    "nvim-treesitter/nvim-treesitter",
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, {
          "astro",
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
          "nix",
          "php",
          "phpdoc",
          "prisma",
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
  {
    "ibhagwan/fzf-lua",
    opts = function(_, opts)
      local config = require("fzf-lua.config")

      -- Trouble: I'm already using ctrl-t in tmux
      if LazyVim.has("trouble.nvim") then
        config.defaults.actions.files["ctrl-x"] = require("trouble.sources.fzf").actions.open
      end
    end,
  },
  {
    "pwntester/octo.nvim",
    opts = {
      default_remote = { "gh", "upstream", "origin" },
    },
  },
}
