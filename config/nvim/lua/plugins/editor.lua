local Util = require("lazyvim.util")
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
  {
    "tpope/vim-fugitive",
    keys = {
      {
        "<leader>gbr",
        "<cmd>GBrowse<cr>",
        desc = "Open in browser",
      },
      {
        "<leader>gbl",
        "<cmd>Git blame<cr>",
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
  { "epilande/vim-react-snippets" },
  { "honza/vim-snippets" },
  {
    "juliosueiras/vim-terraform-snippets",
    build = "rm -fr coq-user-snippets; mkdir coq-user-snippets && cat terraform/* >> coq-user-snippets/terraform.snip",
  },
  { "noahfrederick/vim-skeleton" },
  -- { "hashivim/vim-terraform" },
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
    "nvim-telescope/telescope.nvim",
    opts = function(_, opts)
      local trouble = require("trouble.providers.telescope")
      opts.defaults = vim.tbl_deep_extend("force", opts.defaults or {}, {
        mappings = { n = { ["<c-x>"] = trouble.open_with_trouble }, i = { ["<c-x>"] = trouble.open_with_trouble } },
      })
    end,
    keys = {
      {
        "<leader>td",
        "<cmd>Telescope terraform_doc full_name=hashicorp/aws<cr>",
        desc = "Goto Symbol",
      },
      {
        "<leader>ts",
        "<cmd>Telescope terraform state_list<cr>",
        desc = "Goto Symbol",
      },
    },
  },
}
