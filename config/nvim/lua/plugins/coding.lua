return {
  { "echasnovski/mini.pairs", enabled = false },
  {
    "Wansmer/treesj",
    keys = {
      {
        "gJ",
        function()
          require("treesj").join()
        end,
        desc = "Join node",
      },
      {
        "gS",
        function()
          require("treesj").split()
        end,
        desc = "Split node",
      },
    },
    opts = {
      max_join_length = 150,
      use_default_keymaps = false,
    },
  },
  {
    "saghen/blink.cmp",
    lazy = false, -- lazy loading handled internally
    ---@module 'blink.cmp'
    ---@type blink.cmp.Config
    opts = {
      keymap = {
        ["<Tab>"] = {
          LazyVim.cmp.map({ "snippet_forward", "ai_accept" }),
          "select_next",
          "fallback",
        },
        ["<S-Tab>"] = {
          LazyVim.cmp.map({ "snippet_backward", "ai_accept" }),
          "select_prev",
          "fallback",
        },
      },
    },
  },
  {
    "zbirenbaum/copilot.lua",
    opts = {
      filetypes = {
        json = false,
        yaml = true,
        markdown = false,
        help = false,
        gitcommit = false,
        gitrebase = false,
        hgcommit = false,
        svn = false,
        cvs = false,
        ["."] = false,
      },
    },
  },
}
