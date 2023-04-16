return {
  { "echasnovski/mini.pairs", enabled = false },
  {
    "nmac427/guess-indent.nvim",
    opts = {
      filetype_exclude = {
        "netrw",
        "tutor",
      },
      buttype_exclude = {
        "help",
        "nofile",
        "terminal",
        "prompt",
      },
    },
  },
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
    "L3MON4D3/LuaSnip",
    keys = function()
      return {} -- disable default tab
    end,
  },
  {
    "hrsh7th/nvim-cmp",
    dependencies = {
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-calc",
      "hrsh7th/cmp-cmdline",
      "hrsh7th/cmp-nvim-lua",
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-nvim-lsp-document-symbol",
      "hrsh7th/cmp-nvim-lsp-signature-help",
      "hrsh7th/cmp-path",
      "andersevenrud/cmp-tmux",
      "saadparwaiz1/cmp_luasnip",
    },
    opts = function(_, opts)
      local has_words_before = function()
        unpack = unpack or table.unpack
        local line, col = unpack(vim.api.nvim_win_get_cursor(0))
        return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
      end
      local luasnip = require("luasnip")
      local cmp = require("cmp")
      opts.mapping = vim.tbl_extend("force", opts.mapping, {
        ["<Tab>"] = cmp.mapping(function(fallback)
          if cmp.visible() then
            cmp.select_next_item()
          elseif luasnip.expand_or_jumpable() then
            luasnip.expand_or_jump()
          elseif has_words_before() then
            cmp.complete()
          else
            fallback()
          end
        end, { "i", "s" }),
        ["<S-Tab>"] = cmp.mapping(function(fallback)
          if cmp.visible() then
            cmp.select_prev_item()
          elseif luasnip.jumpable(-1) then
            luasnip.jump(-1)
          else
            fallback()
          end
        end, { "i", "s" }),
      })
      opts.sources = cmp.config.sources(
        vim.list_extend(opts.sources, {
          { name = "luasnip" },
          { name = "nvim_lsp" },
          { name = "nvim_lua" },
          { name = "calc" },
          { name = "nvim_lsp_signature_help" },
          { name = "tmux" },
        }),
        {
          { name = "buffer" },
        }
      )
    end,
    config = function(_, opts)
      local cmp = require("cmp")
      cmp.setup(opts)
      cmp.setup.cmdline({ "/", "?" }, {
        mapping = cmp.mapping.preset.cmdline(),
        sources = cmp.config.sources({
          { name = "nvim_lsp_document_symbol" },
        }, {
          { name = "buffer" },
        }),
      })
      cmp.setup.cmdline(":", {
        mapping = cmp.mapping.preset.cmdline(),
        sources = cmp.config.sources({
          { name = "path" },
        }, {
          {
            name = "cmdline",
            option = {
              ignore_cmds = { "Man", "!" },
            },
          },
        }),
      })
    end,
  },
  {
    "echasnovski/mini.indentscope",
    config = function()
      require("mini.indentscope").setup()
    end,
  },
}
