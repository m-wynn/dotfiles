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
    "hrsh7th/nvim-cmp",
    dependencies = {
      { "hrsh7th/cmp-buffer" },
      { "hrsh7th/cmp-calc" },
      { "hrsh7th/cmp-cmdline" },
      { "hrsh7th/cmp-nvim-lua" },
      { "hrsh7th/cmp-nvim-lsp" },
      { "hrsh7th/cmp-nvim-lsp-document-symbol" },
      { "hrsh7th/cmp-nvim-lsp-signature-help" },
      { "hrsh7th/cmp-path" },
      { "andersevenrud/cmp-tmux" },
      { "zbirenbaum/copilot-cmp" },
      { "rafamadriz/friendly-snippets" },
      { "garymjr/nvim-snippets", opts = { friendly_snippets = true } },
    },
    opts = function(_, opts)
      local has_words_before = function()
        unpack = unpack or table.unpack
        local line, col = unpack(vim.api.nvim_win_get_cursor(0))
        return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
      end
      local cmp = require("cmp")
      opts.mapping = vim.tbl_extend("force", opts.mapping, {
        ["<Tab>"] = cmp.mapping(function(fallback)
          if cmp.visible() then
            cmp.select_next_item()
          elseif vim.snippet.active({ direction = 1 }) then
            vim.schedule(function()
              vim.snippet.jump(1)
            end)
          elseif has_words_before() then
            cmp.complete()
          else
            fallback()
          end
        end, { "i", "s" }),
        ["<S-Tab>"] = cmp.mapping(function(fallback)
          if cmp.visible() then
            cmp.select_prev_item()
          elseif vim.snippet.active({ direction = -1 }) then
            vim.schedule(function()
              vim.snippet.jump(-1)
            end)
          else
            fallback()
          end
        end, { "i", "s" }),
      })
      opts.sources = cmp.config.sources({
        { name = "path" },
        { name = "snippets" },
        { name = "copilot", group_index = 2 },
        { name = "nvim_lsp", group_index = 2 },
        { name = "nvim_lua", group_index = 2 },
        { name = "calc" },
        { name = "nvim_lsp_signature_help" },
        { name = "tmux" },
        { name = "buffer" },
      })
      opts.sorting = {
        priority_weight = 2,
        comparators = {
          require("copilot_cmp.comparators").prioritize,
          cmp.config.compare.offset,
          cmp.config.compare.exact,
          cmp.config.compare.score,
          cmp.config.compare.recently_used,
          cmp.config.compare.locality,
          cmp.config.compare.kind,
          cmp.config.compare.sort_text,
          cmp.config.compare.length,
          cmp.config.compare.order,
        },
      }
      opts.auto_brackets = { "terraform" }
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
  -- {
  --   "CopilotC-Nvim/CopilotChat.nvim",
  --   opts = {
  --     debug = false,
  --     show_help = false, -- Show help text for CopilotChatInPlace, default: yes
  --     context = "buffer",
  --     window = {
  --       layout = "float",
  --       relative = "cursor",
  --       width = 1,
  --       height = 0.4,
  --       row = 1,
  --     },
  --   },
  --   build = function()
  --     vim.notify("Please update the remote plugins by running ':UpdateRemotePlugins', then restart Neovim.")
  --   end,
  --   event = "VeryLazy",
  --   branch = "canary",
  --   keys = {
  --     {
  --       "<leader>aq",
  --       function()
  --         local input = vim.fn.input("Quick Chat: ")
  --         if input ~= "" then
  --           -- if visual, selection should be visual, else buffer
  --           if vim.v.visualmode == "v" then
  --             require("CopilotChat").ask(input, { selection = require("CopilotChat.select").visual })
  --           else
  --             require("CopilotChat").ask(input, { selection = require("CopilotChat.select").buffer })
  --           end
  --         end
  --       end,
  --       desc = "CopilotChat - Quick chat",
  --       mode = { "n", "v" },
  --     },
  --     {
  --       "<leader>ah",
  --       function()
  --         local actions = require("CopilotChat.actions")
  --         require("CopilotChat.integrations.telescope").pick(actions.help_actions())
  --       end,
  --       desc = "CopilotChat - Help actions",
  --     },
  --     {
  --       "<leader>ap",
  --       function()
  --         local actions = require("CopilotChat.actions")
  --         require("CopilotChat.integrations.telescope").pick(actions.prompt_actions())
  --       end,
  --       desc = "CopilotChat - Prompt actions",
  --     },
  --   },
  -- },
  {
    "mvaldes14/terraform.nvim",
    ft = "terraform",
  },
}
