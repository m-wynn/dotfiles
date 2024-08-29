table.unpack = table.unpack or unpack
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
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  })
end
vim.opt.rtp:prepend(vim.env.LAZY or lazypath)

local plugins = {
  "ui.edgy",
  "ui.mini-indentscope",
  "coding.copilot",
  "coding.copilot-chat",
  "coding.mini-surround",
  "coding.neogen",
  "coding.yanky",
  "dap.core",
  "dap.nlua",
  "editor.dial",
  "editor.inc-rename",
  "editor.mini-diff",
  "editor.outline",
  "editor.telescope",
  "formatting.black",
  "lang.docker",
  "lang.git",
  "lang.go",
  "lang.helm",
  "lang.json",
  "lang.markdown",
  "lang.nix",
  "lang.php",
  "lang.prisma",
  "lang.python",
  "lang.rust",
  "lang.sql",
  "lang.tailwind",
  "lang.terraform",
  "lang.toml",
  "lang.typescript",
  "lang.yaml",
  "linting.eslint",
  "test.core",
  "util.dot",
  "util.mini-hipatterns",
  "util.octo",
  "util.project",
}

-- Import each plugin
local setupTable = { { "LazyVim/LazyVim", import = "lazyvim.plugins" } }
for _, plugin in ipairs(plugins) do
  table.insert(setupTable, { import = "lazyvim.plugins.extras." .. plugin })
end

-- Add the non-extras plugin
table.insert(setupTable, { import = "plugins" })

require("lazy").setup({
  defaults = {
    lazy = false,
    version = false,
  },
  install = { colorscheme = { "catppuccin" } },
  checker = { enabled = true },
  performance = {
    rtp = {
      disabled_plugins = {
        "gzip",
        -- "matchit",
        -- "matchparen",
        -- "netrwPlugin",
        "tarPlugin",
        "tohtml",
        "tutor",
        "zipPlugin",
      },
    },
  },
  table.unpack(setupTable),
})

vim.g.root_spec = { ".git", "lsp", "lua", "cwd" }
vim.lsp.set_log_level("error")
