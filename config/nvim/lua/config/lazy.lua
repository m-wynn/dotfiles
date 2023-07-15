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

require("lazy").setup({
  { "LazyVim/LazyVim", import = "lazyvim.plugins" },
  { import = "lazyvim.plugins.extras.coding.copilot" },
  { import = "lazyvim.plugins.extras.ui.edgy" },
  { import = "lazyvim.plugins.extras.dap.core" },
  { import = "lazyvim.plugins.extras.dap.nlua" },
  { import = "lazyvim.plugins.extras.lang.docker" },
  { import = "lazyvim.plugins.extras.lang.json" },
  { import = "lazyvim.plugins.extras.lang.python" },
  { import = "lazyvim.plugins.extras.lang.rust" },
  { import = "lazyvim.plugins.extras.lang.terraform" },
  { import = "lazyvim.plugins.extras.lang.typescript" },
  { import = "plugins" },
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
})

require("lazyvim.util").get_root = function()
  local root_patterns = { ".git" }
  local path = vim.api.nvim_buf_get_name(0)
  path = path ~= "" and vim.loop.fs_realpath(path) or nil
  path = path and vim.fs.dirname(path) or vim.loop.cwd()
  ---@type string?
  local root = vim.fs.find(root_patterns, { path = path, upward = true })[1]
  root = root and vim.fs.dirname(root) or vim.loop.cwd()
  ---@type string[]
  local roots = {}
  if not root then
    if path then
      for _, client in pairs(vim.lsp.get_active_clients({ bufnr = 0 })) do
        local workspace = client.config.workspace_folders
        local paths = workspace
            and vim.tbl_map(function(ws)
              return vim.uri_to_fname(ws.uri)
            end, workspace)
          or client.config.root_dir and { client.config.root_dir }
          or {}
        for _, p in ipairs(paths) do
          local r = vim.loop.fs_realpath(p)
          if path:find(r, 1, true) then
            roots[#roots + 1] = r
          end
        end
      end
    end
    table.sort(roots, function(a, b)
      return #a > #b
    end)
    ---@type string?
    root = roots[1]
  end
  ---@cast root string
  return root
end
