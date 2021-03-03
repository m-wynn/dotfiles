-- While we migrate
vim.api.nvim_command('source ~/.config/nvim/oldinit.vim')

require('packages')
require('settings')
