-- While we migrate

require('packages')
require('settings')

vim.api.nvim_command('source ~/.config/nvim/oldinit.vim')
