local function map(mode, lhs, rhs, opts)
    local options = {noremap = true}
    if opts then options = vim.tbl_extend('force', options, opts) end
    vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

local function t(str) return vim.api.nvim_replace_termcodes(str, true, true, true) end

vim.g.mapleader = " "

-- Remove search highlights
map('n', '<ESC>', '<cmd>nohlsearch<CR><Esc>')

map('n', '//', '*')
map('n', '??', '#')
-- map('n', ';', ':')

-- I find myself opening files in the same folder a lot
map('n', '<C-e>', ':e %:h/')
map('c', '<C-e>', '| e %:h/')
