local function map(mode, lhs, rhs, opts)
    local options = { noremap = true, silent = true }
    if opts then options = vim.tbl_extend('force', options, opts) end
    vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

-- Remove search highlights
map('n', '<ESC>', '<cmd>nohlsearch<CR><Esc>')

map('n', '//', '*')
map('n', '??', '#')

-- I find myself opening files in the same folder a lot
vim.keymap.set('n', '<C-e>',
    function() return string.format(':e ./%s/', vim.fn.expand('%:h')) end,
    { expr = true }
)
vim.keymap.set('c', '<C-e>',
    function() return string.format('| e ./%s/', vim.fn.expand('%:h')) end,
    { expr = true }
)

-- Use relativenumber when in insertmode
vim.cmd [[
 augroup line_numbers
 autocmd InsertEnter * setlocal norelativenumber
 autocmd InsertLeave * setlocal relativenumber
 augroup END
]]

function _G.toggle_relativenumber()
    vim.cmd [[
        if &relativenumber == 1
            setlocal norelativenumber
        else
            setlocal relativenumber
        end
    ]]
end

map('n', '<C-m>', ':lua toggle_relativenumber()<CR>')

-- Save files as sudo when I forget to start vim using sudo
map('c', 'w!!', '!sudo tee > /dev/null')


map('n', '<F12>', '<Esc>:syntax sync fromstart<CR>')
map('i', '<F12>', '<C-o>:syntax sync fromstart<CR>')

-- Jump to start and end of line using home row keys
map('n', 'H', '^')
map('n', 'L', '$')

-- When pasting in visual mode, don't overwrite the buffer
map('v', 'p', '"_dP')

vim.g.copilot_filetypes = {
    xml = false,
    json = false,
    yaml = false,
}
