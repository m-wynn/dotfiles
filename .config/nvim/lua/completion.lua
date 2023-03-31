vim.o.completeopt = "menuone,noinsert,noselect"
vim.o.shortmess = vim.o.shortmess .. "c"

vim.g.coq_settings = { auto_start = "shut-up" }

require("coq_3p") {
    { src = "nvimlua", short_name = "nLUA" },
    { src = "copilot", short_name = "COP", accept_key = "<c-f>" },
    -- { src = "vimtex", short_name = "vTEX" },
    { src = "bc", short_name = "MATH", precision = 6 },
    -- {
    --     src = "repl",
    --     sh = "zsh",
    --     shell = { p = "python", n = "node" },
    --     max_lines = 99,
    --     deadline = 500,
    --     unsafe = { "rm", "poweroff", "mv" }
    -- }
}

local function t(str)
    return vim.api.nvim_replace_termcodes(str, true, true, true)
end

function _G.smart_tab()
    return vim.fn.pumvisible() == 1 and t '<C-n>' or t '<Tab>'
end

vim.api.nvim_set_keymap('i', '<Tab>', 'v:lua.smart_tab()', { expr = true, noremap = true })

local servers = {
    gopls = {},
    terraformls = {},
    -- terraform_lsp = {},
    tflint = {},
    rust_analyzer = {},
    intelephense = {},
    pyright = {},
    yamlls = {
        settings = {
            yaml = {
                schemaDownload = {
                    enable = true
                },
                schemas = {
                    [vim.fn.expand("$HOME/Documents/schema/schemas/all.json")] = "*.yaml"
                },
                format = {
                    enable = true
                },
                validate = true,
                hover = true,
                completion = true,
            }
        }
    },
    lua_ls = {},
}

local nvim_lsp = require('lspconfig')
local opts = { noremap = true, silent = true }
vim.keymap.set('n', '<space>e', vim.diagnostic.open_float, opts)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist, opts)
local on_attach = function(client, bufnr)
    local bufopts = { noremap = true, silent = true, buffer = bufnr }
    if client.name == "yamlls" then
        client.server_capabilities.document_formatting = true
    end
    if client.name == "terraformls" then
        require('treesitter-terraform-doc').setup({
            command_name       = "OpenDoc",
            url_opener_command = "!open",
        })
        vim.keymap.set('n', '<space>do', ':OpenDoc<cr>', bufopts)
    end
    -- Mappings.

    vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
    vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
    vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, bufopts)
    vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, bufopts)
    vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, bufopts)
    vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, bufopts)
    vim.keymap.set('n', '<space>wl', function()
        print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
    end, bufopts)
    vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, bufopts)
    vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, bufopts)
    vim.keymap.set('n', 'gr', vim.lsp.buf.references, bufopts)
    vim.keymap.set('n', '<space>ca', vim.lsp.buf.code_action, bufopts)
    -- vim.keymap.set('n', '<space>e', vim.lsp.diagnostic.show_line_diagnostics, bufopts)
    vim.keymap.set("n", '<C-f>', function() vim.lsp.buf.format { async = true } end, bufopts)

    -- Set autocommands conditional on server_capabilities
    if client.server_capabilities.document_highlight then
        vim.api.nvim_exec([[
        hi LspReferenceRead cterm=bold ctermbg=red guibg=LightYellow
        hi LspReferenceText cterm=bold ctermbg=red guibg=LightYellow
        hi LspReferenceWrite cterm=bold ctermbg=red guibg=LightYellow
            ]], false)
    end
end

-- -- Use a loop to conveniently both setup defined servers
-- -- and map buffer local keybindings when the language server attaches
local coq = require "coq"
for lsp_name, lsp_config in pairs(servers) do
    local setup = { on_attach = on_attach }
    for k, v in pairs(coq.lsp_ensure_capabilities()) do setup[k] = v end
    for k, v in pairs(lsp_config) do setup[k] = v end
    nvim_lsp[lsp_name].setup(setup)
end
