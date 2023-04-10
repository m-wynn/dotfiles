vim.o.completeopt = "menuone,noinsert,noselect"
vim.o.shortmess = vim.o.shortmess .. "c"

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
                    enable = true,
                    defaultConfig = {
                        indent_style = "space",
                        indent_size = "4",
                    },
                },
                validate = true,
                hover = true,
                completion = true,
            }
        }
    },
    lua_ls = {
        settings = {
            Lua = {
                format = {
                    enable = true,
                },
                runtime = {
                    version = 'LuaJIT',
                },
                diagnostics = {
                    globals = { 'vim' },
                },
                workspace = {
                    library = vim.api.nvim_get_runtime_file("", true),
                },
                telemetry = {
                    enable = false,
                }
            }
        }
    },
}

local nvim_lsp = require('lspconfig')
local opts = { noremap = true, silent = true }
vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float, opts)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, opts)
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
        vim.keymap.set('n', '<leader>do', ':OpenDoc<cr>', bufopts)
    end
    -- Mappings.

    vim.keymap.set("n", '<C-f>', function() vim.lsp.buf.format { async = true } end, bufopts)
    vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
    vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
    vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, bufopts)
    vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, bufopts)
    vim.keymap.set('n', '<leader>wa', vim.lsp.buf.add_workspace_folder, bufopts)
    vim.keymap.set('n', '<leader>wr', vim.lsp.buf.remove_workspace_folder, bufopts)
    vim.keymap.set('n', '<leader>wl', function()
        print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
    end, bufopts)
    vim.keymap.set('n', '<leader>D', vim.lsp.buf.type_definition, bufopts)
    vim.keymap.set('n', '<leader>rn', vim.lsp.buf.rename, bufopts)
    vim.keymap.set('n', 'gr', vim.lsp.buf.references, bufopts)
    vim.keymap.set('n', '<leader>ca', vim.lsp.buf.code_action, bufopts)
    vim.keymap.set('n', '<leader>e', vim.lsp.diagnostic.show_line_diagnostics, bufopts)

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
local capabilities = require('cmp_nvim_lsp').default_capabilities()
for lsp_name, lsp_config in pairs(servers) do
    local setup = { on_attach = on_attach }
    setup['capabilities'] = capabilities
    for k, v in pairs(lsp_config) do setup[k] = v end
    nvim_lsp[lsp_name].setup(setup)
end
