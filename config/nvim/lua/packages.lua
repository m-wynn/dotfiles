MAP = vim.api.nvim_set_keymap

pcall(require("local"))

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
vim.opt.rtp:prepend(lazypath)

-- vim.cmd([[
--   augroup lazy_config
--   autocmd!
--   autocmd BufWritePost packages.lua source <afile> | PackerCompile
--   augroup end
-- ]])

require('lazy').setup({
    { "catppuccin/nvim", name = "catppuccin",
        priority = 1000,
        opts = {
            flavour = "mocha", -- latte, frappe, macchiato, mocha
            background = {
                light = "latte",
                dark = "mocha",
            },
            transparent_background = false,
            term_colors = true,
            dim_inactive = {
                enabled = true,
                percentage = 0.05,
            },
            integrations = {
                cmp = true,
                dap = {
                    enabled = false,
                    enable_ui = false,
                },
                gitsigns = true,
                lualine = true,
                mini = true,
                native_lsp = {
                    enabled = true,
                    virtual_text = {
                        errors = { "italic" },
                        hints = { "italic" },
                        warnings = { "italic" },
                        information = { "italic" },
                    },
                    underlines = {
                        errors = { "underline" },
                        hints = { "underline" },
                        warnings = { "underline" },
                        information = { "underline" },
                    },
                },
                notify = true,
                nvimtree = true,
                telescope = true,
                treesitter = true,
                treesitter_context = true,
                lsp_trouble = true,
                which_key = true,
            },
        },
        config = function()
            vim.cmd.colorscheme "catppuccin"
        end
    },
    { "folke/which-key.nvim",
        config = function()
            vim.o.timeout = true
            vim.o.timeoutlen = 300
            require("which-key").setup()
        end,
    },
    {
        "Wansmer/treesj",
        keys = {
            { "gJ", function() require("treesj").join() end, desc = "Join node" },
            { "gS", function() require("treesj").split() end, desc = "Split node" },
        },
        opts = {
            max_join_length = 150,
            use_default_keymaps = false,
        },
    },

    { "folke/noice.nvim",
        dependencies = {
            "MunifTanjim/nui.nvim",
            "rcarriga/nvim-notify",
        },
        opts = {
            lsp = {
                override = {
                    ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
                    ["vim.lsp.util.stylize_markdown"] = true,
                    ["cmp.entry.get_documentation"] = true,
                },
            },
            presets = {
                bottom_search = true, -- use a classic bottom cmdline for search
                command_palette = true, -- position the cmdline and popupmenu together
                long_message_to_split = true, -- long messages will be sent to a split
                inc_rename = false, -- enables an input dialog for inc-rename.nvim
                lsp_doc_border = false, -- add a border to hover docs and signature help
            },
        }
    },
    { 'hrsh7th/nvim-cmp',
        dependencies = {
            'hrsh7th/cmp-buffer',
            'hrsh7th/cmp-calc',
            'hrsh7th/cmp-cmdline',
            'hrsh7th/cmp-copilot',
            'hrsh7th/cmp-nvim-lua',
            'hrsh7th/cmp-nvim-lsp',
            'hrsh7th/cmp-nvim-lsp-document-symbol',
            'hrsh7th/cmp-nvim-lsp-signature-help',
            'hrsh7th/cmp-path',
            'andersevenrud/cmp-tmux',
            'saadparwaiz1/cmp_luasnip',
        },
        config = function()
            local cmp = require('cmp')
            local has_words_before = function()
                unpack = unpack or table.unpack
                local line, col = unpack(vim.api.nvim_win_get_cursor(0))
                return col ~= 0 and
                    vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match('%s') == nil
            end
            cmp.setup({
                window = {
                    completion = cmp.config.window.bordered(),
                    documentation = cmp.config.window.bordered(),
                },
                snippet = {
                    expand = function(args)
                        require 'luasnip'.lsp_expand(args.body)
                    end
                },
                mapping = cmp.mapping.preset.insert({
                    ['<C-b>'] = cmp.mapping.scroll_docs(-4),
                    ['<C-g>'] = cmp.mapping.scroll_docs(4),
                    ['<C-leader>'] = cmp.mapping.complete(),
                    ['<C-e>'] = cmp.mapping.abort(),
                    ['<CR>'] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
                    ['<Tab>'] = function(fallback)
                        if not cmp.select_next_item() then
                            if vim.bo.buftype ~= 'prompt' and has_words_before() then
                                cmp.complete()
                            else
                                fallback()
                            end
                        end
                    end,
                    ['<S-Tab>'] = function(fallback)
                        if not cmp.select_prev_item() then
                            if vim.bo.buftype ~= 'prompt' and has_words_before() then
                                cmp.complete()
                            else
                                fallback()
                            end
                        end
                    end,
                }),
                sources = cmp.config.sources({
                    { name = 'nvim_lsp' },
                    { name = 'nvim_lua' },
                    { name = 'calc' },
                    { name = 'nvim_lsp_signature_help' },
                    { name = 'copilot' },
                    { name = 'tmux' },
                    { name = 'luasnip' },
                }, {
                    { name = 'buffer' },
                }),

            })
            cmp.setup.cmdline({ '/', '?' }, {
                mapping = cmp.mapping.preset.cmdline(),
                sources = cmp.config.sources({
                    { name = 'nvim_lsp_document_symbol' },
                }, {
                    { name = 'buffer' }
                })
            })
            cmp.setup.cmdline(':', {
                mapping = cmp.mapping.preset.cmdline(),
                sources = cmp.config.sources({
                    { name = 'path' }
                }, {
                    {
                        name = 'cmdline',
                        option = {
                            ignore_cmds = { 'Man', '!' }
                        }
                    }
                })
            })
        end
    },
    {
        'echasnovski/mini.indentscope',
        config = function()
            require('mini.indentscope').setup()
        end
    },
    -- {
    --     'echasnovski/mini.animate',
    --     config = function()
    --         require('mini.animate').setup()
    --     end
    -- },
    { 'svermeulen/vimpeccable' },

    -- navigation and tools
    { 'airblade/vim-rooter' },
    { 'jreybert/vimagit' },
    { 'akinsho/toggleterm.nvim',
        keys = {
            { "<leader>tt", function() require('toggleterm').exec("cd $(dirname " .. vim.fn.expand("%:p") .. ")/") end },
        },
        opts = {
            size = 120,
            shade_terminals = false,
            direction = 'vertical'
        }
    },
    { 'tommcdo/vim-fubitive' },
    { 'tpope/vim-eunuch' },
    { 'tpope/vim-fugitive' },
    { 'tpope/vim-rhubarb' },
    {
        'nvim-telescope/telescope.nvim',
        dependencies = { { 'nvim-lua/plenary.nvim' } },
        keys = {
            { "<leader>f", function() require('telescope.builtin').find_files() end, desc = "Telesope files" },
            { "<leader>s", function() require('telescope.builtin').live_grep() end },
            { "<leader>b", function() require('telescope.builtin').buffers() end },
            { "<leader>rr", function() require('telescope.builtin').lsp_references() end },
            { "<leader>ws", function() require('telescope.builtin').lsp_dynamic_workspace_symbols() end },
            { "<leader>gs", function() require('telescope.builtin').git_status() end },
            { "<leader>gb", function() require('telescope.builtin').git_branches() end },
            { "<leader>gc", function() require('telescope.builtin').git_bcommits() end },
            { "<leader>a",
                function() require('telescope.builtin').lsp_code_actions(require('telescope.themes').get_cursor({})) end
            },
            { "<leader><leader>", function() require('telescope').extensions.frecency.frecency() end }
        },
        config = function()
            local trouble = require("trouble.providers.telescope")
            require "telescope".load_extension("frecency")
            require('telescope').setup {
                defaults = {
                    mappings = {
                        i = { ["<c-x>"] = trouble.open_with_trouble },
                        n = { ["<c-x>"] = trouble.open_with_trouble },
                    },
                },
                extensions = {
                    frecency = {
                        show_scores = false,
                        show_unindexed = false,
                        ignore_patterns = { "*.git/*", "*/tmp/*", "*/.terraform/*", "*venv/*" },
                        disable_devicons = false,
                        workleaders = WORKSPACES
                    },
                },
            }
        end
    },
    {'nvim-telescope/telescope-fzf-native.nvim', 
        dependencies = { 'nvim-telescope/telescope.nvim' },
        build = 'cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build',
        config = function()
            require('telescope').load_extension('fzf')
        end
    },
    {
        "nvim-telescope/telescope-frecency.nvim",
        dependencies = { "tami5/sqlite.lua" },
    },

    -- { 'nvim-telescope/telescope-project.nvim',
    --     config = function()
    --         require('telescope').load_extension('project')
    --         MAP("n", "<leader>p", [[<cmd>lua require('telescope').extensions.project.project{}<cr>]],
    --             { noremap = true })
    --     end
    -- },
    { 'justinmk/vim-dirvish' },

    -- nvimtree
    {
        'kyazdani42/nvim-tree.lua',
        dependencies = { 'kyazdani42/nvim-web-devicons' },
        config = function()
            require 'nvim-tree'.setup {
                disable_netrw = false,
                open_on_setup = false,
                diagnostics = {
                    enable = false,
                    icons = {
                        hint = "",
                        info = "",
                        warning = "",
                        error = "",
                    },
                },
                update_cwd = false,
                renderer = {
                    highlight_git = true,
                    icons = {
                        show = {
                            git = true,
                            folder = true,
                            file = true
                        },
                    },
                },
            }
            MAP('n', '<c-n>', '<cmd>NvimTreeToggle<CR><c-w><c-p><cmd>NvimTreeFindFile<CR><c-w><c-p>',
                { noremap = true, silent = true })
        end
    },
    {
        "folke/trouble.nvim",
        dependencies = "kyazdani42/nvim-web-devicons",
        config = function()
            require("trouble").setup {
                use_lsp_diagnostic_signs = false,
                auto_close = true,
            }
            vim.api.nvim_set_keymap("n", "<leader>xx", "<cmd>Trouble<cr>",
                { silent = true, noremap = true }
            )
        end
    },
    -- lanugages
    { "jose-elias-alvarez/null-ls.nvim",
        dependencies = { "nvim-lua/plenary.nvim", "neovim/nvim-lspconfig" },
        config = function()
            local null_ls = require("null-ls")
            local sources = {
                null_ls.builtins.formatting.eslint_d,
                null_ls.builtins.formatting.fixjson,
                null_ls.builtins.formatting.nginx_beautifier,
                null_ls.builtins.formatting.phpcbf,
                -- null_ls.builtins.formatting.prettierd,
                null_ls.builtins.formatting.shellharden,
                null_ls.builtins.formatting.sqlformat,
                null_ls.builtins.formatting.terraform_fmt,
                null_ls.builtins.formatting.phpcsfixer,
                null_ls.builtins.formatting.isort,
                null_ls.builtins.formatting.black,
                null_ls.builtins.diagnostics.hadolint,
                null_ls.builtins.diagnostics.shellcheck,
                null_ls.builtins.diagnostics.flake8
            }

            null_ls.setup({ sources = sources, debug = false })
        end,
    },

    { 'neovim/nvim-lspconfig', dependencies = { "Afourcat/treesitter-terraform-doc.nvim", "towolf/vim-helm", "hrsh7th/cmp-nvim-lsp" },
        config = function()
            vim.o.completeopt = "menuone,noinsert,noselect"
            vim.o.shortmess = vim.o.shortmess .. "c"

            local configs = require('lspconfig.configs')
            local util = require('lspconfig.util')
            if not configs.helm_ls then
                configs.helm_ls = {
                    default_config = {
                        cmd = { "helm_ls", "serve" },
                        filetypes = { 'helm' },
                        root_dir = function(fname)
                            return util.root_pattern('Chart.yaml')(fname)
                        end,
                    },
                }
            end
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
                helm_ls = {
                    filetypes = { "helm" },
                }
            }

            local nvim_lsp = require('lspconfig')
            local opts = { noremap = true, silent = true }
            vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float, opts)
            vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
            vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
            vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, opts)
            local on_attach = function(client, bufnr)
                if vim.bo[bufnr].filetype == "helm" and client.name == "yamlls" then
                    vim.defer_fn(function()
                        local clients = vim.lsp.get_active_clients()
                        for client_id, c in pairs(clients) do
                            if c.name == "yamlls" then
                                vim.lsp.buf_detach_client(0, client_id)
                            end
                        end
                    end, 1000)
                    goto continue
                end
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
                ::continue::
            end

            -- -- Use a loop to conveniently both setup defined servers
            local capabilities = require('cmp_nvim_lsp').default_capabilities()
            for lsp_name, lsp_config in pairs(servers) do
                local setup = { on_attach = on_attach }
                setup['capabilities'] = capabilities
                for k, v in pairs(lsp_config) do setup[k] = v end
                nvim_lsp[lsp_name].setup(setup)
            end
        end
    },
    { 'nvim-treesitter/playground', lazy = true },
    { 'nvim-treesitter/nvim-treesitter', build = ':TSUpdate',
        config = function()
            require("nvim-treesitter.configs").setup {
                ensure_installed = {
                    "bash",
                    "c",
                    "cmake",
                    "comment",
                    "cpp",
                    "css",
                    "devicetree",
                    "dockerfile",
                    "gitattributes",
                    "gitignore",
                    "go",
                    "hcl",
                    "hjson",
                    "hocon",
                    "html",
                    "http",
                    "java",
                    "javascript",
                    "jsdoc",
                    "json",
                    "json5",
                    "jsonc",
                    "kotlin",
                    "latex",
                    "llvm",
                    "lua",
                    "make",
                    "markdown",
                    "markdown_inline",
                    "nix",
                    "php",
                    "phpdoc",
                    "python",
                    "regex",
                    "ruby",
                    "rust",
                    "scss",
                    "sql",
                    "terraform",
                    "tsx",
                    "typescript",
                    "vim",
                    "vimdoc",
                    "yaml",
                },
                ignore_install = { "phpdoc" },
                highlight = {
                    enable = true,
                },
                incremental_selection = {
                    enable = true,
                    keymaps = {
                        init_selection = "gnn",
                        node_incremental = "grn",
                        scope_incremental = "grc",
                        node_decremental = "grm",
                    },
                },
                indent = {
                    enable = true,
                    disable = { 'yaml' },
                },
            }
        end
    },

    -- todo: replace with https://github.com/echasnovski/mini.nvim/blob/main/readmes/mini-align.md maybe?
    { 'junegunn/vim-easy-align',
        config = function()
            MAP('x', 'ga', '<Plug>(EasyAlign)', {})
            MAP('n', 'ga', '<Plug>(EasyAlign)', {})
        end
    },
    { 'machakann/vim-sandwich' },
    { 'michaeljsmith/vim-indent-object' },
    -- {'rhysd/clever-f.vim',
    --     config = function()
    --         vim.g.clever_f_smart_case = 1
    --     end
    -- },
    { 'tpope/vim-abolish' },
    { 'tpope/vim-commentary' },
    { 'tpope/vim-ragtag' },
    { 'tpope/vim-repeat' },
    { 'tpope/vim-sleuth' },
    { 'tpope/vim-surround' },
    { 'tpope/vim-unimpaired' },
    { 'wellle/targets.vim' },
    { 'iamcco/markdown-preview.nvim', build = 'cd app & yarn install' },

    -- snippets
    {
        "L3MON4D3/LuaSnip",
        version = "1.*",
        build = "make install_jsregexp"
    },
    { 'epilande/vim-react-snippets' },
    { 'honza/vim-snippets' },
    { 'juliosueiras/vim-terraform-snippets',
        build = 'rm -fr coq-user-snippets; mkdir coq-user-snippets && cat terraform/* >> coq-user-snippets/terraform.snip' },
    { 'noahfrederick/vim-skeleton' },

    -- ui
    { 'ap/vim-css-color' },
    {
        'hoob3rt/lualine.nvim',
        dependencies = { 'kyazdani42/nvim-web-devicons', lazy = true },

        opts = {
            options = {
                theme = 'catppuccin',
                section_separators = { '', '' },
                component_separators = { '', '' },
                icons_enabled = true,
            },
            sections = {
                lualine_a = { { 'mode', upper = true } },
                lualine_b = { { 'branch', icon = '' } },
                lualine_c = { { 'filename', file_status = true } },
                lualine_x = { 'encoding', 'fileformat', 'filetype' },
                lualine_y = { 'progress' },
                lualine_z = { 'location' },
            },
            inactive_sections = {
                lualine_a = {},
                lualine_b = {},
                lualine_c = { 'filename' },
                lualine_x = { 'location' },
                lualine_y = {},
                lualine_z = {},
            },
        }
    },
    { 'romgrk/barbar.nvim',
        dependencies = { 'kyazdani42/nvim-web-devicons', lazy = true },
        config = function()
            MAP("n", "]b", [[<Cmd>:BufferNext<CR>]], { noremap = true })
            MAP("n", "[b", [[<Cmd>:BufferPrevious<CR>]], { noremap = true })
        end
    },

    -- { "unblevable/quick-scope",
    --     config = function()
    --         vim.g.qs_highlight_on_keys = { 'f', 'F', 't', 'T' }
    --     end
    -- },
    { "mfussenegger/nvim-dap",
        config = function()
            local dap = require('dap')
            dap.adapters.php = {
                type = 'executable',
                command = 'node',
                args = { '/Users/matthew/Documents/git/vscode-php-debug/out/phpDebug.js' },
            }

            dap.configurations.php = {
                {
                    type = 'php',
                    request = 'launch',
                    name = 'Listen for Xdebug',
                    port = 9000,
                    pathMappings = {
                        ['/srv/www/multisite'] = '/Users/matthew/Documents/git/multisite/multisite',
                        ['/srv/www/private'] = '/Users/matthew/Documents/git/multisite/private',
                        ['/srv/www/public'] = '/Users/matthew/Documents/git/multisite/public',
                        ['/srv/www/public/index.php'] = '/Users/matthew/Documents/git/multisite/public/index.php',
                    },
                    xdebugSettings = {
                        max_data = -1
                    },
                },
            }
            function dapsidebars(str)
                local widgets = require('dap.ui.widgets')
                local scopes = widgets.sidebar(widgets.scopes)
                local frames = widgets.sidebar(widgets.frames)
                scopes.open()
                frames.open()
            end

            MAP("n", "<leader>ec", [[<cmd>lua require('dap').continue()<cr>]], { noremap = true })
            MAP("n", "<leader>es", [[<cmd>lua require('dap').step_over()<cr>]], { noremap = true })
            MAP("n", "<leader>ei", [[<cmd>lua require('dap').step_into()<cr>]], { noremap = true })
            MAP("n", "<leader>eo", [[<cmd>lua require('dap').step_out()<cr>]], { noremap = true })
            MAP("n", "<leader>eb", [[<cmd>lua require('dap').toggle_breakpoint()<cr>]], { noremap = true })
            MAP("n", "<leader>er", [[<cmd>lua require('dap').repl_open()<cr>]], { noremap = true })
            MAP("n", "<leader>el", [[<cmd>lua require('dap').run_last()<cr>]], { noremap = true })
            MAP("n", "<leader>eh", [[<cmd>lua require('dap.ui.widgets').hover()<cr>]], { noremap = true })
            MAP("n", "<leader>ef", [[<cmd>lua require('dap.ui.widgets').hover()<cr>]], { noremap = true })
        end
    },
    { 'theHamsta/nvim-dap-virtual-text',
        opts = {
            enabled = true,
            enabled_commands = true,
            highlight_changed_variables = true,
            highlight_new_as_changed = true,
            show_stop_reason = true,
            commented = true,
            virt_text_pos = 'eol',
            all_frames = false,
            virt_lines = false,
            virt_text_win_col = nil
        }
    },
    { "simrat39/rust-tools.nvim",
        dependencies = { "nvim-lua/popup.nvim", "nvim-lua/plenary.nvim", "mfussenegger/nvim-dap" },
        config = function()
            require('rust-tools').setup({})
        end
    },

    { "towolf/vim-helm",
        priority = 1000,
    },
    { "github/copilot.vim",
        config = function()
            vim.keymap.set('i', '<C-j>', 'copilot#Accept("\\<CR>")', { silent = true, script = true, expr = true })
            vim.g.copilot_no_tab_map = true
            vim.g.copilot_filetypes = {
                xml = false,
                json = false,
                yaml = false,
            }
        end
    },

    {
        "cuducos/yaml.nvim",
        ft = "yaml",
        dependencies = {
            "nvim-treesitter/nvim-treesitter",
            "nvim-telescope/telescope.nvim"
        },
    },
})
