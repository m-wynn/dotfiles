MAP = vim.api.nvim_set_keymap

local execute = vim.api.nvim_command
local fn = vim.fn

pcall(require("local"))

local install_path = fn.stdpath('data')..'/site/pack/packer/opt/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
    execute('!git clone https://github.com/wbthomason/packer.nvim '..install_path)
end

vim.cmd [[packadd packer.nvim]]

vim.cmd([[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost packages.lua source <afile> | PackerCompile
  augroup end
]])

return require('packer').startup({function()
    use {'wbthomason/packer.nvim',
        opt = true,
    }
    -- lua
    use {'svermeulen/vimpeccable'}
    use {'nvim-lua/plenary.nvim'}

    -- navigation and tools
    use {'airblade/vim-rooter'}
    use {'jreybert/vimagit'}
    use {'kassio/neoterm',
        config = function()
            vim.g.neoterm_default_mod = "botright"
            vim.g.neoterm_size = "20"
            MAP("n", "<leader>tt", "<cmd>T cd $(dirname %:p)/<CR>", {noremap = true})
        end
    }
    use {'tommcdo/vim-fubitive'}
    use {'tpope/vim-eunuch'}
    use {'tpope/vim-fugitive'}
    use {'tpope/vim-rhubarb'}
    -- use {'lotabout/skim.vim',
    --     requires = {{'lotabout/skim', run='./install'}},
    --     config = function()
    --         MAP("n", "<leader>f", [[<Cmd>:Files<CR>]], {noremap = true})
    --         MAP("n", "<leader>s", [[<Cmd>:Rg<CR>]], {noremap = true})
    --         MAP("n", "<leader>b", [[<Cmd>:Buffers<CR>]], {noremap = true})
    --     end
    -- }
    use {
        'nvim-telescope/telescope.nvim',
        requires = { {'nvim-lua/plenary.nvim'} },
        config = function()

            -- vim.cmd [[highlight TelescopeSelection      guifg=#ffffff gui=bold]]
            MAP("n", "<leader>f", [[<cmd>lua require('telescope.builtin').find_files()<cr>]], {noremap = true})
            MAP("n", "<leader>s", [[<cmd>lua require('telescope.builtin').live_grep()<cr>]], {noremap = true})
            MAP("n", "<leader>b", [[<cmd>lua require('telescope.builtin').buffers()<cr>]], {noremap = true})
            MAP("n", "<leader>r", [[<cmd>lua require('telescope.builtin').lsp_references()<cr>]], {noremap = true})
            MAP("n", "<leader>w", [[<cmd>lua require('telescope.builtin').lsp_dynamic_workspace_symbols()<cr>]], {noremap = true})
            MAP("n", "<leader>gs", [[<cmd>lua require('telescope.builtin').git_status()<cr>]], {noremap = true})
            MAP("n", "<leader>gb", [[<cmd>lua require('telescope.builtin').git_branches()<cr>]], {noremap = true})
            MAP("n", "<leader>gc", [[<cmd>lua require('telescope.builtin').git_bcommits()<cr>]], {noremap = true})
            MAP("n", "<leader>a", [[<cmd>lua require('telescope.builtin').lsp_code_actions(require('telescope.themes').get_cursor({}))<cr>]], {noremap = true})
            require('telescope').setup {
                extensions = {
                    frecency = {
                        show_scores = false,
                        show_unindexed = false,
                        ignore_patterns = {"*.git/*", "*/tmp/*", "*/.terraform/*", "*venv/*"},
                        disable_devicons = false,
                        workspaces = WORKSPACES
                    }
                },
            }

        end
    }
    use {'nvim-telescope/telescope-fzf-native.nvim', run = 'make',
        config = function()
            require('telescope').load_extension('fzf')
        end
    }
    use {
        "nvim-telescope/telescope-frecency.nvim",
        config = function()
            require"telescope".load_extension("frecency")
            MAP("n", "<leader><leader>", [[<cmd>lua require('telescope').extensions.frecency.frecency()<cr>]], {noremap = true})

        end,
        requires = {"tami5/sqlite.lua"}
    }

    use {'nvim-telescope/telescope-project.nvim',
        config = function()
            require('telescope').load_extension('project')
            MAP("n", "<leader>p", [[<cmd>lua require('telescope').extensions.project.project{}<cr>]], {noremap = true})
        end
    }
    use {'justinmk/vim-dirvish'}

    -- nerdtree
    use {
        'kyazdani42/nvim-tree.lua',
        requires = {'kyazdani42/nvim-web-devicons'},
        config = function()
            vim.g.nvim_tree_git_hl = 1
            vim.g.nvim_tree_gitignore = 1
            vim.g.nvim_tree_ignore = {'.git', 'node_modules', '.cache', '.terraform'}
            vim.g.nvim_tree_show_icons = {
                git = 1,
                folders = 1,
                files = 1
            }
            require'nvim-tree'.setup {
                disable_netrw = false,
                auto_close = false,
                open_on_setup = false,
                update_to_buf_dir = true,
                lsp_diagnostics = true,
                hijack_netrw = 1,
                indent_markers = 1,
                quit_on_open = false,
                update_cwd = false,
            }
            MAP('n', '<c-n>', '<cmd>NvimTreeToggle<CR><c-w><c-p><cmd>NvimTreeFindFile<CR><c-w><c-p>', {noremap = true, silent = true})
        end
    }

    -- lanugages
    use {'neovim/nvim-lspconfig',
        config = function()
            -- https://github.com/neovim/nvim-lspconfig/blob/master/CONFIG.md
            local sumneko_lua_path = vim.env.HOME .. "/Documents/git/lua-language-server/"

            local servers = {
                terraformls = {},
                rust_analyzer = {},
                intelephense = {},
                pyright = {},
                yamlls = {},
                sumneko_lua = {
                    cmd = {sumneko_lua_path .. "bin/Linux/lua-language-server", "-E", sumneko_lua_path .. "main.lua"},
                    settings = {
                        Lua = {
                            diagnostics = {
                                globals = {"vim", "use"}
                            }
                        }
                    }
                },
            }

            local nvim_lsp = require('lspconfig')
            local on_attach = function(client, bufnr)
                local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
                -- local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

                -- Mappings.
                local opts = { noremap=true, silent=true }
                buf_set_keymap('n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
                buf_set_keymap('n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
                buf_set_keymap('n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
                buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
                buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
                buf_set_keymap('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
                buf_set_keymap('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
                buf_set_keymap('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
                buf_set_keymap('n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
                buf_set_keymap('n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
                buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
                buf_set_keymap('n', '<space>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
                buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
                buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
                buf_set_keymap('n', '<space>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)

                -- Set some keybinds conditional on server capabilities
                if client.resolved_capabilities.document_formatting then
                    buf_set_keymap("n", "<C-f>", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)
                elseif client.resolved_capabilities.document_range_formatting then
                    buf_set_keymap("n", "<C-f>", "<cmd>lua vim.lsp.buf.range_formatting()<CR>", opts)
                end

                -- Set autocommands conditional on server_capabilities
                if client.resolved_capabilities.document_highlight then
                    vim.api.nvim_exec([[
                    hi LspReferenceRead cterm=bold ctermbg=red guibg=LightYellow
                    hi LspReferenceText cterm=bold ctermbg=red guibg=LightYellow
                    hi LspReferenceWrite cterm=bold ctermbg=red guibg=LightYellow
                        ]], false)
                end
            end

            -- -- Use a loop to conveniently both setup defined servers
            -- -- and map buffer local keybindings when the language server attaches
            for lsp_name, lsp_config in pairs(servers) do
                local setup = {on_attach = on_attach}
                for k, v in pairs(lsp_config) do setup[k] = v end
                nvim_lsp[lsp_name].setup(setup)
            end
        end
    }
    use {'nvim-treesitter/playground'}
    use {'nvim-treesitter/nvim-treesitter', run=':TSUpdate',
        config = function()
            require'nvim-treesitter.configs'.setup {
                ensure_installed = "maintained",
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
                    disable = { 'yaml' }
                },
            }
        end
    }
    use {'sheerun/vim-polyglot'}

    -- completion
    use {'nvim-lua/completion-nvim',
        requires = {'steelsojka/completion-buffers', 'nvim-treesitter/completion-treesitter'},
        config = function()
            vim.cmd [[autocmd BufEnter * lua require'completion'.on_attach()]]
            vim.o.completeopt="menuone,noinsert,noselect"
            vim.o.shortmess = vim.o.shortmess .. "c"
            vim.g.completion_enable_snippet = 'UltiSnips'
            vim.g.completion_auto_change_source = 1
            vim.g.completion_trigger_keyword_length = 3
            vim.g.completion_chain_complete_list = {
                default = {
                    { complete_items = { 'snippet', 'lsp', 'ts', 'path', 'buffers' } },
                    { mode = { '<c-p>' } },
                    { mode = { '<c-n>' } }
                },
                TelescopePrompt = {},
                frecency = {},
            }

            local function t(str)
                return vim.api.nvim_replace_termcodes(str, true, true, true)
            end

            function _G.smart_tab()
                return vim.fn.pumvisible() == 1 and t'<C-n>' or t'<Tab>'
            end
            MAP('i', '<Tab>', 'v:lua.smart_tab()', {expr = true, noremap = true})
        end
    }

    -- editing
    use {'AndrewRadev/splitjoin.vim'}
    use {'junegunn/vim-easy-align',
        config = function()
            MAP('x', 'ga', '<Plug>(EasyAlign)', {})
            MAP('n', 'ga', '<Plug>(EasyAlign)', {})
        end
    }
    use {'machakann/vim-sandwich'}
    use {'michaeljsmith/vim-indent-object'}
    use {'rhysd/clever-f.vim',
        config = function()
            vim.g.clever_f_smart_case = 1
        end
    }
    use {'tpope/vim-abolish'}
    use {'tpope/vim-commentary'}
    use {'tpope/vim-ragtag'}
    use {'tpope/vim-repeat'}
    use {'tpope/vim-sleuth'}
    use {'tpope/vim-surround'}
    use {'tpope/vim-unimpaired'}
    use {'wellle/targets.vim'}
    use {'iamcco/markdown-preview.nvim', run='cd app & yarn install'}

    -- snippets
    use {'SirVer/ultisnips',
        config = function()
            vim.g.UltiSnipsExpandTrigger='<c-y>'
            vim.g.UltiSnipsJumpForwardTrigger='<c-b>'
            vim.g.UltiSnipsJumpBackwardTrigger='<c-z>'
            vim.g.UltiSnipsEditSplit='vertical'
            vim.o.runtimepath = vim.o.runtimepath .. ",~/.config/nvim/my-snippets"
        end
    }
    use {'epilande/vim-react-snippets'}
    use {'honza/vim-snippets'}
    use {'juliosueiras/vim-terraform-snippets', run='rm snippets && mkdir snippets && mv terraform snippets/terraform' }
    use {'noahfrederick/vim-skeleton'}

    -- ui
    use {'ap/vim-css-color'}
    use {
      'hoob3rt/lualine.nvim',
      requires = {'kyazdani42/nvim-web-devicons', opt = true},

    config = function()
      require('lualine').setup{
        options = {
          theme = 'gruvbox',

          section_separators = {'', ''},

          component_separators = {'', ''},
          icons_enabled = true,
        },
        sections = {
          lualine_a = { {'mode', upper = true} },
          lualine_b = { {'branch', icon = ''} },
          lualine_c = { {'filename', file_status = true} },
          lualine_x = { 'encoding', 'fileformat', 'filetype' },
          lualine_y = { 'progress' },
          lualine_z = { 'location'  },
        },
        inactive_sections = {
          lualine_a = {  },
          lualine_b = {  },
          lualine_c = { 'filename' },
          lualine_x = { 'location' },
          lualine_y = {  },
          lualine_z = {  }
        },
      }
    end


    }
    use {"ellisonleao/gruvbox.nvim", requires = {"rktjmp/lush.nvim"}}

    use {'romgrk/barbar.nvim',
        requires = {'kyazdani42/nvim-web-devicons', opt = true},
        config = function()
            MAP("n", "]b", [[<Cmd>:BufferNext<CR>]], {noremap = true})
            MAP("n", "[b", [[<Cmd>:BufferPrevious<CR>]], {noremap = true})
        end
    }

    -- use {'andrewstuart/vim-kubernetes'}
end,
config = {
  display = {
    open_fn = function()
      return require('packer.util').float({ border = 'single' })
    end
  }
}})
