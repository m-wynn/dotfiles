MAP = vim.api.nvim_set_keymap

local execute = vim.api.nvim_command
local fn = vim.fn

pcall(require("local"))

local install_path = fn.stdpath('data') .. '/site/pack/packer/opt/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
    execute('!git clone https://github.com/wbthomason/packer.nvim ' .. install_path)
end

vim.cmd [[packadd packer.nvim]]

vim.cmd([[
  augroup packer_user_config
  autocmd!
  autocmd BufWritePost packages.lua source <afile> | PackerCompile
  augroup end
]])

return require('packer').startup({ function()
    use { 'wbthomason/packer.nvim',
        opt = true,
    }
    -- lua
    use { 'svermeulen/vimpeccable' }
    use { 'nvim-lua/plenary.nvim' }

    -- navigation and tools
    use { 'airblade/vim-rooter' }
    use { 'jreybert/vimagit' }
    use { 'kassio/neoterm',
        config = function()
            vim.g.neoterm_default_mod = "vertical"
            vim.g.neoterm_size = "120"
            MAP("n", "<leader>tt", "<cmd>T cd $(dirname %:p)/<CR>", { noremap = true })
        end
    }
    use { 'tommcdo/vim-fubitive' }
    use { 'tpope/vim-eunuch' }
    use { 'tpope/vim-fugitive' }
    use { 'tpope/vim-rhubarb' }
    use {
        'nvim-telescope/telescope.nvim',
        requires = { { 'nvim-lua/plenary.nvim' } },
        config = function()
            MAP("n", "<leader>f", [[<cmd>lua require('telescope.builtin').find_files()<cr>]], { noremap = true })
            MAP("n", "<leader>s", [[<cmd>lua require('telescope.builtin').live_grep()<cr>]], { noremap = true })
            MAP("n", "<leader>b", [[<cmd>lua require('telescope.builtin').buffers()<cr>]], { noremap = true })
            MAP("n", "<leader>r", [[<cmd>lua require('telescope.builtin').lsp_references()<cr>]], { noremap = true })
            MAP("n", "<leader>w", [[<cmd>lua require('telescope.builtin').lsp_dynamic_workspace_symbols()<cr>]], { noremap = true })
            MAP("n", "<leader>gs", [[<cmd>lua require('telescope.builtin').git_status()<cr>]], { noremap = true })
            MAP("n", "<leader>gb", [[<cmd>lua require('telescope.builtin').git_branches()<cr>]], { noremap = true })
            MAP("n", "<leader>gc", [[<cmd>lua require('telescope.builtin').git_bcommits()<cr>]], { noremap = true })
            MAP("n", "<leader>a", [[<cmd>lua require('telescope.builtin').lsp_code_actions(require('telescope.themes').get_cursor({}))<cr>]], { noremap = true })
            local trouble = require("trouble.providers.telescope")
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
                        workspaces = WORKSPACES
                    }
                },
            }
            require "telescope".load_extension("frecency")
            MAP("n", "<leader><leader>", [[<cmd>lua require('telescope').extensions.frecency.frecency()<cr>]], { noremap = true })
        end
    }
    use {'nvim-telescope/telescope-fzf-native.nvim', run = 'cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build',
        config = function()
            require('telescope').load_extension('fzf')
        end
    }
    use {
        "nvim-telescope/telescope-frecency.nvim",
        config = function()
            require "telescope".load_extension("frecency")
            MAP("n", "<leader><leader>", [[<cmd>lua require('telescope').extensions.frecency.frecency()<cr>]], { noremap = true })

        end,
        requires = { "tami5/sqlite.lua" }
    }

    use { 'nvim-telescope/telescope-project.nvim',
        config = function()
            require('telescope').load_extension('project')
            MAP("n", "<leader>p", [[<cmd>lua require('telescope').extensions.project.project{}<cr>]], { noremap = true })
        end
    }
    use { 'justinmk/vim-dirvish' }

    -- nvimtree
    use {
        'kyazdani42/nvim-tree.lua',
        requires = { 'kyazdani42/nvim-web-devicons' },
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
                    }
                },
                update_cwd = false,
                renderer = {
                    highlight_git = true,
                    icons = {
                        show = {
                            git = true,
                            folder = true,
                            file = true
                        }
                    }
                }
            }
            MAP('n', '<c-n>', '<cmd>NvimTreeToggle<CR><c-w><c-p><cmd>NvimTreeFindFile<CR><c-w><c-p>', { noremap = true, silent = true })
        end
    }
    use {
        "folke/trouble.nvim",
        requires = "kyazdani42/nvim-web-devicons",
        config = function()
            require("trouble").setup {
                use_lsp_diagnostic_signs = false,
                auto_close = true,
            }
            vim.api.nvim_set_keymap("n", "<leader>xx", "<cmd>Trouble<cr>",
                { silent = true, noremap = true }
            )
        end
    }

    -- lanugages
    use { "jose-elias-alvarez/null-ls.nvim",
        requires = { "nvim-lua/plenary.nvim", "neovim/nvim-lspconfig" },
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
    }

    -- completion
    use { 'ms-jpq/coq_nvim',
        requires = { 'ms-jpq/coq.artifacts', 'ms-jpq/coq.thirdparty' },
    }
    use { 'neovim/nvim-lspconfig', requires = { "Afourcat/treesitter-terraform-doc.nvim" } }
    use { 'nvim-treesitter/playground' }
    use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate',
        config = function()
            require 'nvim-treesitter.configs'.setup {
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
                    "help",
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
                    disable = { 'yaml' }
                },
            }
        end
    }
    use { 'sheerun/vim-polyglot' }


    -- editing
    use { 'AndrewRadev/splitjoin.vim' }
    use { 'junegunn/vim-easy-align',
        config = function()
            MAP('x', 'ga', '<Plug>(EasyAlign)', {})
            MAP('n', 'ga', '<Plug>(EasyAlign)', {})
        end
    }
    use { 'machakann/vim-sandwich' }
    use { 'michaeljsmith/vim-indent-object' }
    -- use {'rhysd/clever-f.vim',
    --     config = function()
    --         vim.g.clever_f_smart_case = 1
    --     end
    -- }
    use { 'tpope/vim-abolish' }
    use { 'tpope/vim-commentary' }
    use { 'tpope/vim-ragtag' }
    use { 'tpope/vim-repeat' }
    use { 'tpope/vim-sleuth' }
    use { 'tpope/vim-surround' }
    use { 'tpope/vim-unimpaired' }
    use { 'wellle/targets.vim' }
    use { 'iamcco/markdown-preview.nvim', run = 'cd app & yarn install' }

    use { 'gelguy/wilder.nvim',
        requires = { "roxma/nvim-yarp", "romgrk/fzy-lua-native" },
        run = ':UpdateRemotePlugins',
        config = function()
            vim.cmd [[
                call wilder#setup({'modes': [':', '/', '?']})

                call wilder#set_option('pipeline', [
                \   wilder#branch(
                \     wilder#python_file_finder_pipeline({
                \       'file_command': {_, arg -> stridx(arg, '.') != -1 ? ['fd', '-tf', '-H'] : ['fd', '-tf']},
                \       'dir_command': ['fd', '-td'],
                \     }),
                \     wilder#substitute_pipeline({
                \       'pipeline': wilder#python_search_pipeline({
                \         'skip_cmdtype_check': 1,
                \         'pattern': wilder#python_fuzzy_pattern({
                \           'start_at_boundary': 0,
                \         }),
                \       }),
                \     }),
                \     wilder#cmdline_pipeline({
                \       'fuzzy': 1,
                \       'fuzzy_filter': has('nvim') ? wilder#lua_fzy_filter() : wilder#vim_fuzzy_filter(),
                \     }),
                \     [
                \       wilder#check({_, x -> empty(x)}),
                \       wilder#history(),
                \     ],
                \     wilder#python_search_pipeline({
                \       'pattern': wilder#python_fuzzy_pattern({
                \         'start_at_boundary': 0,
                \       }),
                \     }),
                \   ),
                \ ])

                let s:highlighters = [
                \ wilder#lua_fzy_highlighter() 
                \ ]

                let s:popupmenu_renderer = wilder#popupmenu_renderer(wilder#popupmenu_border_theme({
                \ 'border': 'rounded',
                \ 'empty_message': wilder#popupmenu_empty_message_with_spinner(),
                \ 'highlighter': s:highlighters,
                \ 'left': [
                \   ' ',
                \   wilder#popupmenu_devicons(),
                \   wilder#popupmenu_buffer_flags({
                \     'flags': ' a + ',
                \     'icons': {'+': '', 'a': '', 'h': ''},
                \   }),
                \ ],
                \ 'right': [
                \   ' ',
                \   wilder#popupmenu_scrollbar(),
                \ ],
                \ }))

                let s:wildmenu_renderer = wilder#wildmenu_renderer({
                \ 'highlighter': s:highlighters,
                \ 'separator': ' · ',
                \ 'left': [' ', wilder#wildmenu_spinner(), ' '],
                \ 'right': [' ', wilder#wildmenu_index()],
                \ })

                call wilder#set_option('renderer', wilder#renderer_mux({
                \ ':': s:popupmenu_renderer,
                \ '/': s:wildmenu_renderer,
                \ 'substitute': s:wildmenu_renderer,
                \ }))
                ]]
        end
    }

    -- snippets
    use { 'epilande/vim-react-snippets' }
    use { 'honza/vim-snippets' }
    use { 'juliosueiras/vim-terraform-snippets', run = 'rm -fr coq-user-snippets; mkdir coq-user-snippets && cat terraform/* >> coq-user-snippets/terraform.snip' }
    use { 'noahfrederick/vim-skeleton' }

    -- ui
    use { 'ap/vim-css-color' }
    use {
        'hoob3rt/lualine.nvim',
        requires = { 'kyazdani42/nvim-web-devicons', opt = true },

        config = function()
            require('lualine').setup {
                options = {
                    theme = 'gruvbox',

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
                    lualine_z = {}
                },
            }
        end


    }
    use { "ellisonleao/gruvbox.nvim",
        requires = { "rktjmp/lush.nvim" },
        config = function()
            vim.cmd [[colorscheme gruvbox]]
        end
    }

    use { 'romgrk/barbar.nvim',
        requires = { 'kyazdani42/nvim-web-devicons', opt = true },
        config = function()
            MAP("n", "]b", [[<Cmd>:BufferNext<CR>]], { noremap = true })
            MAP("n", "[b", [[<Cmd>:BufferPrevious<CR>]], { noremap = true })
        end
    }

    use { "unblevable/quick-scope",
        config = function()
            vim.g.qs_highlight_on_keys = { 'f', 'F', 't', 'T' }
        end
    }
    use { "mfussenegger/nvim-dap",
        config = function()
            local dap = require('dap')
            dap.adapters.php = {
                type = 'executable',
                command = 'node',
                args = { '/Users/matthew/Documents/git/vscode-php-debug/out/phpDebug.js' }
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
                }
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
    }
    use { 'theHamsta/nvim-dap-virtual-text',
        config = function()
            require("nvim-dap-virtual-text").setup {
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
        end
    }
    use { "simrat39/rust-tools.nvim",
        requires = { "nvim-lua/popup.nvim", "nvim-lua/plenary.nvim", "mfussenegger/nvim-dap" },
        config = function()
            require('rust-tools').setup({})
        end
    }

    use { "towolf/vim-helm" }
    use { "github/copilot.vim" }

    use {
      "cuducos/yaml.nvim",
      ft = {"yaml"},
      requires = {
        "nvim-treesitter/nvim-treesitter",
        "nvim-telescope/telescope.nvim"
      },
    }
end,
config = {
    display = {
        open_fn = function()
            return require('packer.util').float({ border = 'single' })
        end
    }
}
})
