"" ale
" let g:ale_completion_enabled = 1
let g:ale_fix_on_save = 1
let g:ale_linter_aliases = {'jsx': ['css', 'javascript', 'javascriptreact'], 'shell': ['sh', 'zsh']}
let g:ale_fixer_aliases = {'jsx': ['css', 'javascript', 'javascriptreact']}
let g:ale_sign_error = '✘'
let g:ale_sign_warning = '⚠'
let g:ale_set_balloons = 1
let g:airline#extensions#ale#enabled = 1
let g:ale_php_langserver_use_global = 1
let g:ale_php_langserver_executable = expand('~/.config/composer/vendor/bin/php-language-server.php')
let g:ale_rust_rustfmt_options = '--edition=2018'

let g:ale_lint_on_text_changed = "never"

lua << EOF
local nvim_lsp = require('lspconfig')
local on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

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
      augroup lsp_document_highlight
        autocmd! * <buffer>
        autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
        autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
      augroup END
    ]], false)
  end
end

-- Use a loop to conveniently both setup defined servers
-- and map buffer local keybindings when the language server attaches
local servers = { "terraformls", "rust_analyzer", "intelephense" }
for _, lsp in ipairs(servers) do
  nvim_lsp[lsp].setup { on_attach = on_attach }
end
EOF


let g:ale_linters = {
      \    'ansible': ['ansible-lint'],
      \    'go': ['gopls'],
      \    'jsx': ['eslint', 'flow-language-server'],
      \    'python': ['flake8'],
      \    'rust': ['analyzer'],
      \    'qml': ['qmllint'],
      \    'shell': ['shellcheck'],
      \    'terraform': ['tflint'],
      \    'vim': ['vint']
      \}

let g:ale_fixers = {
      \   '*': ['remove_trailing_lines', 'trim_whitespace'],
      \   'go': ['gofmt', 'goimports'],
      \   'html': ['prettier'],
      \   'json': ['prettier'],
      \   'jsx': ['prettier'],
      \   'kotlin': ['ktlint'],
      \   'markdown': ['prettier'],
      \   'python': ['black'],
      \   'rust': ['rustfmt'],
      \   'terraform': ['terraform', 'remove_trailing_lines'],
      \   'yaml': ['prettier']
      \}

let g:ale_rust_rls_config = {'rust': {'clippy_preference': 'on', 'all_features': v:true}}

"" vim-fugitive -- All the gits
augroup delete_fugitive_buffers
  autocmd BufReadPost fugitive://* set bufhidden=delete
  "" Delete old git-object buffers when traversing the dag
augroup ENDuto
set autoread

nmap <Leader>gs :Gstatus<CR>
nmap <Leader>gd :Gdiff<CR>
nmap <Leader>gs :Gdiff HEAD~1<CR>
nmap <Leader>gb :Gblame<CR>

" Change vim skeleton file
let g:skeleton_template_dir = expand('~/.config/nvim/templates')

let g:sandwich#recipes = deepcopy(g:sandwich#default_recipes)

"" Neoterm
let g:neoterm_autoscroll=1
let g:neoterm_default_mod='belowright'
let g:neoterm_size=16
tnoremap <Esc><Esc> <C-\><C-n>

"""
" Packages to install
" ansible-lint: pip install ansible-lint
" gopls:        GO111MODULE=on go get golang.org/x/tools/gopls@latest
" langserver:   open 'https://github.com/felixfbecker/php-language-server/blob/4ca2b20e2dd5812d4ee1037a80f0cea4d82f353b/README.md#global-installation'
" prettier:     sudo npm install -g prettier
" pyls:         pip3 install --user 'python-language-server'
" qmllint:      dnf install qt5-qtdeclarative-devel
" rls:          rustup component add rls
" shellcheck:   dnf install shellcheck
" terraform:    open 'https://github.com/hashicorp/terraform/releases'
" tflint:       open 'https://github.com/terraform-linters/tflint/releases'
" vint:         pip install --user vim-vint
" yapf:         pip3 install --user 'yapf'
