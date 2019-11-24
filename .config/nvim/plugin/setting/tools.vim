"" ale
" let g:ale_completion_enabled = 1
let g:ale_fix_on_save = 1
let g:ale_linter_aliases = {'jsx': ['css', 'javascript'], 'shell': ['sh', 'zsh']}
let g:ale_fixer_aliases = {'jsx': ['css', 'javascript']}
let g:ale_sign_error = '✘'
let g:ale_sign_warning = '⚠'
let g:ale_set_balloons = 1
let g:airline#extensions#ale#enabled = 1
let g:ale_php_langserver_use_global = 1
let g:ale_php_langserver_executable = expand('~/.config/composer/vendor/bin/php-language-server.php')


nnoremap gd :ALEGoToDefinition<cr>
nnoremap K :ALEHover<cr>

let g:ale_linters = {
      \    'ansible': ['ansible-lint'],
      \    'go': ['gopls'],
      \    'web': ['eslint', 'flow-language-server'],
      \    'php': ['langserver'],
      \    'python': ['flake8', 'pyls'],
      \    'rust': ['rls'],
      \    'qml': ['qmllint'],
      \    'shell': ['shellcheck'],
      \    'vim': ['vint'],
      \    'terraform': ['tflint'],
      \}

let g:ale_fixers = {
      \   '*': ['remove_trailing_lines', 'trim_whitespace'],
      \   'go': ['gofmt', 'goimports'],
      \   'json': ['prettier'],
      \   'html': ['prettier'],
      \   'markdown': ['prettier'],
      \   'web': ['prettier_eslint'],
      \   'kotlin': ['ktlint'],
      \   'python': ['yapf'],
      \   'rust': ['rustfmt'],
      \   'terraform': ['terraform'],
      \   'yaml': ['prettier']
      \}

let g:ale_rust_rls_config = {'rust': {'clippy_preference': 'on', 'all_features': v:true}}

"" vim-fugitive -- All the gits
augroup delete_fugitive_buffers
  autocmd BufReadPost fugitive://* set bufhidden=delete
  "" Delete old git-object buffers when traversing the dag
augroup END

" Change vim skeleton file
let g:skeleton_template_dir = expand('~/.config/nvim/templates')

let g:sandwich#recipes = deepcopy(g:sandwich#default_recipes)


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
