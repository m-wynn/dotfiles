"" ale
" let g:ale_completion_enabled = 1
let g:ale_fix_on_save = 1
let g:ale_linter_aliases = {'jsx': ['css', 'javascript']}
let g:ale_fixer_aliases = {'jsx': ['css', 'javascript']}
let g:ale_sign_error = '✘'
let g:ale_sign_warning = '⚠'
let g:ale_set_balloons = 1
let g:airline#extensions#ale#enabled = 1

nnoremap gd :ALEGoToDefinition<cr>
nnoremap K :ALEHover<cr>

let g:ale_linters = {
      \    'rust': ['cargo', 'rls'],
      \    'go': ['gopls'],
      \    'jsx': ['eslint', 'flow-language-server'],
      \    'python': ['flake8', 'pyls'],
      \}
let g:ale_fixers = {
      \   '*': ['remove_trailing_lines', 'trim_whitespace'],
      \   'rust': ['rustfmt'],
      \   'go': ['gofmt', 'goimports'],
      \   'kotlin': ['ktlint'],
      \   'jsx': ['prettier_eslint'],
      \   'python': ['yapf'],
      \}

"" ctrlp
let g:ctrlp_user_command = 'rg %s --files --color=never --glob ""'
let g:ctrlp_use_caching = 0
let g:ctrlp_match_window_reversed = 0


"" vim-better-whitespace
let g:better_whitespace_filetypes_blacklist=['mail', 'diff', 'gitcommit', 'unite', 'qf', 'help']



"" vim-fugitive -- All the gits
augroup delete_fugitive_buffers
  autocmd BufReadPost fugitive://* set bufhidden=delete
  "" Delete old git-object buffers when traversing the dag
augroup END
