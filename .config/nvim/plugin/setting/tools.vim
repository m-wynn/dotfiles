"" neomake
augroup neomake_on_save
  au!
  autocmd BufWritePost * Neomake
augroup END

let g:neomake_python_enabled_makers = ['flake8']


"" ctrlp
let g:ctrlp_user_command = 'rg %s --files --color=never --glob ""'
let g:ctrlp_use_caching = 0
let g:ctrlp_match_window_reversed = 0


"" vim-better-whitespace
let g:better_whitespace_filetypes_blacklist=['mail', 'diff', 'gitcommit', 'unite', 'qf', 'help']


"" nerdtree
function! NERDTreeRefresh()
  if &filetype ==# 'nerdtree'
    silent exe substitute(mapcheck('R'), '<CR>', '', '')
  endif
endfunction

augroup nerdtree
  autocmd BufEnter * call NERDTreeRefresh()
augroup end

let g:NERDTreeUpdateOnCursorHold = 0


"" vim-fugitive -- All the gits
augroup delete_fugitive_buffers
  autocmd BufReadPost fugitive://* set bufhidden=delete
  "" Delete old git-object buffers when traversing the dag
augroup END


