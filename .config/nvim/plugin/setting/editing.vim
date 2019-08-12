"" vim-easy-align
xmap ga <Plug>(EasyAlign)|         " Start interactive EasyAlign in visual mode (e.g. ipga)

nmap ga <Plug>(EasyAlign)|         " Start interactive EasyAlign for a motion/text object (e.g. gaip)


"" vim-lua-indent
let g:polyglot_disabled = ['lua', 'latex']
let g:lua_version = 5
let g:lua_subversion = 2


"" clever-f.vim
let g:clever_f_smart_case = 1

function! s:check_back_space() abort "{{{
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~ '\s'
endfunction"}}}
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-y>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ deoplete#manual_complete()


"" ultisnips -- Snippets
let g:UltiSnipsExpandTrigger='<c-y>'
let g:UltiSnipsJumpForwardTrigger='<c-b>'
let g:UltiSnipsJumpBackwardTrigger='<c-z>'

let g:UltiSnipsEditSplit='vertical'             " If you want :UltiSnipsEdit to split your window.
set runtimepath+=~/.config/nvim/my-snippets/
