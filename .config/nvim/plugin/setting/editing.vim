"" vim-easy-align
xmap ga <Plug>(EasyAlign)|         " Start interactive EasyAlign in visual mode (e.g. ipga)

nmap ga <Plug>(EasyAlign)|         " Start interactive EasyAlign for a motion/text object (e.g. gaip)


"" vim-lua-indent
let g:polyglot_disabled = ['lua']
let g:lua_version = 5
let g:lua_subversion = 2


"" clever-f.vim
let g:clever_f_smart_case = 1


"" ultisnips -- Snippets
let g:UltiSnipsExpandTrigger='<tab>'
let g:UltiSnipsJumpForwardTrigger='<c-b>'
let g:UltiSnipsJumpBackwardTrigger='<c-z>'

let g:UltiSnipsEditSplit='vertical'             " If you want :UltiSnipsEdit to split your window.
set runtimepath+=~/.config/nvim/my-snippets/

"" vim-latex/vim-latex -- LaTex stuff
let g:Tex_DefaultTargetFormat = 'pdf'
set grepprg=grep\ -nH\ $*
let g:tex_flavor='latex'
let g:Imap_UsePlaceHolders = 0        "Set this if you use '()'
let g:Imap_FreezeImap=1

