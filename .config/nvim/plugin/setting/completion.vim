"" deoplete.nvim -- Completion
let g:deoplete#enable_at_startup = 1
let g:deoplete#sources = {'_': ['ale', 'file', 'around', 'buffer', 'member']}

call g:deoplete#custom#source('ale', 'rank', 600)

set completeopt+=noinsert

"" neopairs.vim
let g:neopairs#enable = 1
