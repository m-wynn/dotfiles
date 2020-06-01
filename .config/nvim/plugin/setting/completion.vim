"" deoplete.nvim -- Completion
let g:deoplete#enable_at_startup = 1
call deoplete#custom#option('sources', {'_': ['ale', 'file', 'ultisnips', 'around', 'buffer', 'member']})

call g:deoplete#custom#source('ale', 'rank', 600)
call g:deoplete#custom#source('ultisnips', 'rank', 800)

set completeopt+=noinsert
autocmd CompleteDone * pclose!

"" neopairs.vim
let g:neopairs#enable = 1
