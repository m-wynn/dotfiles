"" euclio/vim-markdown-composer -- define BuildComposer
function! BuildComposer(info)
  if a:info.status != 'unchanged' || a:info.force
    !cargo build --release
    UpdateRemotePlugins
  endif
endfunction

"" Plugins using Vim Plug https://github.com/junegunn/vim-plug

call plug#begin('~/.vim/plugged')
	Plug 'ap/vim-css-color'				" Sets the background to your color. #ff0000 < that is white on red
	Plug 'benekastah/neomake'			" Make for all kinds of things.  Can take advantage of Neovims asyncronity
	Plug 'cazador481/fakeclip.neovim'		" * and + map to the X clipboard if X is running.  & maps to tmux if it's running.
	Plug 'chase/vim-ansible-yaml'			" Syntax highlighting for ansible yaml files.  It knows if you're in an ansible folder.
	Plug 'danro/rename.vim'				" Rename file :rename[!] {newname}
	Plug 'ervandew/supertab'			" Make the tab key do tab completion.  Or any other key, for that matter.  Customizable
	Plug 'euclio/vim-markdown-composer',	{ 'do': function('BuildComposer') }		" Adds asyncronous markdown previous (neovim pls)
	Plug 'joonty/vdebug', { 'for': 'php' }		" Interfaces with debuggers.  Needs some configuration soon
	Plug 'markcornick/vim-vagrant'			" Vagrant support
	Plug 'ntpeters/vim-better-whitespace'		" Easily strip whitespace
	Plug 'shawncplus/phpcomplete.vim'		" Lots of completions and ctag-jumping stuff for PHP.  Pretty cool, check readme for ctags
	Plug 'Shougo/deoplete.nvim'			" Completion
	Plug 'StanAngeloff/php.vim'			" Newer PHP syntax highlighting that's a pain to actually get working, I think
	Plug 'Townk/vim-autoclose'			" Automagically closes parentheses and such.
	Plug 'tpope/vim-fugitive'			" Git plugin for like, :Gstatus
	Plug 'tpope/vim-unimpaired'			" Add lots of handy mappings
	Plug 'vim-airline/vim-airline'			" Informative tabline/status bar for vim
	Plug 'vim-airline/vim-airline-themes'		" Themes for Airline
	Plug 'vim-latex/vim-latex'			" Such a powerful thing for LaTeX
	Plug 'vim-scripts/auctex.vim'			" Better Vim syntax highlighting
	Plug 'wlangstroth/vim-racket'			" Racket stuff
	Plug 'w0ng/vim-hybrid'				" Colors!
	Plug 'xolox/vim-misc'				" Miscellaneous stuff, required for vim-notes
	Plug 'xolox/vim-notes'				" Notes in Vim!
	Plug 'yegappan/mru'				" Most Recently Used Files
call plug#end()

"" Lose the annoyances of vi
set nocompatible

"" Load plugins and indentation for specific filetypes
filetype plugin indent on

"" Syntax highlighting
syntax on

"" title in the titlebar.  A bit weird
set titlestring=VIM:\ %-5.10t\ %a%r%m titlelen=15
set title

"" tabs
set tabstop=8 softtabstop=0 noexpandtab shiftwidth=8

"" Put temps somewhere else
set dir=~/.vim/swaps
set backupdir=~/.vim/backups
set undodir=~/.vim/undo
set undofile

" Colors!
let $NVIM_TUI_ENABLE_TRUE_COLOR=1
let g:hybrid_custom_term_colors = 1
set background=dark
colorscheme hybrid
let g:airline_theme = 'hybridline'
hi Normal ctermbg=none					" Use this if you have a colorscheme that breaks terminal transparency

" Show matching parenthesis
set showmatch

" Autoindentation.
set autoindent

" Allow backspacing over line breaks, start of insert action, and autoindentation
set backspace=eol,start,indent

" Ignore case when searching
set ignorecase
" When searching try to be smart about cases
set smartcase
" Highlight search results
set hlsearch
" Makes search act like search in modern browsers
set incsearch

" Show line numbers
set number
autocmd InsertEnter * :set norelativenumber
autocmd InsertLeave * :set relativenumber

function! NumberToggle()
  if(&relativenumber == 1)
    set norelativenumber
  else
    set relativenumber
  endif
endfunc

nnoremap <C-n> :call NumberToggle()<cr>

" Make the mouse useful
set mouse=a

" Allow saving of files as sudo when I forgot to start vim using sudo.
cmap w!! w !sudo tee > /dev/null %

"Word wrapping is fine, just don't insert newlines, please.
set wrap linebreak nolist
set textwidth=0
set wrapmargin=0

"""""""""""""""""""
" Plugin-Specific "
"    Settings     "
"""""""""""""""""""

"" bling/vim-airline
set laststatus=2					" Always show the status
set noshowmode						" Don't show the mode below the statusline, we're taking care of that in vim-airline
let g:airline_powerline_fonts = 1			" Use powerline symbols
let g:airline#extensions#tabline#enabled = 1		" Tablinify the tabbar

"" ervandew/supertab --  changes to let us use tab again
let g:SuperTabMappingForward = '<c-p>'
let g:SuperTabMappingBackward = '<s-c-p>'

"" ntpters/vim-better-whitespace -- Automagically strip on save
autocmd BufWritePre * StripWhitespace
"let g:better_whitespace_filetypes_blacklist+=['<filetype1>', '<filetype2>', '<etc>']

"" StanAngeloff/php.vim -- Overrides that should be removed or expanded later
function! PhpSyntaxOverride()
  hi! def link phpDocTags  phpDefine
  hi! def link phpDocParam phpType
endfunction

augroup phpSyntaxOverride
  autocmd!
  autocmd FileType php call PhpSyntaxOverride()
augroup END


"" vim-latex/vim-latex -- LaTex stuff
let g:Tex_DefaultTargetFormat = 'pdf'
set grepprg=grep\ -nH\ $*
let g:tex_flavor='latex'
let g:Imap_UsePlaceHolders = 0		"Set this if you ever EVER are going to use '()'
let g:Imap_FreezeImap=1

let g:notes_directories = ['~/Dropbox/Fall15/notes']

"" Eclim
let g:EclimCompletionMethod = 'omnifunc'


autocmd FileType racket set tabstop=2|set shiftwidth=2|set expandtab
autocmd FileType haskell set tabstop=2|set shiftwidth=2|set expandtab

autocmd! BufWritePost * Neomake


let g:deoplete#enable_at_startup = 1
