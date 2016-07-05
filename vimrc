"" Plugins using Vim Plug https://github.com/junegunn/vim-plug

call plug#begin('~/.vim/plugged')
	Plug 'amirh/HTML-AutoCloseTag', {'for': 'html'}		" Auto close html tags
	Plug 'AndrewRadev/splitjoin.vim'					" Splitting and joining lines and blocks
	Plug 'ap/vim-css-color'								" Sets the background to your color. #ff0000 < that is white on red
	Plug 'benekastah/neomake'							" Make for all kinds of things.  Can take advantage of Neovims asyncronity
	Plug 'cazador481/fakeclip.neovim'					" * and + map to the X clipboard if X is running.  & maps to tmux if it's running.
	Plug 'fweep/vim-zsh-path-completion'				" Better completion for :e and friends
	Plug 'honza/vim-snippets'							" Snippits Stuff
	Plug 'jiangmiao/auto-pairs'							" Insert or delete brackets, parens, quotes in pair
	Plug 'junegunn/vim-easy-align'						" Align things more easily
	Plug 'Konfekt/FastFold'								" Speed up folds
	Plug 'majutsushi/tagbar'							" Display tags in a window, ordered by scope
	Plug 'ntpeters/vim-better-whitespace'				" Easily strip whitespace
	Plug 'pearofducks/ansible-vim', {'for': 'ansible'}	" Ansible stuff
	Plug 'shawncplus/phpcomplete.vim'					" Lots of completions and ctag-jumping stuff for PHP.  Pretty cool, check readme for ctags
	Plug 'sheerun/vim-polyglot'							" Support for sooo many languages
	Plug 'Shougo/context_filetype.vim'					" Add context filetype feature to completion
	Plug 'Shougo/deoplete.nvim'							" Completion
	Plug 'Shougo/echodoc.vim'							" Print documentation in autocomplete
	Plug 'Shougo/neoinclude.vim'						" Completion framework
	Plug 'Shougo/neopairs.vim'							" Autoclose parentheses
	Plug 'tmhedberg/SimpylFold'							" Python folding
	Plug 'SirVer/ultisnips'								" Snippits
	Plug 'tpope/vim-abolish'							" Smarter find-replacement, and because I can't spell
	Plug 'tpope/vim-afterimage'							" Edit images, pdfs, and word docs in vim
	Plug 'tpope/vim-commentary'							" Comment things easily with gc
	Plug 'tpope/vim-eunuch'								" Handy UNIX commands like :Locate and :SudoEdit
	Plug 'tpope/vim-fugitive'							" Git plugin for like, :Gstatus
	Plug 'tpope/vim-ragtag'								" More tag mappings
	Plug 'tpope/vim-repeat'								" Use . in to repeat plugin stuff
	Plug 'tpope/vim-sleuth'								" Figure out tabs based on the file
	Plug 'tpope/vim-surround'							" Change the surrounding stuff
	Plug 'tpope/vim-unimpaired'							" Add lots of handy mappings
	Plug 'tpope/vim-vinegar'							" Improve netrw
	Plug 'vim-airline/vim-airline'						" Informative tabline/status bar for vim
	Plug 'vim-airline/vim-airline-themes'				" Themes for Airline
	Plug 'vim-latex/vim-latex'							" Such a powerful thing for LaTeX
	Plug 'w0ng/vim-hybrid'								" Colors!
	Plug 'wellle/tmux-complete.vim'
	Plug 'xolox/vim-misc'								" Miscellaneous stuff, required for vim-notes
	Plug 'xolox/vim-notes'								" Notes in Vim!
	Plug 'yegappan/mru'									" Most Recently Used Files
	Plug 'zchee/deoplete-jedi', {'for': 'python'}		" Python completion
	Plug 'docker/docker' , {'rtp': '/contrib/syntax/vim/'} " Docker syntax
call plug#end()

""""""""""""""
"	General  "
"	  Vim	 "
""""""""""""""

"" Lose the annoyances of vi
set nocompatible		" This is generally considered useless

"" Load plugins and indentation for specific filetypes
filetype plugin indent on

"" Syntax highlighting
syntax on

"" title in the titlebar.  A bit weird
set titlestring=VIM:\ %-5.10t\ %a%r%m titlelen=15
set title

"" Put temps somewhere else
set dir=~/.vim/swaps//
set backupdir=~/.vim/backups//
set undodir=~/.vim/undo//
set undofile

" Colors!
"let $NVIM_TUI_ENABLE_TRUE_COLOR=0
let g:hybrid_custom_term_colors = 1
set background=dark
colorscheme hybrid
let g:airline_theme = 'hybridline'
" Use this if you have a colorscheme that breaks terminal transparency
hi Normal ctermbg=none

" Show matching parenthesis
set showmatch

" Autoindentation.
set autoindent

"" tabs
set tabstop=4 softtabstop=0 noexpandtab shiftwidth=4
set shiftround		" Indents to the next multiple of shiftwidth


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
" Wrap scan around end-of-file
set wrapscan

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

" Allow switching buffers without saving
set hidden

"Word wrapping is fine, just don't insert newlines, please.
set wrap linebreak nolist
set textwidth=0
set wrapmargin=0

" File-specific settings
autocmd FileType racket set tabstop=2|set shiftwidth=2|set expandtab
autocmd FileType haskell set tabstop=2|set shiftwidth=2|set expandtab
autocmd FileType mail setlocal fo+=aw

" Make Enter select completion without adding a new line.
inoremap <expr> <CR>       pumvisible() ? "\<C-y>" : "\<CR>"


"""""""""""""""""""
" Plugin-Specific "
"	 Settings	  "
"""""""""""""""""""

"" benekastah/neomake -- Asyncronous syntax checking
autocmd! BufWritePost * Neomake				" Run Neomake on every write

"" junegunn/vim-easy-align
xmap ga <Plug>(EasyAlign)|		 " Start interactive EasyAlign in visual mode (e.g. ipga)

nmap ga <Plug>(EasyAlign)|		" Start interactive EasyAlign for a motion/text object (e.g. gaip)

"" ntpters/vim-better-whitespace -- Automagically strip on save
let g:better_whitespace_filetypes_blacklist=['mail', 'diff', 'gitcommit', 'unite', 'qf', 'help']

"" Shougo/deoplete.nvim -- Completion
let g:deoplete#enable_at_startup = 1

"" SirVer/ultisnips -- Snippets
let g:UltiSnipsExpandTrigger="<tab>"		"Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

let g:UltiSnipsEditSplit="vertical" 			" If you want :UltiSnipsEdit to split your window.

"" tpope/vim-fugitive -- All the gits
autocmd BufReadPost fugitive://* set bufhidden=delete	" Delete old git-object buffers when traversing the dag

"" vim-airline/vim-airline
set laststatus=2					" Always show the status
set noshowmode						" Don't show the mode below the statusline, we're taking care of that in vim-airline
let g:airline_powerline_fonts = 1			" Use powerline symbols
let g:airline#extensions#tabline#enabled = 1		" Tablinify the tabbar
let g:airline#extensions#whitespace#mixed_indent_algo = 1	" Don't complain about C-style comments with funny indents
let g:airline#extensions#tabline#buffer_nr_show = 1


"" vim-latex/vim-latex -- LaTex stuff
let g:Tex_DefaultTargetFormat = 'pdf'
set grepprg=grep\ -nH\ $*
let g:tex_flavor='latex'
let g:Imap_UsePlaceHolders = 0		"Set this if you ever EVER are going to use '()'
let g:Imap_FreezeImap=1

"" xolox/vim-notes
let g:notes_directories = ['~/Documents/notes']
