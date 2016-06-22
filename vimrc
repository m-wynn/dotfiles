"" Plugins using Vim Plug https://github.com/junegunn/vim-plug

call plug#begin('~/.vim/plugged')
	Plug 'amirh/HTML-AutoCloseTag', {'for': 'html'}		" Auto close html tags
	Plug 'AndrewRadev/splitjoin.vim'					" Splitting and joining lines and blocks
	Plug 'ap/vim-css-color'								" Sets the background to your color. #ff0000 < that is white on red
	Plug 'benekastah/neomake'							" Make for all kinds of things.  Can take advantage of Neovims asyncronity
	Plug 'cazador481/fakeclip.neovim'					" * and + map to the X clipboard if X is running.  & maps to tmux if it's running.
	Plug 'danro/rename.vim'								" Rename file :rename[!] {newname}
	Plug 'markcornick/vim-vagrant'						" Vagrant support
	Plug 'nhooyr/neoman.vim'							" Man pages in vim
	Plug 'ntpeters/vim-better-whitespace'				" Easily strip whitespace
	Plug 'pearofducks/ansible-vim', {'for': 'ansible'}	" Ansible stuff
	Plug 'shawncplus/phpcomplete.vim'					" Lots of completions and ctag-jumping stuff for PHP.  Pretty cool, check readme for ctags
	Plug 'sheerun/vim-polyglot'							" Support for sooo many languages
	Plug 'Shougo/deoplete.nvim'							" Completion
	Plug 'Shougo/echodoc.vim'							" Completion
	Plug 'Shougo/neoinclude.vim'						" Completion
	Plug 'Shougo/neopairs.vim'							" Completion
	Plug 'tmhedberg/SimpylFold'							" Python folding
	Plug 'Townk/vim-autoclose'							" Automagically closes parentheses and such.
	Plug 'tpope/vim-commentary'							" Comment things easily
	Plug 'tpope/vim-fugitive'							" Git plugin for like, :Gstatus
	Plug 'tpope/vim-ragtag'								" More tag mappings
	Plug 'tpope/vim-sleuth'								" Figure out tabs based on the file
	Plug 'tpope/vim-surround'							" Change the surrounding stuff
	Plug 'tpope/vim-unimpaired'							" Add lots of handy mappings
	Plug 'vim-airline/vim-airline'						" Informative tabline/status bar for vim
	Plug 'vim-airline/vim-airline-themes'				" Themes for Airline
	Plug 'vim-latex/vim-latex'							" Such a powerful thing for LaTeX
	Plug 'w0ng/vim-hybrid'								" Colors!
	Plug 'welle/tmux-complete.vim'
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


"""""""""""""""""""
" Plugin-Specific "
"	 Settings	  "
"""""""""""""""""""

"" benekastah/neomake -- Asyncronous checking
autocmd! BufWritePost * Neomake				" Run Neomake on every write

"" ntpters/vim-better-whitespace -- Automagically strip on save
""autocmd BufWritePre * StripWhitespace
"let g:better_whitespace_filetypes_blacklist+=['<filetype1>', '<filetype2>', '<etc>']

"" Shougo/deoplete.nvim -- Completion
let g:deoplete#enable_at_startup = 1

"" tpope/vim-fugitive -- All the gits
autocmd BufReadPost fugitive://* set bufhidden=delete	" Delete old git-object buffers when traversing the dag

"" vim-airline/vim-airline
set laststatus=2					" Always show the status
set noshowmode						" Don't show the mode below the statusline, we're taking care of that in vim-airline
let g:airline_powerline_fonts = 1			" Use powerline symbols
let g:airline#extensions#tabline#enabled = 1		" Tablinify the tabbar


"" vim-latex/vim-latex -- LaTex stuff
let g:Tex_DefaultTargetFormat = 'pdf'
set grepprg=grep\ -nH\ $*
let g:tex_flavor='latex'
let g:Imap_UsePlaceHolders = 0		"Set this if you ever EVER are going to use '()'
let g:Imap_FreezeImap=1

"" xolox/vim-notes
let g:notes_directories = ['~/Dropbox/Fall15/notes']
