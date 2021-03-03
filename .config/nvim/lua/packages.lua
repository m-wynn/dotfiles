local paqfile=os.getenv("HOME") .. "/.local/share/nvim/site/pack/paqs/opt/paq-nvim"

function file_exists(name)
   local f=io.open(name,"r")
   if f~=nil then io.close(f) return true else return false end
end

if not file_exists(paqfile) then
    os.execute("mkdir -p \"" .. paqfile .. "\"")
    os.execute("git clone https://github.com/savq/paq-nvim.git \"" .. paqfile .. "\"")
end

vim.cmd 'packadd paq-nvim'         -- Load package
local paq = require'paq-nvim'.paq  -- Import module and bind `paq` function
paq{'savq/paq-nvim', opt=true}     -- Let Paq manage itself

-- navigation and tools
paq {'airblade/vim-rooter'}
paq {'jreybert/vimagit'}
paq {'kassio/neoterm'}
paq {'lotabout/skim.vim'}
paq {'deathlyfrantic/vim-fubitive'}
paq {'tpope/vim-eunuch'}
paq {'tpope/vim-fugitive'}
paq {'tpope/vim-rhubarb'}

-- nerdtree
paq {'scrooloose/nerdtree'}
paq {'tiagofumo/vim-nerdtree-syntax-highlight'}
paq {'Xuyuanp/nerdtree-git-plugin'}

-- lanugages
paq {'neovim/nvim-lspconfig'}
paq {'nvim-treesitter/nvim-treesitter', run=vim.fn[':TSUpdate']}
paq {'sheerun/vim-polyglot'}

-- completion
paq {'deoplete-plugins/deoplete-lsp'}
paq {'fszymanski/deoplete-emoji'}
paq {'Shougo/context_filetype.vim'}
paq {'Shougo/deoplete.nvim', run=vim.fn[':UpdateRemotePlugins']}
paq {'Shougo/echodoc.vim'}
paq {'Shougo/neoinclude.vim'}
paq {'Shougo/neopairs.vim'}
paq {'wellle/tmux-complete.vim'}

-- editing
paq {'AndrewRadev/splitjoin.vim'}
paq {'junegunn/vim-easy-align'}
paq {'machakann/vim-sandwich'}
paq {'michaeljsmith/vim-indent-object'}
paq {'rhysd/clever-f.vim'}
paq {'tpope/vim-abolish'}
paq {'tpope/vim-commentary'}
paq {'tpope/vim-ragtag'}
paq {'tpope/vim-repeat'}
paq {'tpope/vim-sleuth'}
paq {'tpope/vim-surround'}
paq {'tpope/vim-unimpaired'}
paq {'wellle/targets.vim'}
paq {'iamcco/markdown-preview.nvim', run='cd app & yarn install'}

-- snippets
paq {'SirVer/ultisnips'}
paq {'epilande/vim-react-snippets'}
paq {'honza/vim-snippets'}
paq {'juliosueiras/vim-terraform-snippets', run='rm snippets && mkdir snippets && mv terraform snippets/terraform' }
paq {'noahfrederick/vim-skeleton'}

-- ui
paq {'ap/vim-css-color'}
paq {'vim-airline/vim-airline'}
paq {'vim-airline/vim-airline-themes'}
paq {'majutsushi/tagbar'}
paq {'morhetz/gruvbox'}
paq {'ryanoasis/vim-devicons'}
