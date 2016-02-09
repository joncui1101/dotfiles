"================================================================================
" Plugin manager
"================================================================================

set nocompatible
set termencoding=utf-8
set term=xterm-256color
let base16colorspace=256  " Access colors present in 256 colorspace"

" 256bit terminal
set t_Co=256

" Needed for vundle, will be turned on after vundle inits
filetype off

set rtp+=~/dotfiles/vim/bundle/vundle/
call vundle#begin('$HOME/dotfiles/vim/bundle')

" Let Vundle manage Vundle
Plugin 'gmarik/vundle'

" Fuzzy search
Plugin 'ctrlpvim/ctrlp.vim'

" Code completion
Plugin 'ervandew/supertab'
Plugin 'Valloric/YouCompleteMe'

" Syntax checker
Plugin 'scrooloose/syntastic'

" Git
Plugin 'tpope/vim-fugitive'

" Snippets
" Plugin 'SirVer/ultisnips'

" Text Objects
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-repeat'
Plugin 'jiangmiao/auto-pairs'
Plugin 'bkad/CamelCaseMotion'
Plugin 'justinmk/vim-sneak'

" Tags
" Plugin 'majutsushi/tagbar'

" Status line
Plugin 'bling/vim-airline'
Plugin 'bling/vim-bufferline'

" Color Themes
Plugin 'chriskempson/base16-vim'

" Comment Plugin
Plugin 'tpope/vim-commentary'

" Emmet Plugin
Plugin 'mattn/emmet-vim'

call vundle#end()

"================================================================================
" Custom Functions (CF)
"================================================================================

" Removes trailing spaces
function! TrimWhiteSpace()
    if &modifiable
        %s/\s\+$//e
    endif
endfunction

" Clear highlighting
if maparg('<C-L>', 'n') ==# ''
    nnoremap <silent> <C-L> :nohlsearch<CR><C-L>
endif

"================================================================================
" Colorscheme (CS)
"================================================================================

set background=dark
" colorscheme solarized
colorscheme base16-flat

"================================================================================
" Auto Commands (AC)
"================================================================================

autocmd BufEnter * set formatoptions-=ro

" Set augroup
augroup MyAutoCmd
    autocmd!
augroup END

augroup vimrc
    autocmd!
augroup END

" Delete trailing whitespace
augroup MyAutoCmd
    autocmd FileWritePre   * :call TrimWhiteSpace()
    autocmd FileAppendPre  * :call TrimWhiteSpace()
    autocmd FilterWritePre * :call TrimWhiteSpace()
    autocmd BufWritePre    * :call TrimWhiteSpace()
augroup END

" Reload vimrc when edited
autocmd MyAutoCmd BufWritePost .vimrc source ~/.vimrc

augroup MyAutoCmd
    autocmd FileWritePre   * :retab
    autocmd FileAppendPre  * :retab
    autocmd FilterWritePre * :retab
    autocmd BufWritePre    * :retab
augroup END

"================================================================================
" General Settings (GS)
"================================================================================

syntax enable

" Detect plugins and indenting
filetype plugin indent on

" Keycode timeout
set ttimeout
set ttimeoutlen=50

" Enable mouse
set mouse=a

" Set encoding for text
set encoding=utf-8

" Add mac to auto-detection of file format line endings
set fileformats+=mac

" Always assume decimal numbers
set nrformats-=octal

" Allow changing buffer without saving it first
set hidden

" Allow a longer history
set history=1000

" Give one virtual space at the end of line
set virtualedit=onemore

" Show last status on statusline
set laststatus=2

" Command-line auto completion
set wildmenu
set wildmode=list:longest,full " Complete longest common string, then each full match
set wildignore=*/.DS_Store,*/.git/*,*/.idea/* " Stuff to ignore when tab completing

" Show the position on the bottom right
set ruler

" Set backspace config
set backspace=eol,start,indent

" Case insensitive search
set ignorecase
set smartcase

" Make search act like search in modern browsers
set incsearch

" Turn backup off
set nobackup
set nowritebackup
set noswapfile

" Text display settings
set autoindent
set wrap
set linebreak
set nolist
set textwidth=0
set wrapmargin=0

" Min # of screen lines above and below cursor
set scrolloff=10

" Min # of screen columns to keep to the left and right of the cursor
set sidescrolloff=20

" Min width of number column
set numberwidth=1

" Auto complete setting
set completeopt=longest,menuone

" Always show tabs
set showtabline=2

" No annoying sound on errors
set vb
set t_vb=
set noerrorbells
set novisualbell

" Show command on bottom
set showcmd

" Show line number
set number

" Use spaces instead of tabs
set expandtab
set smarttab

" 1 tab == 4 spaces
set shiftwidth=4
set tabstop=4
let softtabstop=4

" Set to auto read when a file is changed from the outside
set autoread

" Set to auto write file
set autowriteall

" Match braces/brackets/etc
set showmatch

" Round indent to multiple of shiftwidth
set shiftround

set foldenable " Enable folds by default
set foldmethod=syntax " Fold via syntax of files
set foldlevelstart=99 " Open all folds by default

" Remove the default mode
set noshowmode

" Default to highlight search
set hlsearch

" Keep undo history across sessions, by storing in file.
" Only works all the time.
if has('persistent_undo')
    silent !mkdir ~/.vimbackups > /dev/null 2>&1
    set undodir=~/.vimbackups
    set undofile
endif

"================================================================================
" Leader Key Mappings (LK)
"================================================================================

" Map leader to comma
let mapleader = ","

" Toggle between paste mode; useful for pasting large items
nnoremap <silent> <Leader>1 :set paste!<CR>

" Toggle copy mode; useful for copying text from files
nnoremap <silent> <Leader>2 :call ToggleCopy()<CR>
function! ToggleCopy()
    if &mouse == 'a'
        set mouse=
        set nonu
    else
        set mouse=a
        set nu
    endif
endfunction

" Next Buffer
nnoremap <silent> <Leader>l :bn<CR>

" Previous Buffer
nnoremap <silent> <Leader>h :bp<CR>

" First Buffer
nnoremap <silent> <Leader>k :bf<CR>

" Last Buffer
nnoremap <silent> <Leader>j :bl<CR>

"================================================================================
" Normal Mode Key Mappings (NK)
"================================================================================

" Inserts a new line in normal mode
nmap <S-Enter> O<Esc>
nmap <CR> o<Esc>

" Inserts new line at cursor (undoes what J does)
nnoremap K i<CR><Esc>h

" Backspace: toggle search highlight
nnoremap <BS> :set hlsearch! hlsearch?<CR>

" Remap colon to semicolon for go into command mode easier
noremap ; :

" Q closes the window
nnoremap Q :q<CR>

"================================================================================
" Insert Mode Key Mappings (IK)
"================================================================================

" Escape from insert mode
inoremap jj <Esc>

"================================================================================
" Visual Mode Key Mappings (VK)
"================================================================================

" Make < > shifts keep selection
xnoremap < <gv
xnoremap > >gv

" Paste in visual mode should not replace register with deleted text
" xnoremap p "_dP

" Delete into blackhold register to not clobber last yank
" xnoremap d "_d

" Indent
xmap <Tab> >

" Unindent
xmap <S-Tab> <

" Swap deleted item with selected item
vnoremap <C-X> <Esc>`.``gvP``P`

"================================================================================
" Airline Settings (AS)
"================================================================================

if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif

" Enable enhanced tabline
let g:airline#extensions#tabline#enabled = 1

" Enable bufferline integration
let g:airline#extensions#bufferline#enabled = 1

let g:airline_powerline_fonts = 1
let g:airline_left_sep = ''
let g:airline_right_sep = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = ''

"================================================================================
" Bufferline Settings (BS)
"================================================================================

let g:bufferline_echo = 0

"================================================================================
" Tagbar Settings (TS)
"================================================================================

" Toggle tagbar with ,2
nnoremap <silent> <Leader>3 :TagbarToggle<CR>

" Where is ctags located?
let g:tagbar_ctags_bin = '/usr/local/Cellar/ctags/5.8/bin/ctags'

" Tagbar opens to the left
let g:tagbar_left = 1

" Cursor will jump to tagbar window when it is open
let g:tagbar_autofocus = 1

" Show line numbers
let g:tagbar_show_linenumbers = 1

"================================================================================
" Syntastic Settings (SS)
"================================================================================

let g:syntastic_php_checkers        = ['php', 'phpcs', 'phpmd']

let g:syntastic_mode_map            = { 'mode': 'active',
                                      \ 'active_filetypes': ['php'],
                                      \ 'passive_filetypes': [] }

" Mark syntax errors with :signs
let g:syntastic_enable_signs        = 1

" Automatically jump to the error when saving the file
let g:syntastic_auto_jump           = 0

" Show the error list automatically
let g:syntastic_auto_loc_list       = 1

" Don't care about warnings
let g:syntastic_quiet_messages      = { 'level': 'warnings'  }

" Check syntax when buffers are first loaded
let g:syntastic_check_on_open       = 1

"================================================================================
" Auto-Pairs Settings (APS)
"================================================================================

let g:AutoPairsFlyMode = 1

"================================================================================
" Sneak Settings (SS)
"================================================================================

let g:sneak#steak = 1

nmap <leader>s <Plug>SneakNext
xmap <leader>s <Plug>SneakNext
nmap <leader>S <Plug>SneakPrevious
xmap <leader>S <Plug>SneakPrevious

"================================================================================
" YouCompleteMe Settings (YS)
"================================================================================

let g:ycm_complete_in_comments_and_strings = 1

"================================================================================
" UltiSnips Settings (US)
"================================================================================

let g:UltiSnipsSnippetsDir = '~/dotfiles/vim/bundle/ultisnips/UltiSnips'

" Specify python version
let g:UltiSnipsUsePythonVersion = 2

let g:UltiSnipsExpandTrigger="<Leader>j"
let g:UltiSnipsJumpForwardTrigger="<Leader>l"
let g:UltiSnipsJumpBackwardTrigger="<Leader>h"

autocmd vimrc BufEnter *.snippets setf snippets

" Use actual tabs instead of spaces in snippet files
autocmd vimrc FileType snippets set noexpandtab
