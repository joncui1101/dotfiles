"================================================================================
" Plugin manager
"================================================================================

let base16colorspace=256  " Access colors present in 256 colorspace"

let g:loaded_python_provider = 1
let g:python3_host_prog = '/usr/local/bin/python3'
let g:python3_host_skip_check = 1

let g:loaded_ruby_provider = 1

call plug#begin('~/.local/share/nvim/plugged')

" Fuzzy search
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" Code Completion
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

" Auto insert pairs
Plug 'jiangmiao/auto-pairs'

" Javascript Completion
Plug 'carlitux/deoplete-ternjs', { 'do': 'npm install -g tern' }

" Vimscript completion
Plug 'Shougo/neco-vim'

" Java Completion
Plug 'artur-shaik/vim-javacomplete2'

" Go Plugins
Plug 'nsf/gocode', { 'rtp': 'nvim', 'do': '~/.local/share/nvim/plugged/gocode/nvim/symlink.sh' }
Plug 'zchee/deoplete-go', { 'do': 'make' }
Plug 'fatih/vim-go', {'do': ':GoInstallBinaries'}

" HTML Plugins
Plug 'mattn/emmet-vim'

" Vue Plugins
Plug 'posva/vim-vue'

" Syntax checker
Plug 'w0rp/ale'

" Git
Plug 'tpope/vim-fugitive'

" Text Objects
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'bkad/CamelCaseMotion'
Plug 'justinmk/vim-sneak'

" Status line
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'bling/vim-bufferline'

" Color Themes
Plug 'chriskempson/base16-vim'
Plug 'rakr/vim-one'

" Comment Plugin
Plug 'tpope/vim-commentary'

" Emmet Plugin
Plug 'mattn/emmet-vim'

call plug#end()

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
colorscheme base16-flat

highlight Normal ctermbg=None
highlight NonText ctermbg=None

"================================================================================
" Auto Commands (AC)
"================================================================================

autocmd BufEnter * set formatoptions-=ro

" Set augroup
augroup MyAutoCmd
    autocmd!
augroup END

augroup initvim
    autocmd!
augroup END

" Delete trailing whitespace
augroup MyAutoCmd
    autocmd FileWritePre   * :call TrimWhiteSpace()
    autocmd FileAppendPre  * :call TrimWhiteSpace()
    autocmd FilterWritePre * :call TrimWhiteSpace()
    autocmd BufWritePre    * :call TrimWhiteSpace()
augroup END

" Reload init.vim when edited
autocmd MyAutoCmd BufWritePost init.vim nested :source ~/.config/nvim/init.vim

augroup MyAutoCmd
    autocmd FileWritePre   * :retab
    autocmd FileAppendPre  * :retab
    autocmd FilterWritePre * :retab
    autocmd BufWritePre    * :retab
augroup END

autocmd FileType java setlocal omnifunc=javacomplete#Complete
autocmd FileType vue setlocal shiftwidth=2 tabstop=2 softtabstop=2
autocmd FileType javascript setlocal shiftwidth=2 tabstop=2 softtabstop=2
autocmd FileType yaml setlocal shiftwidth=2 tabstop=2 softtabstop=2

autocmd FileType go nmap <leader>r <Plug>(go-run)
autocmd FileType go nmap <leader>b <Plug>(go-build)

"================================================================================
" General Settings (GS)
"================================================================================

" Enable mouse if possible
if has('mouse')
    set mouse=a
endif

" Allow changing buffer without saving it first
set hidden

" Give one virtual space at the end of line
set virtualedit=onemore

" Command-line auto completion
set wildmenu
set wildmode=list:longest,full " Complete longest common string, then each full match
set wildignore=*/.DS_Store,*/.git/*,*/.idea/*,*/node_modules/* " Stuff to ignore when tab completing

" Case insensitive search
set ignorecase
set smartcase

" Turn backup off
set nobackup
set nowritebackup
set noswapfile

" Break line at character specified by 'breakat'
set linebreak

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

" Show line number
set number

" Use spaces instead of tabs
set expandtab

" 1 tab == 4 spaces
set shiftwidth=4
set tabstop=4
let softtabstop=4

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

" <Leader>1: Toggle between paste mode
nnoremap <silent> <Leader>1 :set paste!<cr>

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

" Indent
xmap <Tab> >

" Unindent
xmap <S-Tab> <

" Swap deleted item with selected item
vnoremap <C-X> <Esc>`.``gvP``P`

"================================================================================
" Airline Settings (AS)
"================================================================================

" Enable enhanced tabline
let g:airline#extensions#tabline#enabled = 1

" Enable bufferline integration
let g:airline#extensions#bufferline#enabled = 1

"================================================================================
" Bufferline Settings (BS)
"================================================================================

let g:bufferline_echo = 0

"================================================================================
" Sneak Settings (SS)
"================================================================================

let g:sneak#steak = 1

nmap <leader>s <Plug>SneakNext
xmap <leader>s <Plug>SneakNext
nmap <leader>S <Plug>SneakPrevious
xmap <leader>S <Plug>SneakPrevious

"================================================================================
" CamelCaseMotion Settings (CCMS)
"================================================================================

call camelcasemotion#CreateMotionMappings('<leader>')

"================================================================================
" deoplete Settings (dS)
"================================================================================

" Use deoplete.
let g:deoplete#enable_at_startup = 1

" Use smartcase.
let g:deoplete#enable_smart_case = 1

"================================================================================
" File Completion Settings (FCS)
"================================================================================

" Javascript
let g:tern_request_timeout = 1
let g:tern_show_signature_in_pum = '0'

let g:tern#filetypes = [ 'jsx', 'javascript.jsx', 'vue' ]

" Go
let g:deoplete#sources#go#gocode_binary = $GOPATH.'/bin/gocode'

"================================================================================
" Ale (AleS)
"================================================================================

" Customize error and warning string.
let g:ale_echo_msg_error_str = 'E'
let g:ale_echo_msg_warning_str = 'W'

" Set message format.
let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'

" Never lint on text change.
let g:ale_lint_on_text_changed = 'never'

" Disable lint on enter.
let g:ale_lint_on_enter = 0

" Whitelist linters.
let g:ale_linters = {
\   'javascript': ['eslint'],
\}

"================================================================================
" FZF (FzfS)
"================================================================================
nnoremap <c-p> :FZF<CR>

"================================================================================
" Golang
"================================================================================
let g:airline#extensions#ale#enabled = 1
let g:go_addtags_transform = "camelcase"
let g:go_auto_sameids = 1
let g:go_auto_type_info = 1
let g:go_fmt_command = "goimports"
let g:go_highlight_build_constraints = 1
let g:go_highlight_extra_types = 1
let g:go_highlight_fields = 1
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_operators = 1
let g:go_highlight_structs = 1
let g:go_highlight_types = 1
