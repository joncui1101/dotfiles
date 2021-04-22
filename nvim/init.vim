"================================================================================
" Plugin manager
"================================================================================

let base16colorspace=256  " Access colors present in 256 colorspace"

let g:loaded_python_provider = 1
let g:python_host_prog = '~/.config/pyenv/versions/neovim2/bin/python'
let g:python3_host_prog = '~/.config/pyenv/versions/neovim3/bin/python'
let g:python3_host_skip_check = 1

let g:loaded_ruby_provider = 1

" Autoinstall vim-plug
if empty(glob('~/.config/local/share/nvim/site/autoload/plug.vim'))
  silent !curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall
endif

call plug#begin('~/.config/local/share/nvim/plugged')

" Fuzzy search
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
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

" Markdown Plugins
Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() }, 'for': ['markdown', 'vim-plug']}

" HTML Plugins
Plug 'mattn/emmet-vim'

" Syntax Plugins
Plug 'sheerun/vim-polyglot'

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

" Comment Plugin
Plug 'tpope/vim-commentary'

" Emmet Plugin
Plug 'mattn/emmet-vim'

" ledger Plugin
Plug 'ledger/vim-ledger'

" vimwiki Plugin
Plug 'vimwiki/vimwiki'

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
colorscheme base16-onedark

highlight Normal ctermbg=None
highlight NonText ctermbg=None

"================================================================================
" Auto Commands (AC)
"================================================================================

autocmd BufEnter * set formatoptions-=ro

" Set augroup
augroup initvim
    autocmd!

    " Reload init.vim when edited
    autocmd BufWritePost init.vim nested :source ~/.config/nvim/init.vim
augroup END

augroup MyAutoCmd
    autocmd!

    " Delete trailing whitespace
    autocmd FileWritePre   * :call TrimWhiteSpace()
    autocmd FileAppendPre  * :call TrimWhiteSpace()
    autocmd FilterWritePre * :call TrimWhiteSpace()
    autocmd BufWritePre    * :call TrimWhiteSpace()

    " convert tabs to space
    autocmd FileWritePre   * :retab
    autocmd FileAppendPre  * :retab
    autocmd FilterWritePre * :retab
    autocmd BufWritePre    * :retab

    " Set *.pp files to json filetype
    autocmd BufNewFile,BufRead *.pp set filetype=json
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

" Case insensitive search
set ignorecase
set smartcase

" Turn backup off
set nowritebackup
set noswapfile

" Break line at character specified by 'breakat'
set linebreak

" Min # of screen lines above and below cursor
set scrolloff=10

" Min # of screen columns to keep to the left and right of the cursor
set sidescrolloff=20

" Min width of number column
set numberwidth=4

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

set foldmethod=syntax " Fold via syntax of files
set foldlevelstart=99 " Open all folds by default

" Remove the default mode
set noshowmode

" When and how to draw the signcolumn.
set signcolumn=yes

" Faster update time
set updatetime=300

" Keep undo history across sessions, by storing in file.
" Only works all the time.
if has('persistent_undo')
    silent !mkdir $XDG_CACHE_HOME/vim/undo > /dev/null 2>&1
    set undodir=$XDG_CACHE_HOME/vim/undo
    set undofile
endif

"================================================================================
" Leader Key Mappings (LK)
"================================================================================

" Map leader to comma
let mapleader = ","

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

" configure whether buffer numbers should be shown.
let g:airline#extensions#tabline#buffer_nr_show = 1

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
" let g:deoplete#enable_at_startup = 1

" Use smartcase.
call deoplete#custom#option('smart_case', v:true)

"================================================================================
" File Completion Settings (FCS)
"================================================================================

" Javascript
let g:tern_request_timeout = 1
let g:tern_show_signature_in_pum = '0'

let g:tern#filetypes = [ 'jsx', 'javascript.jsx', 'vue' ]

"================================================================================
" ALE (AleS)
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
nnoremap <c-p> :Files<CR>
nnoremap <c-g> :GFiles<CR>
nnoremap <c-o> :Buffer<CR>
nnoremap <c-f> :Rg!

command! -bang -nargs=? -complete=dir Files
    \ call fzf#vim#files(<q-args>, fzf#vim#with_preview({'options': []}), <bang>0)

let g:fzf_layout = { 'window': { 'width': 0.9, 'height': 0.6 } }

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
let g:go_def_mode = 'gopls'
let g:go_info_mode = 'gopls'

"================================================================================
" Vimwiki (VWS)
"================================================================================
let g:vimwiki_list = [{'path': '~/wiki/', 'name': "Jonathan's Wiki", 'auto_toc': 1, 'index': 'main', 'syntax': 'markdown', 'ext': '.md'}]
