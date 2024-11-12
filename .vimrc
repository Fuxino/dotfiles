filetype on
filetype plugin on
filetype indent on
syntax on
set nocompatible
set number
set shiftwidth=4
set tabstop=4
set expandtab
set backup
set backupdir=~/.vim/backup/
set directory=/tmp
set scrolloff=10
set incsearch
set showcmd
set showmode
set showmatch
set hlsearch
set wildmenu
set wildmode=list:longest
set wildignore=*.docx,*.jpg,*.png,*.gif,*.pdf,*.pyc,*.exe,*.flv,*.img,*.xlsx,*.mp4,*.zip,*.iso,*.odt,*.tar,*.gz,*.xz,*.bz2,*.zst,*.rar,*.m4a,*.bmp,*.ogg,*.mp3,*.flv
set background=dark
set grepprg=grep\ -nH\ $*



" PLUGINS ---------------------------------------------------------------- {{{

call plug#begin()

Plug 'catppuccin/vim', { 'as': 'catppuccin' }
Plug 'ghifarit53/tokyonight-vim'

call plug#end()

let g:ale_linters={'python': ['pylint'], 'c': ['cc'], 'sh': ['shell'], 'tex': ['lacheck'], 'haskell': ['hlint']}
let g:ale_linters_explicit=1
let g:ale_enabled=1
let g:ale_python_pylint_options='--max-line-length=160 --disable=missing-docstring --disable=invalid-name'
" let g:everforest_background='hard'
" let g:everforest_better_performance=1
let g:tokyonight_style='storm'
let g:tokyonight_enable_italic=1
let g:rainbow_active=1
let g:tex_flavor='latex'

" }}}



" MAPPINGS --------------------------------------------------------------- {{{

" Mappings code goes here.

" }}}



" VIMSCRIPT -------------------------------------------------------------- {{{

" This will enable code folding.
" Use the marker method of folding.
augroup filetype_vim
    autocmd!
    autocmd FileType vim setlocal foldmethod=marker
augroup END


if has('termguicolors')
    set termguicolors
endif


au BufRead /tmp/mutt-* set tw=144

" }}}



" STATUS LINE ------------------------------------------------------------ {{{

" Status bar code goes here.

" }}}

" colorscheme catppuccin_frappe
colorscheme tokyonight
