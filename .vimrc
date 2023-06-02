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



" PLUGINS ---------------------------------------------------------------- {{{

call plug#begin('~/.vim/plugged')

Plug 'preservim/nerdtree'
Plug 'dense-analysis/ale'

call plug#end()

let g:everforest_background='hard'
let g:everforest_better_performance=1
let g:ale_python_flake8_options='--max-line-length=120'
let g:rainbow_active=1

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



colorscheme everforest
