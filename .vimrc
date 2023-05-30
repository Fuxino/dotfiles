set nocompatible
filetype on
filetype plugin on
filetype indent on
syntax on
set number
set shiftwidth=4
set tabstop=4
set expandtab
set nobackup
set scrolloff=10
set nowrap
set incsearch
set showcmd
set showmode
set showmatch
set hlsearch
set wildmenu
set wildmode=list:longest
set wildignore=*.docx,*.jpg,*.png,*.gif,*.pdf,*.pyc,*.exe,*.flv,*.img,*.xlsx

if has('termguicolors')
    set termguicolors
endif

set background=dark

let g:everforest_background='hard'
let g:everforest_better_performance=1


" PLUGINS ---------------------------------------------------------------- {{{

call plug#begin('~/.vim/plugged')

Plug 'preservim/nerdtree'

call plug#end()


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


au BufRead /tmp/mutt-* set tw=144

" }}}



" STATUS LINE ------------------------------------------------------------ {{{

" Status bar code goes here.

" }}}



colorscheme everforest
