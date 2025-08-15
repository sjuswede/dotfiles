" basic vimrc
" further config in per filetype files under .vim/ftplugin
set title
set noerrorbells

filetype plugin indent on

call plug#begin("~/.local/share/vim-plugins")

Plug 'dpelle/vim-LanguageTool' 
Plug 'ron89/thesaurus_query.vim' 
Plug 'junegunn/goyo.vim' 
Plug 'junegunn/limelight.vim' 
Plug 'reedes/vim-pencil' 
Plug 'reedes/vim-wordy'

Plug 'jceb/vim-orgmode'

Plug 'godlygeek/tabular'
Plug 'preservim/vim-markdown'

call plug#end()

if executable('rg')
    let g:rg_derive_root='true'
endif


