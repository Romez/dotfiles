call plug#begin('~/.vim/plugged')

Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-surround'
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-commentary'
Plug 'mattn/emmet-vim', { 'for': ['html', 'javascript' ]}
Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }
Plug 'vim-airline/vim-airline'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'ervandew/supertab'
Plug 'tpope/vim-repeat'
Plug 'ntpeters/vim-better-whitespace'
Plug 'tpope/vim-sensible'
Plug 'nanotech/jellybeans.vim'
Plug 'yardnsm/vim-import-cost', { 'do': 'npm install' }
Plug 'jiangmiao/auto-pairs'
Plug 'leafgarland/typescript-vim'
Plug 'ludovicchabant/vim-lawrencium'
Plug 'mhinz/vim-signify'
Plug 'tpope/vim-fugitive'

call plug#end()

let mapleader = "\<Space>"
nmap <silent> // :nohlsearch<CR>
nnoremap <leader>b :Buffers<CR>

set hidden
set number
set expandtab
set softtabstop=2
set shiftwidth=2
set splitbelow
set cmdheight=2
set signcolumn=yes
set scrolloff=3
set autoindent

silent! colorscheme jellybeans

" format the entire file
nnoremap ff :normal! gg=G``<CR>

" FZF
set rtp+=/usr/local/opt/fzf

" LSP
augroup filetype_typescript
    autocmd!
    autocmd BufReadPost,BufRead,BufNewFile *.ts setlocal filetype=typescript
augroup END
augroup filetype_typescript.tsx
    autocmd!
    autocmd BufReadPost *.tsx setlocal filetype=typescript
augroup END
augroup filetype_scss
  autocmd!
  autocmd BufReadPost *.scss setlocal filetype=css
augroup END
augroup filetype_json
  autocmd!
  autocmd BufReadPost *.json setlocal filetype=json
augroup END

let g:LanguageClient_serverCommands = {
	\ 'javascript': ['typescript-language-server', '--stdio'],
        \ 'typescript': ['typescript-language-server', '--stdio'],
        \ 'css': ['css-languageserver', '--stdio'],
        \ 'json': ['json-language-server --stdio'],
        \ 'cpp': ['clangd'],
        \ 'yaml': ['yaml-language-server', '--stdio']
      \ }

let g:LanguageClient_loggingLevel = 'DEBUG'

nnoremap <buffer> <silent> gd :call LanguageClient#textDocument_definition()<CR>
nnoremap <buffer> <silent> gt :call LanguageClient#textDocument_typeDefinition()<CR>
nnoremap <leader>rn :call LanguageClient#textDocument_rename()<CR>

" NerdTree
nmap <C-m> :NERDTreeFind<CR>
nmap <silent> <leader><leader> :NERDTreeToggle<CR>

" Map ctrl-movement keys to window switching
map <C-k> <C-w><Up>
map <C-j> <C-w><Down>
map <C-l> <C-w><Right>
map <C-h> <C-w><Left>

nnoremap <Leader>w :w<CR>
nnoremap <Leader>o :GFiles .<CR>
nnoremap <leader>fc :Commits<CR>
nnoremap <leader>ff :Files<CR>
nnoremap <leader>fa :Ag<CR>

" Open new split for C+hjkl, if split doesnt exist
map <silent> <C-h> :call WinMove('h')<CR>
map <silent> <C-j> :call WinMove('j')<CR>
map <silent> <C-k> :call WinMove('k')<CR>
map <silent> <C-l> :call WinMove('l')<CR>
function! WinMove(key)
  let t:curwin = winnr()
  exec "wincmd ".a:key
    if (t:curwin == winnr())
      if (match(a:key,'[jk]'))
        wincmd v
      else
        wincmd s
      endif
   exec "wincmd ".a:key
  endif
endfunction

" emmet
let g:user_emmet_leader_key=','
let g:user_emmet_settings = {
\  'javascript' : {
\      'extends' : 'jsx',
\  },
\}

" Use deoplete.
let g:deoplete#enable_at_startup = 1

" ImportCost
augroup import_cost_auto_run
  autocmd!
  autocmd InsertLeave *.js,*.jsx,*.ts,*.tsx ImportCost
  autocmd BufEnter *.js,*.jsx,*.ts,*.tsx ImportCost
  autocmd CursorHold *.js,*.jsx,*.ts,*.tsx ImportCost
augroup END
