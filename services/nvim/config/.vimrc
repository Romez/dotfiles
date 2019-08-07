call plug#begin('~/.vim/plugged')

Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-surround'
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-commentary'
Plug 'mattn/emmet-vim', { 'for': ['html', 'javascript' ]}
" Plug 'autozimu/LanguageClient-neovim', {
"     \ 'branch': 'next',
"     \ 'do': 'bash install.sh',
"     \ }
Plug 'vim-airline/vim-airline'
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
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'pangloss/vim-javascript'

call plug#end()

let mapleader = "\<Space>"
nmap <silent> // :nohlsearch<CR>
nnoremap <leader>b :Buffers<CR>

" Don't redraw while executing macros (good performance config)
set lazyredraw
set ttyfast

set hidden
set number
set expandtab
set softtabstop=2
set shiftwidth=2
set splitbelow
set cmdheight=2
set scrolloff=3
set autoindent
set foldmethod=syntax

silent! colorscheme jellybeans

" format the entire file
nnoremap ff :normal! gg=G``<CR>

" https://github.com/vim/vim/blob/master/runtime/doc/russian.txt
" Enable hotkeys for Russian layout
set langmap=ФИСВУАПРШОЛДЬТЩЗЙКЫЕГМЦЧНЯ;ABCDEFGHIJKLMNOPQRSTUVWXYZ,фисвуапршолдьтщзйкыегмцчня;abcdefghijklmnopqrstuvwxyz

" Turn off linewise keys. Normally, the `j' and `k' keys move the cursor down one entire line. with line wrapping on, this can cause the cursor to actually skip a few lines on the screen because it's moving from line N to line N+1 in the file. I want this to act more visually -- I want `down' to mean the next line on the screen
nmap j gj
nmap k gk

vmap <leader>y :w! /tmp/.vbuf<CR>
nmap <leader>y :.w! /tmp/.vbuf<CR>
nmap <leader>p :r /tmp/.vbuf<CR>

" coc
nmap <silent> rn <Plug>(coc-rename)
nmap <silent> gr <Plug>(coc-references)
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)

" FZF
set rtp+=/usr/local/opt/fzf
command! -bang -nargs=* Ag
  \ call fzf#vim#ag(
  \   '',
  \   <bang>0 ? fzf#vim#with_preview('up:60%')
  \           : fzf#vim#with_preview('right:50%:hidden', '?'),
  \   <bang>0)
let g:fzf_layout = { 'window': 'enew' }

"" LSP
" augroup filetype_typescript
"     autocmd!
"     autocmd BufReadPost,BufRead,BufNewFile *.ts setlocal filetype=typescript
" augroup END
" augroup filetype_typescript.tsx
"     autocmd!
"     autocmd BufReadPost *.tsx setlocal filetype=typescript
" augroup END
" augroup filetype_scss
"   autocmd!
"   autocmd BufReadPost *.scss setlocal filetype=css
" augroup END
" augroup filetype_json
"   autocmd!
"   autocmd BufReadPost *.json setlocal filetype=json
" augroup END

" let g:LanguageClient_serverCommands = {
" 	\ 'javascript': ['typescript-language-server', '--stdio'],
"         \ 'typescript': ['typescript-language-server', '--stdio'],
"         \ 'css': ['css-language-server', '--stdio'],
"         \ 'json': ['json-language-server --stdio'],
"         \ 'cpp': ['clangd'],
"         \ 'yaml': ['yaml-language-server', '--stdio'],
"         \ 'dockerfile': ['docker-langserver --stdio']
"       \ }

" let g:LanguageClient_loggingLevel = 'DEBUG'

" nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
" nnoremap <silent> gt :call LanguageClient#textDocument_typeDefinition()<CR>
" nnoremap <leader>rf :call LanguageClient#textDocument_references()<CR>
" nnoremap <leader>rn :call LanguageClient#textDocument_rename()<CR>

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

" ImportCost
augroup import_cost_auto_run
  autocmd!
  autocmd InsertLeave *.js,*.jsx,*.ts,*.tsx ImportCost
  autocmd BufEnter *.js,*.jsx,*.ts,*.tsx ImportCost
  autocmd CursorHold *.js,*.jsx,*.ts,*.tsx ImportCost
augroup END
