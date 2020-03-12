""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" Basic Setup
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Required:
" I like to use like this because its more declarative
" PS: the filetype plugin indent on command is like a combination of these commands:
" filetype plugin indent on
filetype on
filetype plugin on
filetype indent on

"" Encoding
set encoding=utf-8
set fileencoding=utf-8
set fileencodings=utf-8
set ttyfast

"" Fix backspace indent
set backspace=indent,eol,start

"" Tabs. May be overridden by autocmd rules
set tabstop=2
set softtabstop=0
set shiftwidth=2
set expandtab

"" Map leader
let g:mapleader="\<Space>"
let g:maplocalleader=","

"" Enable hidden buffers
set hidden

"" Searching
set hlsearch
set incsearch
set ignorecase
set smartcase

set fileformats=unix,dos,mac

if exists('$SHELL')
  set shell=$SHELL
else
  set shell=/bin/sh
endif


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" Visual Settings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
syntax on
set ruler
set number
" set relativenumber

set cursorline              " Highlight cursor line

let no_buffers_menu=1

set timeoutlen=500 " The time in milliseconds that is waited for a key code or mapped key sequence to complete.
set ttimeoutlen=50                   " ms to wait for next key in a sequence
set foldcolumn=1 " Add a bit extra margin to the left
set relativenumber     " show line numbers relative to current line

" Enable mouse. see :help mouse for info.
set mouse=a

" Highlight matching pairs
set showmatch

" Permite mover o cursor onde não há texto
set virtualedit=insert,block,onemore


" How many tenths of a second to blink when matching brackets
set matchtime=2

" Maximum number of items to show in the popup menu
set pumheight=15
" popoup menu maximum width (default 15)
" set pumwidth=

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" Colorschemes
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Codedark
if empty(glob('~/.config/vim/colors/codedark.vim'))
	silent !curl -fLo ~/.config/vim/colors/codedark.vim --create-dirs
				\ https://raw.githubusercontent.com/tomasiser/vim-code-dark/master/colors/codedark.vim
endif

silent! colorscheme default

set mousemodel=popup
set t_Co=256
set guioptions=egmrti
set gfn=Monospace\ 10

if has("gui_running")
  if has("gui_mac") || has("gui_macvim")
    set guifont=Menlo:h12
    set transparency=7
  endif
else
  let g:CSApprox_loaded = 1

  " IndentLine
  let g:indentLine_enabled = 1
  let g:indentLine_concealcursor = 0
  let g:indentLine_char = '┆'
  let g:indentLine_faster = 1


  if $COLORTERM == 'gnome-terminal'
    set term=gnome-256color
  else
    if $TERM == 'xterm'
      set term=xterm-256color
    endif
  endif

endif

if &term =~ '256color'
  set t_ut=
endif

" change cursor shape according to mode
autocmd VimEnter * silent exec "! echo -ne '\e[1 q'"
autocmd VimLeave * silent exec "! echo -ne '\e[5 q'"

"" Disable the blinking cursor.
set guicursor=a:blinkon0

"" Disable the blinking cursor.
set gcr=a:blinkon0

"" Fix cursor at center of screen while scrolling
set scrolloff=1000

"" Use modeline overrides
set modeline
set modelines=10

set title
set titleold="Terminal"
set titlestring=%F

set statusline=%F%m%r%h%w%=(%{&ff}/%Y)\ (line\ %l\/%L,\ col\ %c)\

" Search mappings: These will make it so that going to the next one in a
" search will center on the line it's found in.
nnoremap n nzzzv
nnoremap N Nzzzv

" Transparent backgroud
highlight! Normal ctermbg=NONE guibg=NONE
highlight! NonText ctermbg=NONE guibg=NONE guifg=NONE ctermfg=NONE


set display+=lastline


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Tabline
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

hi TabLine      ctermfg=Black  ctermbg=Green     cterm=NONE
hi TabLineFill  ctermfg=Black  ctermbg=Green     cterm=NONE
hi TabLineSel   ctermfg=White  ctermbg=DarkBlue  cterm=NONE

" To enable the close button in the upper right corner, add the following to your ~/.vimrc
let g:tablineclosebutton=1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" Status Line
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set laststatus=2
set noshowmode " Hide insertStatus (like --INSERT--) fromthe statsline
set showcmd                          " show last command in the status line

if exists("*fugitive#statusline")
  set statusline+=%{fugitive#statusline()}
endif



""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" Abbreviations
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"" no one is really happy until you have this shortcuts
cnoreabbrev W! w!
cnoreabbrev Q! q!
cnoreabbrev Qall! qall!
cnoreabbrev Wq wq
cnoreabbrev Wa wa
cnoreabbrev wQ wq
cnoreabbrev WQ wq
cnoreabbrev W w
cnoreabbrev Q q
cnoreabbrev Qall qall

set ffs=unix,dos,mac   " Use Unix as the standard file type

" this was preventing the bookmark item to stay as the pwd and me using fzf project search
" set autochdir          " set current directory to recently opened file



  set nocompatible                     " not compatible with Vi
  set background=dark
  set cscopeverbose                    " bservose cscope output
  set complete-=i                      " don't scan current on included files for completion
  set display=lastline                 " display more message text
  set formatoptions=tcqj               " more intuitive autoformatting
  set fsync                            " call fsync() for robust file saving
  set history=10000                    " longest possible command history
  set langnoremap                      " helps avoid mapings breaking
  set listchars=tab:>\ ,trail:-,nbsp:+ " chars for :list
  set nrformats=bin,hex                " <c-a> and <c-x> support
  set sessionoptions-=options         " do not carry options across sessions
  set shortmess=F                      " less verbose file info
  set sidescroll=1                     " smoother sideways scrolling
  set smarttab                         " tab settings aware <Tab> key
  set tabpagemax=50                    " maximum number of tabs open by -p flag
  set tags=./tags;,tags                " filenames to look for the tag command
  set viminfo+=!                       " save global variables across sessions


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" terminal and shell configuration
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

if exists('$SHELL')
  set shell=$SHELL
else
  set shell=/bin/sh
endif

nnoremap <silent> <leader>sh :terminal<CR> " terminal emulation


" separators used in the interface, like the windows borders
set fillchars=vert:\|,fold:-          " separator characters

hi vertsplit guifg=fg guibg=bg


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" command-line completion
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set wildmenu                         " enhanced command line completion
set wildmode=list:longest,list:full
" set wildmode=full    " command-line completion enhanced mode
" set wildmode=list:longest  " Make wildmenu behave like bash completion. Finding commands are so easy now.

set cmdheight=2      " Height of the command bar

" List of files to be ignored in command line completions for file and dir names
set wildignore+=*.ai,*.bmp,*.gif,*.ico,*.jpg,*.jpeg,*.png,*.psd,*.webp
set wildignore+=*.aux,*.out,*.toc
set wildignore+=*.avi,*.divx,*.mp4,*.webm,*.mov,*.m2ts,*.mkv,*.vob,*.mpg,*.mpeg
set wildignore+=*.doc,*.pdf,*.cbr,*.cbz,*.docx,*.ppt,*.odt
set wildignore+=*.eot,*.otf,*.ttf,*.woff
set wildignore+=*.mp3,*.oga,*.ogg,*.wav,*.flac
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest,*.rbc,*.class,*.jar,*.iso
set wildignore+=*.swp,.lock,.DS_Store,._*
set wildignore+=*.zip,*.tar.gz,*.tar.bz2,*.rar,*.tar.xz,*.kgb
set wildignore+=.git,.hg,.svn
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.pyc,*.db,*.sqlite


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" Commands
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"

" Some usefull info on commands
" -nargs makes the new command be able to take an argument like :Command arg1
" -bang makes the command be able to interpret a ! like in :Command!
" The ! right after command is used to tell vim to redefine the command if it already exists, this way when you source your .vimrc you don't have an error message.

" remove trailing whitespaces
command! FixWhitespace :%s/\s\+$//e

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" Functions
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
if !exists('*s:setupWrapping')
  function s:setupWrapping()
    set wrap
    set wm=2
    set textwidth=79
  endfunction
endif

"--------------------------------------------
"
" align() function  similar to emacs align-regexp
"source: https://vim.fandom.com/wiki/Regex-based_text_alignment
function! AlignSection(regex) range
  let extra = 1
  let sep = empty(a:regex) ? '=' : a:regex
  let maxpos = 0
  let section = getline(a:firstline, a:lastline)
  for line in section
    let pos = match(line, ' *'.sep)
    if maxpos < pos
      let maxpos = pos
    endif
  endfor
  call map(section, 'AlignLine(v:val, sep, maxpos, extra)')
  call setline(a:firstline, section)
endfunction

function! AlignLine(line, sep, maxpos, extra)
  let m = matchlist(a:line, '\(.\{-}\) \{-}\('.a:sep.'.*\)')
  if empty(m)
    return a:line
  endif
  let spaces = repeat(' ', a:maxpos - strlen(m[1]) + a:extra)
  return m[1] . spaces . m[2]
endfunction

command! -nargs=? -range Align <line1>,<line2>call AlignSection('<args>')

"--------------------------------------------

function! VisualSelection(direction, extra_filter) range
  let l:saved_reg = @"
  execute "normal! vgvy"

  let l:pattern = escape(@", '\\/.*$^~[]')
  let l:pattern = substitute(l:pattern, "\n$", "", "")

  if a:direction == 'b'
    execute "normal ?" . l:pattern . "^M"
  elseif a:direction == 'gv'
    call CmdLine("Ack \"" . l:pattern . "\" " )
  elseif a:direction == 'replace'
    call CmdLine("%s" . '/'. l:pattern . '/')
  elseif a:direction == 'f'
    execute "normal /" . l:pattern . "^M"
  endif

  let @/ = l:pattern
  let @" = l:saved_reg
endfunction

"--------------------------------------------

" Returns true if paste mode is enabled (used by statusline)
function! HasPaste()
  if &paste
    return 'PASTE MODE  '
  endif
  return ''
endfunction

"--------------------------------------------

" Zoom / Restore window.
function! s:ZoomToggle() abort
  if exists('t:zoomed') && t:zoomed
    execute t:zoom_winrestcmd
    let t:zoomed = 0
  else
    let t:zoom_winrestcmd = winrestcmd()
    resize
    vertical resize
    let t:zoomed = 1
  endif
endfunction
command! ZoomToggle call s:ZoomToggle()

" mapping the ZoomToggle function
noremap <leader>z :ZoomToggle<CR>


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" Autocmd Rules
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Maintain cwd for every opened file
autocmd InsertEnter * let save_cwd = getcwd() | set autochdir
autocmd InsertLeave * set noautochdir | execute 'cd' fnameescape(save_cwd)

"" The PC is fast enough, do syntax highlight syncing from start unless 200 lines
augroup vimrc-sync-fromstart
  autocmd!
  autocmd BufEnter * :syntax sync maxlines=200
augroup END

"" Remember cursor position
augroup vimrc-remember-cursor-position
  autocmd!
  autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
augroup END

"" txt
augroup vimrc-wrapping
  autocmd!
  autocmd BufRead,BufNewFile *.txt call s:setupWrapping()
augroup END

"" make/cmake
augroup vimrc-make-cmake
  autocmd!
  autocmd FileType make setlocal noexpandtab
  autocmd BufNewFile,BufRead CMakeLists.txt setlocal filetype=cmake
augroup END

"" Remember cursor position
augroup vimrc-remember-cursor-position
  autocmd!
  autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
augroup END

set autoread


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" My Mappings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Examples of how mappings work
" Source https://vim.fandom.com/wiki/Mapping_keys_in_Vim_-_Tutorial_(Part_1)

" :nmap - Display normal mode maps
" :imap - Display insert mode maps
" :vmap - Display visual and select mode maps
" :smap - Display select mode maps
" :xmap - Display visual mode maps
" :cmap - Display command-line mode maps
" :omap - Display operator pending mode maps

" Fast saving
nmap <leader>w :w!<cr>
" :W sudo saves the file
command! W w !sudo tee % > /dev/null

" Fast quitting
nmap <leader>q :q!<cr>

" quicly edit/reload configuration file
nnoremap cvo :e ~/.vimrc<CR>
nnoremap cvs :so ~/.vimrc<CR>
nnoremap cno :e ~/.config/nvim/init.vim<CR>
nnoremap cns :so ~/.config/nvim/init.vim<CR>

" Comment line with C-/ like in VSCode
" for some reason vim ses C-/ as C-_
nmap <C-_> gcc
" gv restores the last visual selection made
vmap <C-_> gcgv

" Retain visual selection after indenting a block with > or <
vnoremap > >gv
vnoremap < <gv

"" Quick exit insert mode with jk
inoremap jk <ESC>

" Go to beginning and end of lines more easilyt/audo
nnoremap H ^
nnoremap L $
" View git history for file
nnoremap <F9> :AgitFile <Cr>


"" Opens an edit command with the path of the currently edited file filled in
noremap <leader>ty :e <C-R>=expand("%:p:h") . "/" <CR>

" Easy Align plugin
" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

vnoremap <silent> <Leader>al :Align<CR>

"" Git
noremap <Leader>ga :Gwrite<CR>
noremap <Leader>gc :Gcommit<CR>
noremap <Leader>gsh :Gpush<CR>
noremap <Leader>gll :Gpull<CR>
noremap <Leader>gs :Gstatus<CR>
noremap <Leader>gb :Gblame<CR>
noremap <Leader>gd :Gvdiff<CR>
noremap <Leader>gr :Gremove<CR>


"" Vmap for maintain Visual Mode after shifting > and <
vmap < <gv
vmap > >gv

"" Open current line on GitHub
nnoremap <leader>go :.Gbrowse<CR>


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set shortmess+=c " Don't pass messages to |ins-completion-menu|.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" session settings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let g:session_directory = "~/.vim/session"
let g:session_autoload = "no"
let g:session_autosave = "no"
let g:session_command_aliases = 1

nnoremap <leader>so :OpenSession<Space>
nnoremap <leader>ss :SaveSession<Space>
nnoremap <leader>sd :DeleteSession<CR>
nnoremap <leader>sc :CloseSession<CR>
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" Buffer settings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" set hidden " Enable hidden buffers. Coc Observation about this: TextEdit might fail if hidden is not set.
set switchbuf=useopen  " don't duplicate an existing open buffer


" Specify the behavior when switching between buffers
try
  set switchbuf=useopen,usetab,newtab
  set stal=2
catch
endtry

" List buffers
nnoremap <leader>bl :Buffers<CR>
" previous buffer
noremap <leader>bp :bprevious<CR>
" next buffer
noremap <leader>bn :bnext<CR>
" Close the current buffer
map <leader>bc :Bclose<cr>:tabclose<cr>gT
" noremap <leader>bc :bdelete<CR>
"" Close all the buffers
map <leader>ba :bufdo bd<cr>
" Switch CWD to the directory of the open buffer
map <leader>cd :cd %:p:h<cr>:pwd<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Clipboard
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set paste               " Paste from a windows or from vim
set go+=a               " Visual selection automatically copied to the clipboard

" On Mac OS X and Windows, the * and + registers both point to the system clipboard so unnamed and unnamedplus have the same effect: the unnamed register is synchronized with the system clipboard.
" On Linux, you have essentially two clipboards: one is pretty much the same as in the other OSes (CtrlC and CtrlV in other programs, mapped to register + in Vim), the other is the "selection" clipboard (mapped to register * in Vim).

" Using only unnamedplus on Linux, Windows and Mac OS X allows you to:
" * CtrlC in other programs and put in Vim with p on all three platforms,
" * yank in Vim with y and CtrlV in other programs on all three platforms.
" * If you also want to use Linux's "selection" clipboard, you will also need unnamed.

" Here is a cross-platform value:
set clipboard^=unnamed,unnamedplus

"   set clipboard=unnamed,unnamedplus
" if has('unnamedplus')
" endif

noremap YY "+y<CR>
noremap <leader>p "+gP<CR>
noremap XX "+x<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set whichwrap+=<,>,h,l,[,]     " wrap movement whenreaching startor endofline
set lazyredraw                 " Don't redraw while executing macros (good performance config)
set magic                      " For regular expressions turn magic on


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" SYNTAX and INDENTATION
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" enables native filetype syntax highlighting base on file type
set autoindent               " Auto-indent new lines
set smartindent             " Enable smart-indent


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Undo | Redo
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set history=10000  " Sets how many lines of history VIM has to remember
set undolevels=1500            " Number of undo levels

"" Persistent Undo
if !isdirectory($HOME."/.vim/undo-dir")
  call mkdir($HOME."/.vim/undo-dir", "", 0700)
endif
set undodir=~/.config/vim/undo-dir
set undofile


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" WHITESPACE SETTINGS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Delete trailing whitespace on save:
autocmd BufWritePre * %s/\s\+$//e

" Show trailing spaces as dots
" set list listchars=trail:·,tab:>·

" highlight trailing whitespace but prevent highlighting while in the insert mode.
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/
autocmd BufWinLeave * call clearmatches()

highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/
autocmd BufWinLeave * call clearmatches()



""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" TEXT EDITING SETTINGS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" 0 to go to the first non-blank character in the beggining of the line instead of first whitespace
map 0 ^

" Return to last edit position when opening files (You want this!)
" au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
" Restore last cursor position and marks on open
au BufReadPost *
      \ if line("'\"") > 1 && line("'\"") <= line("$") && &ft !~# 'commit'
      \ |   exe "normal! g`\""
      \ | endif

set showbreak=+++           " Wrap-broken line prefix
set textwidth=110                    " Line wrap (number of cols)
set linebreak                     " Break lines at word (requires Wrap lines)


" Move text with M-hjkl
" this has to use these wierd characters because terminals cant recognize alt keys
" these were tested in alacritty, st and uxrvt (i3-sensible-terminal)
nnoremap j :m .+1<CR>==
nnoremap k :m .-2<CR>==
inoremap j <Esc>:m .+1<CR>==gi
inoremap k <Esc>:m .-2<CR>==gi
vnoremap j :m '>+1<CR>gv=gv
vnoremap k :m '<-2<CR>gv=gv
"" Move visual block
vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '<-2<CR>gv=gv


" replace all instances of currently selected word for new input (leader + r)
map <leader>r :%s///g<left><left>
vnoremap <C-r> "hy:%s/<C-r>h//gc<left><left><left>


" Disable Vim native text folding
" set nofoldenable

" ~    : Changes the case of current character
" guu  : Change current line from upper to lower.
" gUU  : Change current LINE from lower to upper.
" guw  : Change to end of current WORD from upper to lower.
" guaw : Change all of current WORD to lower.
" gUw  : Change to end of current WORD from lower to upper.
" gUaw : Change all of current WORD to upper.
" g~~  : Invert case to entire line
" g~w  : Invert case to current WORD
" guG : Change to lowercase until the end of document.



""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" SEARCH SETTINGS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Makes Esc clear last search highlights after searching
nnoremap <esc> :noh<return><esc>

" Disable highlight when <leader><cr> is pressed
map <silent> <leader><cr> :noh<cr>

" Visual mode pressing * or # searches for the current selection
" Super useful! From an idea by Michael Naumann
vnoremap <silent> * :<C-u>call VisualSelection('', '')<CR>/<C-R>=@/<CR><CR>
vnoremap <silent> # :<C-u>call VisualSelection('', '')<CR>?<C-R>=@/<CR><CR>

" Search mappings: These will make it so that going to the next one in a
" search will center on the line it's found in.
nnoremap n nzzzv
nnoremap N Nzzzv

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" SPELL CHECKING
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set spelllang=pt_br,en_us

" Toggle and untoggle spell checking
map <leader>ss :setlocal spell!<cr>

" Spellchecking shortcuts
map <leader>sn ]s
map <leader>sp [s
map <leader>sa zg
map <leader>s? z=


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" FILES, BACKUPS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set nobackup
set nowritebackup
set noswapfile

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" TABS SETTINGS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"" Opens a tab edit command with the path of the currently edited file filled
noremap <leader>te :tabe <C-R>=expand("%:p:h") . "/" <CR>

" navigate to tabs using <leader><number>, similar to i3 workspaces switch
nnoremap <leader>1 1gt
nnoremap <leader>2 2gt
nnoremap <leader>3 3gt
nnoremap <leader>4 4gt
nnoremap <leader>5 5gt

" Useful mappings for managing tabs
map <leader>tn :tabnew<cr>
map <leader>to :tabonly<cr>
map <leader>tc :tabclose<cr>
map <leader>tm :tabmove
map <leader>t<leader> :tabnext

" Let 'tl' toggle between this and the last accessed tab
let g:lasttab = 1
nmap <leader>tl :exe "tabn ".g:lasttab<CR>
au TabLeave * let g:lasttab = tabpagenr()

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" WINDOWS SPLTTING
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Setas redimensionam janelas adjacentes
nnoremap <left> :vertical resize -5<cr>
nnoremap <right> :vertical resize +5<cr>
nnoremap <up> :resize -5<cr>
nnoremap <down> :resize +5<cr>

" Open new split panes to right and bottom, which feels more natural than Vim’s default:
set splitbelow
set splitright

" horizontal splits
noremap <silent> <leader>sh :<C-u>split<CR>
noremap <silent> <leader>- :<C-u>split<CR>

" vertical split
noremap <silent> <leader>sv :<C-u>vsplit<CR>
noremap <silent> <leader>\| :<C-u>vsplit<CR>

"" Split

"" Switching windows with C-hjkl
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l
noremap <C-h> <C-w>h

" Switching windows with C-HJKL
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" TMUX & VIM STUFF
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

autocmd BufEnter * call system("tmux rename-window " . expand("%:t"))
autocmd VimLeave * call system("tmux setw automatic-rename")
autocmd BufEnter * let &titlestring = ' ' . expand("%:t")

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" netrw bindings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" make sure netrw wont be configured in NeoVim, which is using nerdtree
	" rmeove the useless banner
	let g:netrw_banner = 0

	" use tree-view (press i to alter view style)
	let g:netrw_liststyle = 3

	" open splits to the right
	let g:netrw_altv = 1

	" If you’ve set custom wildignores netrw can inherit them by doing this:
	let g:netrw_list_hide = &wildignore

	" " Open netrw on vim enter
	" augroup ProjectDrawer
	"   autocmd!
	"   autocmd VimEnter * :Vexplore
	" augroup END

	" Change how files are opened
	" 1 - open files in a new horizontal split
	" 2 - open files in a new vertical split
	" 3 - open files in a new tab
	" 4 - open in previous window
	let g:netrw_browse_split = 4

	" Netrw width
	let g:netrw_winsize = 25 " that means 25% of the current view (page / window)


	" Mapping q to close netrw is easily solved:
	autocmd FileType netrw nnoremap q :bd<CR>
	" (Or if you want the split it's in to stay open:)
	autocmd FileType netrw nnoremap q :bp\|bd #<CR>


	let g:NetrwIsOpen=0

	function! ToggleNetrw()
		if g:NetrwIsOpen
			let i = bufnr("$")
			while (i >= 1)
				if (getbufvar(i, "&filetype") == "netrw")
					silent exe "bwipeout " . i
				endif
				let i-=1
			endwhile
			let g:NetrwIsOpen=0
		else
			let g:NetrwIsOpen=1
			silent Lexplore
		endif
	endfunction

	" Add your own mapping. For example:
	noremap <silent> <F4> :call ToggleNetrw()<CR>


	" supposed improved version of the above toggle netrw function
	" try it out
	" Based on alwc's answer without using global state. This way it doesn't get out of sync when Netrw was open manually first. It will still "glitch" when opening Netrw manully after it was opend with this function. But there is nothing one can do about manual commands I guess.

	function! ToggleNetrw2()
		let i = bufnr("$")
		let wasOpen = 0
		while (i >= 1)
			if (getbufvar(i, "&filetype") == "netrw")
				silent exe "bwipeout " . i
				let wasOpen = 1
			endif
			let i-=1
		endwhile
		if !wasOpen
			silent Lexplore
		endif
	endfunction
	map <F8> :call ToggleNetrw2() <CR>

	" open in current window
	" map <F3> :Explore<CR>
	" vertical split
	" map <F4> :Vexplore<CR>
	" map <S-F4> :Vexplore<CR>
	" horizontal split
	map <F7> :Sexplore<CR>

