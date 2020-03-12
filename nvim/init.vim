" ############################################
" my neovim config
" ############################################

" " to use .vimrc instead

" set runtimepath^=~/.vim runtimepath+=~/.vim/after
" let &packpath = &runtimepath
" source ~/.vimrc

let vimplug_exists=expand('~/.vim/autoload/plug.vim')

if !filereadable(vimplug_exists)
	if !executable("curl")
		echoerr "You have to install curl or first install vim-plug yourself!"
		execute "q!"
	endif
	echo "Installing Vim-Plug..."
	echo ""
	silent exec "!\curl -fLo " . vimplug_exists . " --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"
	let g:not_finish_vimplug = "yes"

	autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugins
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" on vim-plug github page it mentions this part should be at the top of the config

call plug#begin("~/.config/nvim/plugged")

" FZF
if isdirectory('~/.fzf')
	Plug '~/.fzf' | Plug 'junegunn/fzf.vim'
else
	Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }
	Plug 'junegunn/fzf.vim'
endif

Plug 'neoclide/coc.nvim', {'branch': 'release'}

Plug 'dense-analysis/ale'

Plug 'scrooloose/nerdtree'
Plug 'jistr/vim-nerdtree-tabs'
Plug 'Xuyuanp/nerdtree-git-plugin'

Plug 'tpope/vim-commentary'           " use with 'gc', equivalent to vscode ctrl+/)
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'               " so that vim-surroud actions will be repeatable with dot command .
Plug 'tpope/vim-rsi'
Plug 'terryma/vim-multiple-cursors'   " Install vim-multiple-cursors (equivalent to vscode ctrl+d)
Plug 'unblevable/quick-scope'         " Highlight jump characters

" Visual plugins
Plug 'vim-scripts/CSApprox'           " make gvim only colorschemes work on terminal
Plug 'RRethy/vim-illuminate'          " illuminate other uses of current word under cursor
Plug 'sheerun/vim-polyglot'
Plug 'Yggdroot/indentLine'

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

Plug 'mkitt/tabline.vim'              " Cleaner tabs

" trying colorizer with named colors first, since some terminal apps use it to define colors
Plug 'ap/vim-css-color'               "show colored hex, rgb(a) hls(a) AND named colors (ex: red)
" Plug 'etdev/vim-hexcolor' "show colored hex, rgb(a) hls(a) BUT NOT named colors (ex: red)
Plug 'luochen1990/rainbow'            " Rainbow parenthesis improved
Plug 'ryanoasis/vim-devicons'         " Icons for dev file types. Has to be loaded after airline

Plug 'Shougo/vimproc.vim', {'do': 'make'}

Plug 'metakirby5/codi.vim'

" ultisnips was remapping C-tab (which vim interprets as C-i) and thus i couldnt jump forward the jump list anylonger
" Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

" Colorscheme
Plug 'tomasiser/vim-code-dark'
Plug 'dunstontc/vim-vscode-theme'
Plug 'chriskempson/base16-vim'

" Git
Plug 'tpope/vim-git'
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'

" Auto closing of pairs
Plug 'jiangmiao/auto-pairs'
" " This one sounds great for ruby and vimscript autoclosing
Plug 'tpope/vim-endwise'

" jump to matching pairs and highlight them (modern replacement for matchit and matchparen)
Plug 'andymass/vim-matchup'

Plug 'terryma/vim-expand-region'

Plug 'valloric/MatchTagAlways'        " Show matching and closing tags

Plug 'gabesoft/vim-ags'               " ag, rg support

" Emacs port of dumb-jump
Plug 'pechorin/any-jump.vim'

Plug 'tpope/vim-eunuch'               " Vim sugar for the UNIX shell commands, like :Delete to delete buffer and file at same time, SudoWrite, etc...

Plug 'christoomey/vim-tmux-navigator'

Plug 'junegunn/vim-easy-align'

" Languages Plugins

" React
Plug 'mxw/vim-jsx'                    " JSX syntax colors and indent support. Depends on vim-javascript
Plug 'xojs/vim-xo'                    " Install vim-xo for xo linting support
" Plug 'prettier/vim-prettier', { 'do': 'yarn install' }
Plug 'editorconfig/editorconfig-vim'

" Javascript and Typescript
Plug 'pangloss/vim-javascript'
Plug 'jelera/vim-javascript-syntax'   " used in vim-boostrap

" typescript
Plug 'leafgarland/typescript-vim'     " used by vim-boostrap
Plug 'HerringtonDarkholme/yats.vim'
" Plug 'heavenshell/vim-tslint'

" Go
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }

" Elixir
Plug 'elixir-editors/vim-elixir'
Plug 'carlosgaldino/elixir-snippets'
Plug 'slashmili/alchemist.vim'
Plug 'mhinz/vim-mix-format'

" erlang
Plug 'jimenezrick/vimerl'

" haskell
Plug 'eagletmt/neco-ghc'
Plug 'dag/vim2hs'
Plug 'pbrisbin/vim-syntax-shakespeare'

" Ruby
Plug 'vim-ruby/vim-ruby'
Plug 'tpope/vim-rails'
Plug 'tpope/vim-rake'
Plug 'sunaku/vim-ruby-minitest'
Plug 'thoughtbot/vim-rspec'
Plug 'tpope/vim-bundler'
Plug 'ecomba/vim-ruby-refactoring'

" rust
Plug 'racer-rust/vim-racer'
Plug 'rust-lang/rust.vim'

" html
Plug 'hail2u/vim-css3-syntax'
Plug 'tpope/vim-haml'
Plug 'mattn/emmet-vim'

" Markdown
Plug 'plasticboy/vim-markdown'
Plug 'suan/vim-instant-markdown'      " Markdown preview instant-markdown-
Plug 'godlygeek/tabular'              " Tabular align texts that grow


call plug#end() " Initialize plugin system


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" Basic Setup
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Required:
" PS: the filetype plugin indent on command is like a combination of these commands:
" filetype plugin indent on
" I like to use like this because its more declarative
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

" " Tomorrow Night
" if empty(glob("~/.vim/colors/base16-tomorrow-night.vim"))
" 	silent !curl -fLo ~/.vim/colors/base16-tomorrow-night.vim --create-dirs
" 				\ https://raw.githubusercontent.com/chriskempson/base16-vim/master/colors/base16-tomorrow-night.vim
" endif

" " Tomorrow Night Eighties
" if empty(glob("~/.vim/colors/base16-tomorrow-night-eighties.vim"))
" 	silent !curl -fLo ~/.vim/colors/base16-tomorrow-night-eighties.vim --create-dirs
" 				\ https://raw.githubusercontent.com/chriskempson/base16-vim/master/colors/base16-tomorrow-night-eighties.vim
" endif

" Tomorrow Night Bright
" This theme was not moved to the new repository on base16 so i download it from the original location
if empty(glob("~/.vim/colors/Tomorrow-Night-Bright.vim"))
	silent !curl -fLo ~/.vim/colors/Tomorrow-Night-Bright.vim --create-dirs
				\ https://raw.githubusercontent.com/chriskempson/tomorrow-theme/master/vim/colors/Tomorrow-Night-Bright.vim
endif

silent! colorscheme codedark
" colorscheme dark_plus

" Load colorscheme based on file extensions
" autocmd BufEnter * colorscheme codedark
" autocmd BufEnter *.html colorscheme Tomorrow-Night-Bright

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
"" Smart Tab Key
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" On visual mode, tab indents
vmap <Tab> >
vmap <S-Tab> <

" On insert mode, tab has conditional behaviour
function! SmartTab()
  let current_position = col(".")
  let l:line = strpart( getline('.'), 0, col('.')-1)
  let l:lastchar = matchstr(getline('.'), '.\%' . col('.') . 'c')

  " if popup menu is visible, go to next in the list
  if pumvisible()
    return "\<C-n>"
  " if there is a snippet to expand, do it
  elseif coc#expandableOrJumpable()
    return "\<C-r>=coc#rpc#request('doKeymap', ['snippets-expand-jump',''])\<CR>"
  "if the line consists only of spaces, then insert TAB character"
  elseif l:line =~ '^\s*$'
    return "\<Tab>"
  "if there is a space behind the cursor, theres nothing to do. insert TAB char
  elseif <SID>check_back_space()
    return "\<TAB>"
  " if the last character is a slash, call file-completion
  elseif l:lastchar =~ "/"
    return "\<C-x>\<C-f>"
  " call omni completion if has omnifunc
  elseif len(&omnifunc) > 0
    return "\<C-x>\<C-o>"
  " call word completion otherwise
  else
    return "\<C-n>"
  endif
endfunction

function! SmartBackTab()
    let l:line = strpart( getline('.'), 0, col('.')-1)
    let l:lastchar = matchstr(getline('.'), '.\%' . col('.') . 'c')
    " if popup menu is visible, go to next in the list
    if pumvisible()
        return "\<C-p>"
    else
        return "\<S-Tab>"
    endif
endfunction

" <C-r>=, or Ctrl+R= is used to insert the result of an expression at the cursor.
inoremap <silent> <Tab> <C-r>=SmartTab()<CR>
inoremap <silent> <S-Tab> <C-r>=SmartBackTab()<CR>

let g:coc_snippet_next = '<tab>'

function! s:check_back_space() abort
	let col = col('.') - 1
	return !col || getline('.')[col - 1]  =~# '\s'
endfunction

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
nnoremap cnco :CocConfig<CR>

" Comment line with C-/ like in VSCode
" for some reason vim ses C-/ as C-_
nmap <C-_> gcc
" gv restores the last visual selection made
vmap <C-_>gcgv

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

" set paste               " Paste from a windows or from vim
set go+=a               " Visual selection automatically copied to the clipboard

" On Mac OS X and Windows, the * and + registers both point to the system clipboard so unnamed and unnamedplus have the same effect: the unnamed register is synchronized with the system clipboard.
" On Linux, you have essentially two clipboards: one is pretty much the same as in the other OSes (CtrlC and CtrlV in other programs, mapped to register + in Vim), the other is the "selection" clipboard (mapped to register * in Vim).

" Using only unnamedplus on Linux, Windows and Mac OS X allows you to:
" * CtrlC in other programs and put in Vim with p on all three platforms,
" * yank in Vim with y and CtrlV in other programs on all three platforms.
" * If you also want to use Linux's "selection" clipboard, you will also need unnamed.

" Here is a cross-platform value:
" set clipboard^=unnamed,unnamedplus

if has('unnamedplus')
  set clipboard=unnamed,unnamedplus
endif

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
nnoremap <M-j> :m .+1<CR>==
nnoremap <M-k> :m .-2<CR>==
inoremap <M-j> <Esc>:m .+1<CR>==gi
inoremap <M-k> <Esc>:m .-2<CR>==gi
vnoremap <M-j> :m '>+1<CR>gv=gv
vnoremap <M-k> :m '<-2<CR>gv=gv
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


"*****************************************************************************
"" Auto install dependencies
"*****************************************************************************




set wildoptions+=pum " use popupmenu for wildmode completion
set wildoptions+=tagfile

set inccommand=split " show live results for some commands (like :s) in a split window
set display+=msgsep "  minimize scrolling when showing messages


" Enables pseudo-transparency for the |popup-menu|
highlight PmenuSel blend=0
" FORCE the current selected element to be fully opaque:
set pumblend=0



""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" Vim Airline
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" vim-airline
let g:airline_theme='codedark'
" let g:airline_theme='base16-spacemacs'

let g:airline_skip_empty_sections = 1


" extensions
let g:airline#extensions#tabline#enabled = 1       " enable tabline upport
let g:airline#extensions#tabline#formatter = 'unique_tail'
let g:airline#extensions#tabline#left_sep = '▶'
let g:airline#extensions#tabline#left_alt_sep = '|'
let g:airline#extensions#tabline#tab_nr_type = 0   " show tab numbers
let g:airline#extensions#tabline#show_buffers = 0  " hide buffers
let g:airline#extensions#tabline#tab_min_count = 1 " show tabline even with only 1 tab

" tagbar
let g:airline#extensions#tagbar#enabled = 1

" git
let g:airline#extensions#branch#enabled = 1

" ale status
let g:airline#extensions#ale#enabled = 1

" enable/disable coc integration >
let g:airline#extensions#coc#enabled = 1
let airline#extensions#coc#error_symbol = 'E:'
let airline#extensions#coc#warning_symbol = 'W:'
let airline#extensions#coc#stl_format_err = '%E{[%e(#%fe)]}'
let airline#extensions#coc#stl_format_warn = '%W{[%w(#%fw)]}'

" symbols
if !exists('g:airline_symbols')
	let g:airline_symbols = {}
endif


" POWERLINE

" automatically populate the g:airline_symbols dictionary with the powerline symbols.
let g:airline_powerline_fonts = 1                     " needed by devicons

if !exists('g:airline_powerline_fonts')
	let g:airline#extensions#tabline#left_sep = ' '
	let g:airline#extensions#tabline#left_alt_sep = '|'
	let g:airline_left_sep          = '▶'
	let g:airline_left_alt_sep      = '»'
	let g:airline_right_sep         = '◀'
	let g:airline_right_alt_sep     = '«'
	let g:airline#extensions#branch#prefix     = '⤴' "➔, ➥, ⎇
	let g:airline#extensions#readonly#symbol   = '⊘'
	let g:airline#extensions#linecolumn#prefix = '¶'
	let g:airline#extensions#paste#symbol      = 'ρ'
	let g:airline_symbols.linenr    = '␊'
	let g:airline_symbols.branch    = '⎇'
	let g:airline_symbols.paste     = 'ρ'
	let g:airline_symbols.paste     = 'Þ'
	let g:airline_symbols.paste     = '∥'
	let g:airline_symbols.whitespace = 'Ξ'
else
	let g:airline#extensions#tabline#left_sep = ''
	let g:airline#extensions#tabline#left_alt_sep = ''

	" powerline symbols
	let g:airline_left_sep = ''
	let g:airline_left_alt_sep = ''
	let g:airline_right_sep = ''
	let g:airline_right_alt_sep = ''
	let g:airline_symbols.branch = ''
	let g:airline_symbols.readonly = ''
	let g:airline_symbols.linenr = '☰'
	let g:airline_symbols.maxlinenr = ''
endif


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" NERDTREE SETTINGS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Toogle NERDTree
nnoremap <s-f8>  :NERDTreeFind<CR>
nnoremap <leader>fe :NERDTreeToggle<CR>
map <f8> :NERDTreeToggle<CR>


" Open NerdTree on Vim startup and move cursor to edit buffer instead of leaving it on nerdtree window w
autocmd VimEnter * NERDTree | wincmd p
autocmd VimEnter * if argc() == 1 | NERDTree | wincmd p | else | NERDTree | endif


let NERDTreeShowBookmarks=1 " Show bookmarks table on Nerdtree


" Open NerdTree directly on the file currently being edited

" Sets PWD and the tree root on the currently selected directory
let g:NERDTreeChDirMode = 2

" Make NERDTree prettier
let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 1

" Automatically close a tab if the only remaining window is NerdTree:
autocmd BufEnter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

" Automatically delete the buffer of the file you just deleted with NerdTree:
let NERDTreeAutoDeleteBuffer = 1

" close nerd tree on opening file
let NERDTreeQuitOnOpen = 0

" Automatically delete the buffer of the file you just deleted with NerdTree:
let NERDTreeAutoDeleteBuffer = 1

"" NERDTree configuration
let g:NERDTreeChDirMode=2
let g:NERDTreeIgnore=['\.rbc$', '\~$', '\.pyc$', '\.db$', '\.sqlite$', '__pycache__']
let g:NERDTreeSortOrder=['^__\.py$', '\/$', '*', '\.swp$', '\.bak$', '\~$']
let g:NERDTreeShowBookmarks=1
let g:nerdtree_tabs_focus_on_files=1
let g:NERDTreeMapOpenInTabSilent = '<RightMouse>'
let g:NERDTreeWinSize = 35



" NERDTrees File highlighting
" Source https://github.com/ryanoasis/vim-devicons/wiki/FAQ-&-Troubleshooting#colors
function! NERDTreeHighlightFile(extension, fg, bg, guifg, guibg)
	exec 'autocmd FileType nerdtree highlight ' . a:extension .' ctermbg='. a:bg .' ctermfg='. a:fg .' guibg='. a:guibg .' guifg='. a:guifg
	exec 'autocmd FileType nerdtree syn match ' . a:extension .' #^\s\+.*'. a:extension .'$#'
endfunction

call NERDTreeHighlightFile('jade', 'green', 'none', 'green', '#151515')
call NERDTreeHighlightFile('ini', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('md', 'blue', 'none', '#3366FF', '#151515')
call NERDTreeHighlightFile('yml', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('config', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('conf', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('json', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('html', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('styl', 'cyan', 'none', 'cyan', '#151515')
call NERDTreeHighlightFile('css', 'cyan', 'none', 'cyan', '#151515')
call NERDTreeHighlightFile('coffee', 'Red', 'none', 'red', '#151515')
call NERDTreeHighlightFile('js', 'Red', 'none', '#ffa500', '#151515')
call NERDTreeHighlightFile('php', 'Magenta', 'none', '#ff00ff', '#151515')
call NERDTreeHighlightFile('ds_store', 'Gray', 'none', '#686868', '#151515')
call NERDTreeHighlightFile('gitconfig', 'Gray', 'none', '#686868', '#151515')
call NERDTreeHighlightFile('gitignore', 'Gray', 'none', '#686868', '#151515')
call NERDTreeHighlightFile('bashrc', 'Gray', 'none', '#686868', '#151515')
call NERDTreeHighlightFile('bashprofile', 'Gray', 'none', '#686868', '#151515')


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" fzf.vim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Install fzf and rg
if !empty(glob("~/.fzf/bin/fzf"))
	if empty(glob("~/.fzf/bin/rg"))
		silent !curl -fLo /tmp/rg.tar.gz
					\ https://github.com/BurntSushi/ripgrep/releases/download/11.0.2/ripgrep-11.0.2-x86_64-unknown-linux-musl.tar.gz
		silent !tar xzvf /tmp/rg.tar.gz --directory /tmp
		silent !cp /tmp/ripgrep-0.10.0-x86_64-unknown-linux-musl/rg ~/.fzf/bin/rg
	endif
endif

" On the resulsts window:
" * ENTER - open in current window
" * CTRL-T -> open in a new tab
" * CTRL-X -> open in horizontal split
" * CTRL-V -> open in vertical splits

" fzf on a floating window
let g:fzf_layout = { 'window': { 'width': 0.6, 'height': 0.6 } }

" Hide statusline on fzf terminal buffers for a cleaner look
if has('nvim') && !exists('g:fzf_layout')
	autocmd! FileType fzf
	autocmd  FileType fzf set laststatus=0 noshowmode noruler
				\| autocmd BufLeave <buffer> set laststatus=2 showmode ruler
endif



" use ripgrep if available, else try ag
if executable('rg')
	let $FZF_DEFAULT_COMMAND = 'rg --files --hidden --follow --glob "!.git/*"'
	set grepprg=rg\ --vimgrep
	command! -bang -nargs=* Find call fzf#vim#grep('rg --column --line-number --no-heading --fixed-strings --ignore-case --hidden --follow --glob "!.git/*" --color "always" '.shellescape(<q-args>).'| tr -d "\017"', 1, <bang>0)
elseif executable('ag')
	let $FZF_DEFAULT_COMMAND = 'ag --hidden --ignore .git -g ""'
	set grepprg=ag\ --nogroup\ --nocolor
else
endif

cnoremap <C-P> <C-R>=expand("%:p:h") . "/" <CR>


" Customize fzf colors to match your color scheme
" - fzf#wrap translates this to a set of `--color` options
let g:fzf_colors =
			\ { 'fg':      ['fg', 'Normal'],
			\ 'bg':      ['bg', 'Normal'],
			\ 'hl':      ['fg', 'Comment'],
			\ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
			\ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
			\ 'hl+':     ['fg', 'Statement'],
			\ 'info':    ['fg', 'PreProc'],
			\ 'border':  ['fg', 'Ignore'],
			\ 'prompt':  ['fg', 'Conditional'],
			\ 'pointer': ['fg', 'Exception'],
			\ 'marker':  ['fg', 'Keyword'],
			\ 'spinner': ['fg', 'Label'],
			\ 'header':  ['fg', 'Comment'] }


"----------------------------
" FZF CUSTOM COMMANDS

" Find word in project

" Grep for word under cursor in project but excludes some files and folders, like node_modules
let g:rg_command = '
			\ rg --column --line-number --no-heading --fixed-strings --ignore-case --no-ignore --hidden --follow --color "always"
			\ -g "*.{js,json,php,md,styl,jade,html,config,py,cpp,c,go,hs,rb,conf}"
			\ -g "!{.git,node_modules,vendor}/*" '

command! -bang -nargs=* RipgrepFind call fzf#vim#grep(g:rg_command .shellescape(<q-args>), 1, <bang>0)
command! -bang -nargs=* FU call fzf#vim#grep(g:rg_command . '-m1 ' . shellescape(<q-args>), 1, <bang>0)

" Grep for word under cursor in project
command! -bang -nargs=* ProjectRg call fzf#vim#grep("rg --column --line-number --no-heading --color=always --smart-case ".shellescape(<q-args>), 1, {'dir': system('git rev-parse --show-toplevel 2> /dev/null')[:-2]}, <bang>0)

"----------------------------
" FZF CUSTOM MAPPINGS

" Find file in the dir of current buffer
nnoremap <Leader>ff :<C-U>execute 'FZF' expand('%:p:h')<CR>

" Find files in home directory
map <leader>hff :Files ~<cr>

" Find files in root of project (PWD)
map <leader>pf :Files<cr>
" PS: the :Files command seems to be equivalent to FZF -m
nnoremap <silent> <leader>pf :FZF -m<CR>

" List all of current mappings
nmap <leader><tab> <plug>(fzf-maps-n)
xmap <leader><tab> <plug>(fzf-maps-x)
omap <leader><tab> <plug>(fzf-maps-o)

" Insert mode completion
imap <c-x><c-k> <plug>(fzf-complete-word)
imap <c-x><c-f> <plug>(fzf-complete-path)
imap <c-x><c-j> <plug>(fzf-complete-file-ag)
imap <c-x><c-l> <plug>(fzf-complete-line)

" Advanced customization using Vim function
inoremap <expr> <c-x><c-k> fzf#vim#complete#word({'left': '15%'})
" or replace the default dictionary completion with fzf-based fuzzy completion
" inoremap <expr> <c-x><c-k> fzf#vim#complete('cat /usr/share/dict/words')

"Recovery commands from history through FZF
nmap <leader>fh :History:<CR>


nnoremap <leader>pgf :RipgrepFind <C-r><C-w><Cr>

" seems to search files in current PWD
" Search for current word under cursor in project
nnoremap <leader>pgw :ProjectRg <C-r><C-w><Cr>

" Find file in git project
nnoremap <leader>gf :GitFiles<Cr>


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" COC settings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
" delays and poor user experience.
set updatetime=300

" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
set signcolumn=yes

" to get the correct comment highlighting for jsonc
autocmd FileType json syntax match Comment +\/\/.\+$+

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()


" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current
" position. Coc only does snippet and additional edit on confirm.
if has('patch8.1.1068')
  " Use `complete_info` if your (Neo)Vim version supports it.
  inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
else
  imap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
endif



" Remap keys for gotos
nmap <silent> lgd <Plug>(coc-definition)
nmap <silent> lgy <Plug>(coc-type-definition)
nmap <silent> lgi <Plug>(coc-implementation)
nmap <silent> lgr <Plug>(coc-references)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
	if (index(['vim','help'], &filetype) >= 0)
		execute 'h '.expand('<cword>')
	else
		call CocAction('doHover')
	endif
endfunction


" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming.
nmap <leader>lrn <Plug>(coc-rename)
" Coc rename
nnoremap <leader>prn :call CocActionAsync('rename')<cr>


" Formatting selected code.
xmap <leader>lf  <Plug>(coc-format-selected)
nmap <leader>lf  <Plug>(coc-format-selected)

augroup mygroup
	autocmd!
	" Setup formatexpr specified filetype(s).
	autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
	" Update signature help on jump placeholder.
	autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end


" Applying codeAction to the selected region.
" Example: `<leader>aap` for current paragraph
xmap <leader>la  <Plug>(coc-codeaction-selected)
nmap <leader>la  <Plug>(coc-codeaction-selected)

" Remap keys for applying codeAction to the current line.
nmap <leader>lfl  <Plug>(coc-codeaction)
" Apply AutoFix to problem on the current line.
nmap <leader>lql  <Plug>(coc-fix-current)


" Introduce function text object
" NOTE: Requires 'textDocument.documentSymbol' support from the language server.
" xmap if <Plug>(coc-funcobj-i)
" xmap af <Plug>(coc-funcobj-a)
" omap if <Plug>(coc-funcobj-i)
" omap af <Plug>(coc-funcobj-a)


" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')

" Add `:Fold` command to fold current buffer.
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')


" Mappings using CoCList:
" Show all diagnostics.
nnoremap <silent> <space>ld  :<C-u>CocList diagnostics<cr>
" Manage extensions.
nnoremap <silent> <space>le  :<C-u>CocList extensions<cr>
" Show commands.
nnoremap <silent> <space>lc  :<C-u>CocList commands<cr>
" Find symbol of current document.
nnoremap <silent> <space>lo  :<C-u>CocList outline<cr>
" Search workspace symbols.
nnoremap <silent> <space>ls  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent> <space>lj  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent> <space>lk  :<C-u>CocPrev<CR>
" Resume latest coc list.
nnoremap <silent> <space>lp  :<C-u>CocListResume<CR>


" Extensions
let g:coc_global_extensions = [
			\ 'coc-git',
			\ 'coc-snippets',
			\ 'coc-highlight',
			\ 'coc-marketplace',
			\ 'coc-pairs',
			\ 'coc-html',
			\ 'coc-css',
			\ 'coc-json',
			\ 'coc-java',
			\ 'coc-rls',
			\ 'coc-angular',
			\ 'coc-tsserver',
			\ 'coc-yaml',
			\ 'coc-vimlsp',
			\ 'coc-prettier',
			\ 'coc-project',
			\ 'coc-actions',
			\ 'coc-tsserver',
			\ 'coc-tslint-plugin',
			\ 'coc-eslint',
			\ 'coc-json',
			\ 'coc-go',
			\ ]


" Multiple Cursors
" nmap <silent> <C-d> <Plug>(coc-cursors-word)*
" xmap <silent> <C-d> y/\V<C-r>=escape(@",'/\')<CR><CR>gN<Plug>(coc-cursors-range)gn

" Or more vscode like behavior:
nmap <expr> <silent> <C-d> <SID>select_current_word()
function! s:select_current_word()
	if !get(g:, 'coc_cursors_activated', 0)
		return "\<Plug>(coc-cursors-word)"
	endif
	return "*\<Plug>(coc-cursors-word):nohlsearch\<CR>"
endfunc

" Overide the default multiple cursors range highlight, which is basicly transparent and difficult to see
hi CocCursorRange guibg=#b16286 guifg=#ebdbb2


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" coc prettier
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

command! -nargs=0 Prettier :call CocAction('runCommand', 'prettier.formatFile')

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" tpope's Endwise
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" disable mapping to not break <CR> confirm selection in coc.nvim (I don't even use them anyways)
let g:endwise_no_mappings = 1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" any jump
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Disabling default any-jump keybindings:
let g:any_jump_disable_default_keybindings = 1

" Jump to definition under cursore
nnoremap <leader>j :AnyJump<CR>
nnoremap <M-.> :AnyJump<CR>

" open previous opened file (after jump)
nnoremap <leader>jb :AnyJumpBack<CR>
nnoremap <M-,> :AnyJumpBack<CR>

" open last closed search window again
nnoremap <leader>jl :AnyJumpLastResults<CR>

" Mappings for popup search window
" o/ open link p/ preview q/x exit r references b back to first result T group by file a load next N results A load all results L toggle results lists ui style

" Show line numbers in search rusults
let g:any_jump_list_numbers = 0

" Auto search references
let g:any_jump_references_enabled = 1

" Auto group results by filename
let g:any_jump_grouping_enabled = 0

" Amount of preview lines for each search result
let g:any_jump_preview_lines_count = 5

" Max search results, other results can be opened via [a]
let g:any_jump_max_search_results = 7

" Prefered search engine: rg or ag
let g:any_jump_search_prefered_engine = 'rg'

" Search results list styles:
" - 'filename_first'
" - 'filename_last'
let g:any_jump_results_ui_style = 'filename_first'

" Any-jump window size & position options
let g:any_jump_window_width_ratio  = 0.6
let g:any_jump_window_height_ratio = 0.6
let g:any_jump_window_top_offset   = 4

" Disable default any-jump keybindings (default: 0)
let g:any_jump_disable_default_keybindings = 1

" Remove comments line from search results (default: 1)
let g:any_jump_remove_comments_from_results = 1

" Cursor keyword selection mode
"
" on line:
" "MyNamespace::MyClass"
" then cursor is on MyClass word
"
" 'word' - will match 'MyClass'
" 'full' - will match 'MyNamespace::MyClass'
let g:any_jump_keyword_match_cursor_mode= 'word'


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Ale
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" show linting errors on a separate window
" let g:ale_open_list = 1
" let g:ale_list_window_size = 5
"
let g:ale_sign_error = '>>'
" let g:ale_sign_warning = '--'
let g:ale_sign_warning = '⚠'

" Show X lines of errors (default: 10)
let g:ale_list_window_size = 10

" ale
let g:ale_linters = {}




""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Expand Region
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Press + to expand the visual selection and _ to shrink it.

" Customize the key mapping if you don't like the default.
vmap K <Plug>(expand_region_expand)
vmap J <Plug>(expand_region_shrink)

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" TsLint
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" auto lint when saving typescript files
" autocmd BufWritePost *.ts,*.tsx call tslint#run('a', win_getid())


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" blamer - gitlens implementation for vim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let g:blamer_enabled = 0

let g:blamer_delay = 500
let g:blamer_show_in_visual_modes = 0
let g:blamer_prefix = ' > '

" Template for the blame message
" Available options: <author>, <author-mail>, <author-time>, <committer>, <committer-mail>, <committer-time>, <summary>, <commit-short>, <commit-long>.
let g:blamer_template = '<committer>, <committer-time> • <summary>'

" The color of the blame message.
highlight Blamer guifg=lightgrey



""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" nvim blame line - another gitlens like implementation
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

nnoremap <silent> <leader>gb :ToggleBlameLine<CR>

" Use autocmd to enable on startup:
" autocmd BufEnter * EnableBlameLine

" Options
" Show blame info below the statusline instead of using virtual text
let g:blameLineUseVirtualText = 0

" Specify the highlight group used for the virtual text ('Comment' by default)
let g:blameLineVirtualTextHighlight = 'Question'

" Add a prefix to the virtual text (empty by default)
let g:blameLineVirtualTextPrefix = '// '

" Customize format for git blame (Default format: '%an | %ar | %s')
let g:blameLineGitFormat = '%an - %s'
" Refer to 'git-show --format=' man pages for format options)


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" EMMET SETTINGS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Use TAB to expand emmet snippets (equivalent to <C-y>,
" let g:user_emmet_expandabbr_key = '<S-tab>'

" Create another binding for emmet, because the default is not great
map <leader>ee <c-y>,
map <C-y>y <c-y>,

" navigate to edit points
inoremap <C->> <c-y>n<CR>

" remap native keybinding <C-y>
" let g:user_emmet_leader_key='<tab>'


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vim prettier
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" PS: using coc-prettier for now because of annoying vim-prettier bug of jumping the cursor to the top of the
" file when formatting


" Enable auto formatting of files that have "@format" or "@prettier" tag
" let g:prettier#autoformat = 1
"
" Allow auto formatting for files without "@format" or "@prettier" tag
" let g:prettier#autoformat_require_pragma = 0

" Force the prettier command to be async (requires neovim or vim8)
" let g:prettier#exec_cmd_async = 1

" Format supported files on save
" autocmd BufWritePre *.js,*.jsx,*.mjs,*.ts,*.tsx,*.css,*.less,*.scss,*.json,*.graphql,*.md,*.vue,*.yaml,*.html PrettierAsync

" dont open the quickfix window when there are errors
" let g:prettier#quickfix_enabled=0

" dont focus on the quickfix window when there are errors
" let g:prettier#quickfix_auto_focus = 0


" Prettier parameters
" let g:prettier#config#print_width = 100
" let g:prettier#config#tab_width = 4
" let g:prettier#config#print_width = 'auto'
" let g:prettier#config#tab_width = 'auto'
" let g:prettier#config#use_tabs = 'auto'
" let g:prettier#config#parser = ''
" let g:prettier#config#config_precedence = 'file-override'
" let g:prettier#config#prose_wrap = 'preserve'
" let g:prettier#config#html_whitespace_sensitivity = 'css'

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Angular
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" If in a TS file
" or add the bindings in after/ftplugin/typescript.vim without the autocmd rules
autocmd FileType javascript,typescript nnoremap <leader>och :edit %<.html<CR>
autocmd FileType javascript,typescript nnoremap <leader>ocsc :edit %<.css<CR>
autocmd FileType javascript,typescript nnoremap <leader>ocss :edit %<.scss<CR>
autocmd FileType javascript,typescript nnoremap <leader>oct :edit %<.test.ts<CR>


autocmd FileType html nnoremap <leader>occ :edit %<.ts<CR>
autocmd FileType html nnoremap <leader>ocsc :edit %<.css<CR>
autocmd FileType html nnoremap <leader>ocss :edit %<.scss<CR>

autocmd FileType css,scss nnoremap <leader>occ :edit %<.ts<CR>
autocmd FileType css,scss nnoremap <leader>och :edit %<.html<CR>

" With a single mapping, use :help wildmenu to choose which of the counterparts you want to switch to:
" in $MYVIMRC
set wildmenu
set wildcharm=<C-z>
nnoremap <key> :edit %<.<C-z>

nnoremap <buffer> <C-]> :TernDef<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Matchup
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Customizing the highlighting colors
" highlight MatchParen ctermbg=blue guibg=white cterm=italic gui=italic

" You may want to put this inside a ColorScheme autocmd so it is preserved after colorscheme changes:
augroup matchup_matchparen_highlight
	autocmd!
	autocmd ColorScheme * highlight MatchParen guifg=red
augroup END

" You can also highlight words differently than parentheses using the MatchWord highlighting group. You might do this if you find the MatchParen style distracting for large blocks.
highlight MatchWord ctermfg=red guifg=blue cterm=underline gui=underline

" There are also MatchParenCur and MatchWordCur which allow you to configure the highlight separately for the match under the cursor.
highlight MatchParenCur cterm=underline gui=underline
highlight MatchWordCur cterm=underline gui=underline

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Rainbow Parentheses Improved
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let g:rainbow_active = 1
" let g:rainbow_conf = {'guifgs': ['#FFD700','#C466C0','#7AB9E0']}
let g:rainbow_conf = {'guifgs': ['royalblue3', 'darkorange3', 'seagreen3', 'firebrick']}


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" VIM COLORIZER SETTINGS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" NEVER INSTALL THIS PLUGIN AGAIN, TOOK ME HOURS TO FIGURE IT WAS THIS PLUGIN CAUSING UNBEARABLE LAG
" APPEARANTLY THIS FUCKER WAS SLOWING THINGS DOWN

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" VIM ILLUMINATE SETTINGS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" disable illuminate words on NerdTREE
let g:Illuminate_ftblacklist = ['nerdtree']

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" VIM DEVICONS SETTINGS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let g:webdevicons_enable = 1
let g:webdevicons_enable_nerdtree = 1 " adding the flags to NERDTree
let g:webdevicons_enable_airline_tabline = 1 " adding to vim-airline's tabline
let g:webdevicons_enable_airline_statusline = 1 " adding to vim-airline's statusline
let g:webdevicons_conceal_nerdtree_brackets = 1 " whether or not to show the nerdtree brackets around flags
let g:WebDevIconsNerdTreeGitPluginForceVAlign = 1 " Force extra padding in NERDTree so that the filetype icons line up vertically
let g:webdevicons_enable_denite = 1 " Adding the custom source to denite

" solve the issue with NERDTree devicons bugging after sourcing vimrc
if exists("g:loaded_webdevicons")
	call webdevicons#refresh()
endif

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" grep
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

nnoremap <silent> <leader>fg :Rgrep<CR>
let Grep_Default_Options = '-IR'
let Grep_Skip_Files = '*.log *.db'
let Grep_Skip_Dirs = '.git node_modules'

autocmd VimEnter * call NERDTreeHighlightFile('jade', 'green', 'none', 'green', '#151515')

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Indent Lines
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let g:indentLine_char_list = ['|', '¦', '┆', '┊']

" Change indent char color
let g:indentLine_color_term = 237       " Vim (has to be terminal colors, from 0 to 256)
let g:indentLine_color_gui = '#A4E57E'  " gvim
let g:indentLine_color_tty_light = 7    " none X terminal (default: 4)
let g:indentLine_color_dark = 1         " none X terminal (default: 2)

" Background (Vim, GVim)
" let g:indentLine_bgcolor_term = 202
" let g:indentLine_bgcolor_gui = '#FF5F00'

"#########################################################################
" LANGUAGES SETTINGS


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" html
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" for html files, 2 spaces
autocmd Filetype html setlocal ts=2 sw=2 expandtab

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" javascript
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let g:javascript_enable_domhtmlcss = 1

" vim-javascript
augroup vimrc-javascript
	autocmd!
	autocmd FileType javascript setl tabstop=4|setl shiftwidth=4|setl expandtab softtabstop=4
augroup END

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" typescript
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" typescript
let g:yats_host_keyword = 1



""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" elixir
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" erlang
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let erlang_folding = 1
let erlang_show_errors = 1


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" go
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" organize imports on save
autocmd BufWritePre *.go :call CocAction('runCommand', 'editor.action.organizeImport')


" add go vet to ale linters
:call extend(g:ale_linters, {
			\"go": ['golint', 'go vet'], })

" vim-go
" run :GoBuild or :GoTestCompile based on the go file
function! s:build_go_files()
	let l:file = expand('%')
	if l:file =~# '^\f\+_test\.go$'
		call go#test#Test(0, 1)
	elseif l:file =~# '^\f\+\.go$'
		call go#cmd#Build(0)
	endif
endfunction

let g:go_list_type = "quickfix"
let g:go_fmt_command = "goimports"
let g:go_fmt_fail_silently = 1

let g:go_highlight_types = 1
let g:go_highlight_fields = 1
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1
let g:go_highlight_structs = 1
let g:go_highlight_generate_tags = 1
let g:go_highlight_space_tab_error = 0
let g:go_highlight_array_whitespace_error = 0
let g:go_highlight_trailing_whitespace_error = 0
let g:go_highlight_extra_types = 1

autocmd BufNewFile,BufRead *.go setlocal noexpandtab tabstop=4 shiftwidth=4 softtabstop=4

augroup completion_preview_close
	autocmd!
	if v:version > 703 || v:version == 703 && has('patch598')
		autocmd CompleteDone * if !&previewwindow && &completeopt =~ 'preview' | silent! pclose | endif
	endif
augroup END

augroup go

	au!
	au Filetype go command! -bang A call go#alternate#Switch(<bang>0, 'edit')
	au Filetype go command! -bang AV call go#alternate#Switch(<bang>0, 'vsplit')
	au Filetype go command! -bang AS call go#alternate#Switch(<bang>0, 'split')
	au Filetype go command! -bang AT call go#alternate#Switch(<bang>0, 'tabe')

	au FileType go nmap <Leader>dd <Plug>(go-def-vertical)
	au FileType go nmap <Leader>dv <Plug>(go-doc-vertical)
	au FileType go nmap <Leader>db <Plug>(go-doc-browser)

	au FileType go nmap <leader>r  <Plug>(go-run)
	au FileType go nmap <leader>t  <Plug>(go-test)
	au FileType go nmap <Leader>gt <Plug>(go-coverage-toggle)
	au FileType go nmap <Leader>i <Plug>(go-info)
	au FileType go nmap <silent> <Leader>l <Plug>(go-metalinter)
	au FileType go nmap <C-g> :GoDecls<cr>
	au FileType go nmap <leader>dr :GoDeclsDir<cr>
	au FileType go imap <C-g> <esc>:<C-u>GoDecls<cr>
	au FileType go imap <leader>dr <esc>:<C-u>GoDeclsDir<cr>
	au FileType go nmap <leader>rb :<C-u>call <SID>build_go_files()<CR>

augroup END


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" haskell
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let g:haskell_conceal_wide = 1
let g:haskell_multiline_strings = 1
let g:necoghc_enable_detailed_browse = 1
autocmd Filetype haskell setlocal omnifunc=necoghc#omnifunc


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ruby
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let g:rubycomplete_buffer_loading = 1
let g:rubycomplete_classes_in_global = 1
let g:rubycomplete_rails = 1

augroup vimrc-ruby
	autocmd!
	autocmd BufNewFile,BufRead *.rb,*.rbw,*.gemspec setlocal filetype=ruby
	autocmd FileType ruby set tabstop=2|set shiftwidth=2|set expandtab softtabstop=2
augroup END

let g:tagbar_type_ruby = {
			\ 'kinds' : [
			\ 'm:modules',
			\ 'c:classes',
			\ 'd:describes',
			\ 'C:contexts',
			\ 'f:methods',
			\ 'F:singleton methods'
			\ ]
			\ }

" RSpec.vim mappings
map <Leader>t :call RunCurrentSpecFile()<CR>
map <Leader>s :call RunNearestSpec()<CR>
map <Leader>l :call RunLastSpec()<CR>
map <Leader>a :call RunAllSpecs()<CR>

" For ruby refactory
if has('nvim')
	runtime! macros/matchit.vim
else
	packadd! matchit
endif

" Ruby refactory
nnoremap <leader>rap  :RAddParameter<cr>
nnoremap <leader>rcpc :RConvertPostConditional<cr>
nnoremap <leader>rel  :RExtractLet<cr>
vnoremap <leader>rec  :RExtractConstant<cr>
vnoremap <leader>relv :RExtractLocalVariable<cr>
nnoremap <leader>rit  :RInlineTemp<cr>
vnoremap <leader>rrlv :RRenameLocalVariable<cr>
vnoremap <leader>rriv :RRenameInstanceVariable<cr>
vnoremap <leader>rem  :RExtractMethod<cr>


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" rust
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Vim racer
au FileType rust nmap gd <Plug>(rust-def)
au FileType rust nmap gs <Plug>(rust-def-split)
au FileType rust nmap gx <Plug>(rust-def-vertical)
au FileType rust nmap <leader>gd <Plug>(rust-doc)

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" markdown
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Disable folding on Vim Markdown
let g:vim_markdown_folding_disabled = 1



