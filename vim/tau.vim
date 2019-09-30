""""""""""""""""""""""""""""""""""""""""""""""
" my vim setup
" heavily based on https://github.com/amix/vimrc/blob/master/vimrcs/basic.vim
"""""""""""""""""""""""""""""""""""""""""""""""


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => General

" Set utf8 as standard encoding and en_US as the standard language
set encoding=utf8

" Shows line numbering
set number

" Sets how many lines of history VIM has to remember
set history=500

" Set to auto read when a file is changed from the outside
set autoread

" Use Unix as the standard file type
set ffs=unix,dos,mac

" With a map leader it's possible to do extra key combinations
" like <leader>w saves the current file
let mapleader = ","

" Fast saving
nmap <leader>w :w!<cr>

" Fast quitting
nmap <leader>q :q!<cr>

" :W sudo saves the file
" (useful for handling the permission-denied error)
command! W w !sudo tee % > /dev/null

" auto reload (source) vimrc if changed
" this makes unnecessary to restart vim after adding plugins or changing something in the config
if has ('autocmd') " Remain compatible with earlier versions
    augroup vimrc     " Source vim configuration upon save
        autocmd! BufWritePost $MYVIMRC source % | echom "Reloaded " . $MYVIMRC 
        autocmd! BufWritePost $MYGVIMRC if has('gui_running') | so % | echom "Reloaded " . $MYGVIMRC | endif | redraw
    augroup END
endif " has autocmd


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => VIM USER INTERFACE

" Set 7 lines to the cursor - when moving vertically using j/k
set so=7

" Avoid garbled characters in Chinese language windows OS
let $LANG='en'
set langmenu=en
source $VIMRUNTIME/delmenu.vim
source $VIMRUNTIME/menu.vim

" Turn on the Wild menu
set wildmenu

" Ignore compiled files
set wildignore=*.o,*~,*.pyc
if has("win16") || has("win32")
    set wildignore+=.git\*,.hg\*,.svn\*
else
    set wildignore+=*/.git/*,*/.hg/*,*/.svn/*,*/.DS_Store
endif

"Always show current position
set ruler

" Height of the command bar
set cmdheight=2

" A buffer becomes hidden when it is abandoned
set hid

" Configure backspace so it acts as it should act
set backspace=eol,start,indent
set whichwrap+=<,>,h,l


" don't duplicate an existing open buffer
set switchbuf=useopen

" show auto complete menus.
set wildmenu
" Make wildmenu behave like bash completion. Finding commands are so easy now.
set wildmode=list:longest

" Don't redraw while executing macros (good performance config)
set lazyredraw

" For regular expressions turn magic on
set magic

" Show matching brackets when text indicator is over them
set showmatch
" How many tenths of a second to blink when matching brackets
set mat=2

" No annoying sound on errors
set noerrorbells
set novisualbell
set t_vb=
set tm=500

" Add a bit extra margin to the left
set foldcolumn=1


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => TEXT EDITING SETTINGS

" replace all instances of currently selected word for new input (leader + r)
map <leader>r :%s///g<left><left>

" Linebreak on 500 characters
set lbr
set tw=500

" Disable Vim native text folding
set nofoldenable

" Move a line of text using ALT+[jk] or Command+[jk] on mac
nmap <M-j> mz:m+<cr>`z
nmap <M-k> mz:m-2<cr>`z
vmap <M-j> :m'>+<cr>`<my`>mzgv`yo`z
vmap <M-k> :m'<-2<cr>`>my`<mzgv`yo`z

if has("mac") || has("macunix")
    nmap <D-j> <M-j>
    nmap <D-k> <M-k>
    vmap <D-j> <M-j>
    vmap <D-k> <M-k>
endif

" Remap VIM 0 to first non-blank character
" not sure what this does, i copied from somewhere
map 0 ^

" Return to last edit position when opening files (You want this!)
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => SEARCH SETTINGS

" Ignore case when searching
set ignorecase

" case insensitive search if all lowercase
set ignorecase smartcase

" Highlight search results
set hlsearch

" Makes Esc clear last search highlights after searching
nnoremap <esc> :noh<return><esc>

" Makes search act like search in modern browsers
set incsearch

" Visual mode pressing * or # searches for the current selection
" Super useful! From an idea by Michael Naumann
" vnoremap <silent> * :<C-u>call VisualSelection('', '')<CR>/<C-R>=@/<CR><CR>
" vnoremap <silent> # :<C-u>call VisualSelection('', '')<CR>?<C-R>=@/<CR><CR>


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => SPELL CHECKING

" Pressing ,ss will toggle and untoggle spell checking
map <leader>ss :setlocal spell!<cr>

" Spellchecking shortcuts 
map <leader>sn ]s
map <leader>sp [s
map <leader>sa zg
map <leader>s? z=


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => CODE EDITING GENERAL SETTINGS

" Delete trailing white space on save, useful for some filetypes ;)
fun! CleanExtraSpaces()
    let save_cursor = getpos(".")
    let old_query = getreg('/')
    silent! %s/\s\+$//e
    call setpos('.', save_cursor)
    call setreg('/', old_query)
endfun

if has("autocmd")
    autocmd BufWritePre *.txt,*.js,*.py,*.wiki,*.sh,*.coffee :call CleanExtraSpaces()
endif


" RunFile() Function (copied from http://urgetopunt.com/2014/02/19/run-scripts-from-vim.html)
" added Shell Scripts and PHP support
function! RunFile()
    if match(@%, '.rb$') != -1
        let argv = input('!ruby % ')
        exec '!ruby % ' . argv
    elseif match(@%, '.py$') != -1
        let argv = input('!python % ')
        exec '!python % ' . argv
    elseif match(@%, '.sh$') != -1
        let argv = input('!sh % ')
        exec '!sh % ' . argv
    elseif match(@%, '.php$') != -1
        let argv = input('!php % ')
        exec '!php % ' . argv
    else
        echo '<< ERROR >> RunFile() only supports ruby and python'
    endif
endfunction
" map the RunFile() function
noremap <Leader>rx :call RunFile()<CR>


"""""""""""""""""""""""""""""""""""""""
" HTML 

" auto indent HTML files on save and in open
" autocmd BufWritePre,BufRead *.html :normal gg=G


"""""""""""""""""""""""""""""""""""""""
" RUBY

" run rake
noremap <Leader>rr :!rake<CR>


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => SYNTAX HIGHLIGHTING

" enable syntax highlighting
syntax enable

" Native vim filetype plugin enables syntax highlighting base on file type
filetype on
filetype plugin on


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => INDENTATION

filetype indent on
set ai "Auto indent
set si "Smart indent
set wrap "Wrap lines

" Use spaces instead of tabs
set expandtab

" Be smart when using tabs
set smarttab

" 1 tab == 4 spaces
set shiftwidth=4
set tabstop=4


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => CODE COMPLETION

" Activate Vim native completion engine (Omnicomplete)
set omnifunc=syntaxcomplete#Complete


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" TMUX & VIM STUFF

" set title of tmux window to currently open file in vim
autocmd BufEnter * call system("tmux rename-window " . expand("%:t"))
autocmd VimLeave * call system("tmux setw automatic-rename")
autocmd BufEnter * let &titlestring = ' ' . expand("%:t")                                                                 
set title


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => COLORS AND FONTs

set t_Co=256

" Set extra options when running in GUI mode
if has("gui_running")
    set guioptions-=T
    set guioptions-=e
    set t_Co=256
    set guitablabel=%M\ %t
endif

" Set vim font (using NerdFont pached Hack for devicons plugin)
set guifont=Hack\ Nerd\ Font\ 15


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => FILES, BACKUPS AND UNDO

" Turn backup off, since most stuff is in SVN, git et.c anyway...
set nobackup
set nowb
set noswapfile


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => NAVIGATION AROUND, TABS, WINDOWS AND BUFFERS

" use <leader>s for horizontal split, <leader>v for vertical split
" <leader>q closes pane
nnoremap <silent> <leader>s :split<CR>
nnoremap <silent> <leader>v :vsplit<CR>
nnoremap <silent> <leader>q :close<CR>

" easier 'vim style' window navigation
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>lR

" easier 'vim style' tab navigation
nnoremap <M-J> <C-W><C-J>
nnoremap <M-K> <C-W><C-K>
nnoremap <M-L> <C-W><C-L>
nnoremap <M-H> <C-W><C-H>

" navigate to tabs using <leader><number>, similar to i3 workspaces switch
nnoremap <leader>1 1gt
nnoremap <leader>2 2gt
nnoremap <leader>3 3gt
nnoremap <leader>4 4gt
nnoremap <leader>5 5gt


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" TABS AND WINDOW SPLTTING

" Open new split panes to right and bottom, which feels more natural than Vim’s default:
set splitbelow
set splitright

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


" Opens a new tab with the current buffer's path
" Super useful when editing files in the same directory
map <leader>te :tabedit <c-r>=expand("%:p:h")<cr>/
" Map <Space> to / (search) and Ctrl-<Space> to ? (backwards search)
map <space> /
map <c-space> ?

" Disable highlight when <leader><cr> is pressed
map <silent> <leader><cr> :noh<cr>

" Close the current buffer
map <leader>bd :Bclose<cr>:tabclose<cr>gT

" Close all the buffers
map <leader>ba :bufdo bd<cr>

map <leader>l :bnext<cr>
map <leader>h :bprevious<cr>


" Switch CWD to the directory of the open buffer
map <leader>cd :cd %:p:h<cr>:pwd<cr>

" Specify the behavior when switching between buffers
try
    set switchbuf=useopen,usetab,newtab
    set stal=2
catch
endtry



"""""""""""""""""""""""""""""
" => Status line
""""""""""""""""""""""""""""""

" Always show the status line
set laststatus=2

" Format the status line
" set statusline=\ %{HasPaste()}%F%m%r%h\ %w\ \ CWD:\ %r%{getcwd()}%h\ \ \ Line:\ %l\ \ Column:\ %c

" set statusline=   " clear the statusline for when vimrc is reloaded
" set statusline+=%-3.3n\                      " buffer number
" set statusline+=%f\                          " file name
" set statusline+=%h%m%r%w                     " flags
" set statusline+=[%{strlen(&ft)?&ft:'none'},  " filetype
" set statusline+=%{strlen(&fenc)?&fenc:&enc}, " encoding
" set statusline+=%{&fileformat}]              " file format
" set statusline+=%=                           " right align
" set statusline+=%{synIDattr(synID(line('.'),col('.'),1),'name')}\  " highlight
" set statusline+=%b,0x%-8B\                   " current char
" set statusline+=%-14.(%l,%c%V%)\ %<%P        " offset


"##############################################################
"#--- PLUGINS                                                 #

" avoid using standard Vim directory names like 'plugin'
" remember to use single quotes

call plug#begin('~/.vim/plugged')

"-------------------------------
" APPEARANCE

Plug 'tomasiser/vim-code-dark'
Plug 'vim-airline/vim-airline'           " Vim Airline statusbar
Plug 'vim-airline/vim-airline-themes'
Plug 'mkitt/tabline.vim'                 " Cleaner tabs
Plug 'kien/rainbow_parentheses.vim'      " Colour matched brackets
Plug 'ryanoasis/vim-devicons'            " Icons for dev file types
" Plug 'tiagofumo/vim-nerdtree-syntax-highlight'  " coloured icons for vim-devicons

"-------------------------------
" CODE EDITING PLUGINS

Plug 'editorconfig/editorconfig-vim'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'                  " so that vim-surroud actions will be repeatable with dot command .
Plug 'tpope/vim-commentary'              " use with 'gc', equivalent to vscode ctrl+/)
" Plug 'terryma/vim-multiple-cursors'       " Install vim-multiple-cursors (equivalent to vscode ctrl+d)
Plug 'scrooloose/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'unblevable/quick-scope'            " Highlight jump characters
Plug 'francoiscabrol/ranger.vim'         " Install ranger-vim integration with ranger file manager
Plug 'christoomey/vim-tmux-navigator'    
Plug 'RRethy/vim-illuminate'             " illuminate other uses of current word under cursor

"-------------------------------
" Git
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-git'

"-------------------------------
" Dev tools (language support, linters, etc..)
Plug 'w0rp/ale'                          " ALE Asynchronous ESLinter for several languages
Plug 'prettier/vim-prettier', { 'do': 'npm install' }
Plug 'amiorin/vim-project'
Plug 'pbrisbin/vim-runfile'
Plug 'chrisbra/Colorizer'                " Show hex codes as colours

"-------------------------------
" SNIPPETS

" Emmet
Plug 'mattn/emmet-vim'

" UltSnips engine
Plug 'SirVer/ultisnips'
" Snippets for UltSnips
Plug 'honza/vim-snippets'


"-------------------------------
" AUTO COMPLETION ENGINES

Plug 'vim-scripts/AutoComplPop'          " Activate omnicomplete suggestions as you type

" Deoplete Completion Engine
" if has('nvim')
"     Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
" else
"     Plug 'Shougo/deoplete.nvim'
"     Plug 'roxma/nvim-yarp'
"     Plug 'roxma/vim-hug-neovim-rpc'
" endif


"-------------------------------
" LANGUAGE SPECIFIC PLUGINS

" HTML
Plug 'adelarsq/vim-matchit' " Matchit extends the % function, navigating to more than a single character
Plug 'valloric/MatchTagAlways'           " Show matching and closing tags

" JavaScript
Plug 'pangloss/vim-javascript'
Plug 'xojs/vim-xo'                       " Install vim-xo for xo linting support
Plug 'mxw/vim-jsx'                       " JSX syntax colors and indent support. Depends on vim-javascript

" Ruby
Plug 'tpope/vim-rake'
Plug 'tpope/vim-bundler'
Plug 'vim-ruby/vim-ruby'
Plug 'tpope/vim-rails'
Plug 'sunaku/vim-ruby-minitest'

" Elixir
Plug 'elixir-editors/vim-elixir'
" elixir module and function completions, mix and iex integration, ...
Plug 'slashmili/alchemist.vim'

" PHP
Plug 'shawncplus/phpcomplete.vim'       " better php omnicompletion

" Latex 
Plug 'lervag/vimtex'

" Markdown
Plug 'suan/vim-instant-markdown'         " Markdown preview instant-markdown-
Plug 'godlygeek/tabular'                 " Tabular align texts that grow
Plug 'plasticboy/vim-markdown'
Plug 'vim-pandoc/vim-pandoc'             " Pandoc for vim
Plug 'vim-pandoc/vim-pandoc-syntax'      " Syntax colors for pandoc-markdown files


Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-sensible'
Plug 'vim-airline/vim-airline'
Plug 'terryma/vim-multiple-cursors'
Plug 'editorconfig/editorconfig-vim'
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-eunuch'
Plug 'ntpeters/vim-better-whitespace'
Plug 'sheerun/vim-polyglot'
Plug 'mhinz/vim-mix-format'
Plug 'vim-scripts/vim-auto-save'
Plug 'nathanaelkane/vim-indent-guides'
" Initialize plugin system
call plug#end()



"##############################################################
"#--- PLUGINS SETTINGS                                        #
"##############################################################

"""""""""""""""""""""""""""""""""""""""
" VIM MARKDOWN SETTINGS

" Disable folding on Vim Markdown
let g:vim_markdown_folding_disabled = 1


"""""""""""""""""""""""""""""""""""""""
" RUBY MINITEST SETTINGS (i_ctrl+x_ctrl+u for minitest method completion)

set completefunc=syntaxcomplete#Complete


"""""""""""""""""""""""""""""""""""""""
" APPLY THE COLOR SCHEME

" This has to be put after the plugins load. The downloaded color scheme
" only becomes available after the plug#end() line

colorscheme codedark


"""""""""""""""""""""""""""""""""""""""
" ALE LINTER SETTINGS

" show linting errors on a separate window
" let g:ale_open_list = 1
" let g:ale_list_window_size = 5


"""""""""""""""""""""""""""""""""""""""
" VIM NATIVE'S OMNICOMPLETE COMPLETION ENGINE SETTINGS
" taken from http://vim.wikia.com/wiki/Make_Vim_completion_popup_menu_work_just_like_in_an_IDE

" The above command will change the 'completeopt' option so that Vim's popup menu doesn't select the first completion item, but rather just inserts the longest common text of all matches; and the menu will come up even if there's only one match.
:set completeopt=longest,menuone

" The following mapping will change the behavior of the <Enter> key when the popup menu is visible. In that case the Enter key will simply select the highlighted menu item, just as <C-Y> does. 
:inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

" Navigate through suggestions with C-j and C-k
inoremap <expr> <C-j> pumvisible() ? "\<C-N>" : "\<C-j>"
inoremap <expr> <C-k> pumvisible() ? "\<C-P>" : "\<C-k>"

" autocmd FileType python set omnifunc=pythoncomplete#Complete
" autocmd FileType javascript set omnifunc=javascriptcomplete#CompleteJS
" autocmd FileType html set omnifunc=htmlcomplete#CompleteTags
" autocmd FileType css set omnifunc=csscomplete#CompleteCSS

"""""""""""""""""""""""""""""""""""""""
" UltSnips Snippets settings
" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<S-tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"




"""""""""""""""""""""""""""""""""""""""
" VIM AIRLINE SETTINGS

" set rtp+=/usr/lib/python3.6/site-packages/powerline/bindings/vim

let g:airline_theme='codedark'                        " this airline theme resembles VSCode blueish status bar
let g:airline#extensions#tabline#enabled=1
let g:airline#extensions#tabline#formatter='unique_tail'
let g:airline_powerline_fonts=1                     " needed by devicons


"""""""""""""""""""""""""""""""""""""""
" NERDTREE SETTINGS

" Toogle NERDTree
map <leader>e :NERDTreeToggle<CR>

" Open NerdTree on Vim startup
autocmd VimEnter * NERDTree
autocmd VimEnter * if argc() | wincmd p | endif

" Open NerdTree directly on the file currently being edited
set autochdir
" Sets the tree root on the current folder in the terminal
let NERDTreeChDirMode=2

" Make NERDTree prettier
let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 1

" Automatically close a tab if the only remaining window is NerdTree:
" autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif


"""""""""""""""""""""""""""""""""""""""
" EMMET SETTINGS
" Use TAB to expand emmet snippets (equivalent to <C-y>,
" let g:user_emmet_expandabbr_key = '<S-tab>'
" Create another binding for emmet, because the default is not great
map <C-Space> <c-y>,

" navigate to edit points
inoremap <C->> <c-y>n<CR>


"""""""""""""""""""""""""""""""""""""""
" VIM COLORIZER SETTINGS

" Make colorizer start when opening or creating files
:autocmd BufNewFile,BufRead * ColorHighlight



"""""""""""""""""""""""""""""""""""""""
" VIM ILLUMINATE SETTINGS

" disable illuminate words on NerdTREE
let g:Illuminate_ftblacklist = ['nerdtree']


"""""""""""""""""""""""""""""""""""""""
" VIM DEVICONS SETTINGS

let g:webdevicons_enable = 1

" adding the flags to NERDTree
let g:webdevicons_enable_nerdtree = 1

" adding to vim-airline's tabline
let g:webdevicons_enable_airline_tabline = 1

" adding to vim-airline's statusline
let g:webdevicons_enable_airline_statusline = 1

" whether or not to show the nerdtree brackets around flags
let g:webdevicons_conceal_nerdtree_brackets = 1

" Force extra padding in NERDTree so that the filetype icons line up vertically
let g:WebDevIconsNerdTreeGitPluginForceVAlign = 1

" Adding the custom source to denite
let g:webdevicons_enable_denite = 1

" This is to solve the issue with NERDTree devicons bugging after
" sourcing vimrc
" this feels like hack (cause it is). try to fix this
if exists("g:loaded_webdevicons")
    call webdevicons#refresh()
endif


"""""""""""""""""""""""""""""""""""""""
" VIM RUNFILE CONFIG

" vim-runfile keymap
noremap <Leader>r, :Run<CR>


"##############################################################
"#--- HELPER METHODS                                          #
"##############################################################

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

" Returns true if paste mode is enabled
function! HasPaste()
    if &paste
        return 'PASTE MODE  '
    endif
    return ''
endfunction

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


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


