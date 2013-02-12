php-api-complete
=================
Vim omnifunc for php api.

Install
-------
Set your .vimrc as follows.

    " Vundle
    Bundle 'git://github.com:yuratomo/php-api-complete.git'

Settings
--------
Set your .vimrc as follows.
### autoload php-api-complete
    au BufNewFile,BufRead *.php   setl omnifunc=phpapi#complete
    
### show status refarence
    au CompleteDone *.php         call phpapi#showRef()
    au BufNewFile,BufRead *.php   inoremap <expr> <c-down> phpapi#nextRef()
    au BufNewFile,BufRead *.php   inoremap <expr> <c-up>   phpapi#prevRef()

### balloon help
    if has("balloon_eval") && has("balloon_multiline") 
      au BufNewFile,BufRead *.php setl bexpr=phpapi#balloon()
      au BufNewFile,BufRead *.php setl ballooneval
    endif
    
ScreenShots
----------

