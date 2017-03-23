" Set text width to 78.
"" This is per RFC 5322, which states, "Each line of characters MUST be no more
""+than 998 characters, and SHOULD be no more than 78 characters, excluding the
""+CRLF."
setlocal textwidth=78

" Automatic formatting
"" a - automatic formatting of paragraphs
"" w - trailing whitespace means paragraph continues on next line
setlocal formatoptions+=aw

" Use English spellcheck
set spell spelllang=en

" allow a maximum of 2 trailing spaces
let b:wstrip_trailing_max=2

" Define an autocmd group to strip blank trailing quotes
augroup StripTrailMailQuote
	autocmd!
	autocmd BufWritePre * :call StripTrailMailQuote()
augroup END

