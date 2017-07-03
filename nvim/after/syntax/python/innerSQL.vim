" Disable current syntax temporarily.
let saved_syntax = b:current_syntax
unlet! b:current_syntax

" Load hive syntax.
syntax include @hive syntax/hive.vim

" Copied from syntax/python.vim to add the keepend
syn region pythonString matchgroup=pythonQuotes
      \ start=+[uU]\=\z(['"]\)\+\zs[\s\n]*+ end="\z1" skip="\\\\\|\\\z1"
      \ contains=pythonEscape,@Spell keepend
syn region  pythonRawString matchgroup=pythonQuotes
      \ start=+[uU]\=[rR]\z(['"]\)\+\zs[\s\n]*+ end="\z1" skip="\\\\\|\\\z1"
      \ contains=@Spell keepend

syn region SQLEmbedded contains=@hive containedin=pythonString,pythonRawString contained
    \ start=+\v(ALTER|BEGIN|CALL|COMMENT|COMMIT|CONNECT|CREATE|DELETE|DROP|END|EXPLAIN|EXPORT|GRANT|IMPORT|INSERT|LOAD|LOCK|MERGE|REFRESH|RENAME|REPLACE|REVOKE|ROLLBACK|SELECT|SET|TRUNCATE|UNLOAD|UNSET|UPDATE|UPSERT)+
    \ end=+;+

" Restore original syntax.
let b:current_syntax = saved_syntax
unlet! saved_syntax
