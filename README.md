# codesearch.el

Google Codesearch (inefficient) implementation in elisp. 

## Instalation

You need to have all elisp files included in this package in the load path, for
example:
```
(add-to-list 'load-path "<path-to-repo>/codesearch.el/")
(require 'codesearch)
```


## Generating index Several interactive function can be used to generate a new
index or update an existing one. Currently there are

```
codesearch-index-add-file   ; add/update file to the index
codesearch-index-add-dir    ; add/update all file matching given regex to the index
codesearch-index-add-update ; updates all files listed in the index
```

If a file is listed, it will only regenerate it's trigrams if the md5 sum
changed since the previous indexation.

## Save/Loading index
An index can be saved using the `codeseach-index-save`
interactive function. Similarly, an existing saved index can be loaded using
`codesearch-index-load`.

## Searching
Searches can be conducted using the `codesearch` interactive
function. The search can be either case sensitive or not. The queries are
textuals but the pipe | character can be used to search for multiple queries in
an OR fasion. The pipe character cannot currently be escaped.

## References

Codesearch paper by Russ Cox @ https://swtch.com/~rsc/regexp/regexp4.html

Dr qubit's avl-tree implementation @ http://www.dr-qubit.org/tags/computing-code-emacs.html
