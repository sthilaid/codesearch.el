
(defun codesearch-get-trigrams (filepath)
  (defun add-to-data (tri pos datas)
    (let ((data (seq-find (lambda (data) (string= (elt data 0) tri))
                          datas)))
      (if data (progn (setcdr data (cons pos (cdr data)))
                      datas)
        (cons (list tri pos) datas))))
  
  (let ((trigrams '()))
    (with-temp-buffer
      (insert-file-contents-literally filepath)
      (let ((max (- (point-max) 3)))
        (while (< (point) max)
          (let ((tri (buffer-substring (point) (+ (point) 3))))
            (setq trigrams (add-to-data tri (point) trigrams)))
          (forward-char))))

    (setq trigrams (sort trigrams (lambda (a b) (string< (car a) (car b)))))

    (let* ((trigram-strings (vconcat (mapcar 'car trigrams)))
           (trigrams-smallcase-strings (vconcat (mapcar (lambda (tri) (downcase tri)) trigram-strings)))
           (trigram-indices (vconcat (mapcar 'cdr trigrams))))
      (list trigram-strings trigrams-smallcase-strings trigram-indices))))

(setq TEST-DATA (codesearch-get-trigrams "/home/david/emacs-stuff/codesearch.el/codesearch.el"))
(pp TEST-DATA)

(defun codesearch-search (query data &optional is-case-sensitive?)
  (defun split-query (query)
    (mapcar (lambda (sub-query) (let ((l (length sub-query))
                                      (res nil))
                                  (if (< l 3)
                                      (list (cons sub-query 0))
                                    (dotimes (index (- l 2) res)
                                      (setq res (cons (cons (substring sub-query index (+ index 3))
                                                            index)
                                                      res))))))
            (split-string query " *| *" t " *")))

  (let* ((tri-queries (split-query query))
         (data-trigrams (elt data (if is-case-sensitive? 0 1)))
         (data-indices (elt data 2)))
    (mapcar (lambda (tri-query)
              (let ((queries-indices (mapcar (lambda (tri-q) (seq-position data-trigrams (car tri-q))) tri-query)))
                (if (seq-some 'not queries-indices)
                    nil
                  (let ((queries-res (seq-mapn (lambda (tri-q index) (let ((offset (cdr tri-q)))
                                                                       (mapcar (lambda (x) (- x offset))
                                                                               (elt data-indices index))))
                                               tri-query
                                               queries-indices)))
                    (if (= (length queries-res) 1)
                        queries-res
                      (let* ((sorted-queries-res (sort queries-res (lambda (x y) (< (length x) (length y)))))
                             (smallest-q-res (car sorted-queries-res)))
                        (seq-reduce (lambda (acc smallest-q-index)
                                      (if (seq-every-p (lambda (q-res) (seq-find (lambda (index) (= smallest-q-index index))
                                                                                 q-res))
                                                       (cdr sorted-queries-res))
                                          (cons smallest-q-index acc)
                                        acc))
                                    smallest-q-res
                                    nil)))))))
            tri-queries)))

(pp (codesearch-search "  string|   seq" (codesearch-get-trigrams (buffer-file-name (current-buffer))) t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interactive search and results presentation

(defun codesearch-current-word-or-region ()
  "returns string around the point, if no region is being defined, else will return the region content."
  (if (use-region-p)
	  (buffer-substring (region-beginning) (region-end))
	(current-word)))

(define-derived-mode codesearch-results-mode tabulated-list-mode "codesearch-results mode"
  "Major mode browsing codesearch query results"
  (setq tabulated-list-format (vector (list "result"  100 t)
                                      ;(list "line"    100 t)
                                      ))
  (setq tabulated-list-entries
        (mapcar (lambda (result)
                  (list result (vector (format "%d" result))))
                codesearch-query-result))
  (setq tabulated-list-sort-key (cons "result" nil))
  (tabulated-list-init-header)
  (use-local-map (let ((map (make-sparse-keymap)))
                   (set-keymap-parent map tabulated-list-mode-map)
                   ;(define-key map (kbd "s") 'ide-browser-change-sort-key)
                   ;(define-key map (kbd "<return>") 'ide-browser-find-file)
                   map)))

(defvar codesearch-global-data nil)

(defun codesearch-genindex (file)
  (interactive "f")
  (setq codesearch-global-data (codesearch-get-trigrams file)))

(defun codesearch (query)
  "Browse results of codesearch query"
  (interactive (let ((word (codesearch-current-word-or-region)))
                 (list (read-string (concat "Codesearch exp (default: \"" word "\"): ")
                                    nil 'codesearch-history))))

  ;; todo: check that codesearch-global-data is valid
  
  (with-current-buffer (get-buffer-create "*codesearch-results*")
    (let ((codesearch-query-result (apply 'append (codesearch-search query codesearch-global-data))))
      (codesearch-results-mode))
    (tabulated-list-print)
    (switch-to-buffer (current-buffer))))
