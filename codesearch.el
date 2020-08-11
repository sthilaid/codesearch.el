
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generate trigrams data

(cl-defstruct codesearch-result filepath trigrams indices smallcase-trigrams smallcase-indices)

(defun codesearch-get-trigrams (filepath)
  (defun add-to-data (tri pos datas)
    (let ((data (seq-find (lambda (data) (string= (elt data 0) tri))
                          datas)))
      (if data (progn (setcdr data (cons pos (cdr data)))
                      datas)
        (cons (list tri pos) datas))))

  (defun add-list-to-data (tri pos-list datas)
    (let ((data (seq-find (lambda (data) (string= (elt data 0) tri))
                          datas)))
      (if data (progn (setcdr data (append pos-list (cdr data)))
                      datas)
        (cons (cons tri pos-list) datas))))
  
  (let ((trigrams '()))
    (with-temp-buffer
      (insert-file-contents filepath)
      (let ((max (- (point-max) 3)))
        (dotimes (i max)
          (let* ((pos (+ i 1))
                 (tri (buffer-substring pos (+ pos 3))))
            (setq trigrams (add-to-data tri pos trigrams))))))

    (setq trigrams (sort trigrams (lambda (a b) (string< (car a) (car b)))))

    (let* ((trigram-strings (vconcat (mapcar 'car trigrams)))
           (trigram-indices (vconcat (mapcar 'cdr trigrams)))
           (smallcase-trigrams (let ((smallcase-datas '()))
                                 (dolist (tri-data trigrams smallcase-datas)
                                   (setq smallcase-datas (add-list-to-data (downcase (car tri-data)) (cdr tri-data) smallcase-datas)))))
           (trigrams-smallcase-strings (vconcat (mapcar 'car smallcase-trigrams)))
           (trigrams-smallcase-indices (vconcat (mapcar 'cdr smallcase-trigrams))))
      (make-codesearch-result :filepath filepath
                              :trigrams trigram-strings
                              :indices trigram-indices
                              :smallcase-trigrams trigrams-smallcase-strings
                              :smallcase-indices trigrams-smallcase-indices))))

(setq TEST-DATA (codesearch-get-trigrams (buffer-file-name (current-buffer))))
(pp TEST-DATA)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; perform a search query in the data and return results

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

  (let* ((tri-queries (split-query (if is-case-sensitive? query (downcase query))))
         (data-trigrams (if is-case-sensitive? (codesearch-result-trigrams data) (codesearch-result-smallcase-trigrams data)))
         (data-indices (if is-case-sensitive? (codesearch-result-indices data) (codesearch-result-smallcase-indices data)))
         ;; todo: for queries with less than 3 chars, fetch all the match-string trigrams and replace the query with those
         (results
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
                              (car queries-res)
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
    (if (> (length results) 0)
        (cons (codesearch-result-filepath data) results)
      nil)))

(pp (codesearch-search "  string|   seq-" (codesearch-get-trigrams (buffer-file-name (current-buffer))) t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interactive search and results presentation

(defun codesearch-current-word-or-region ()
  "returns string around the point, if no region is being defined, else will return the region content."
  (if (use-region-p)
	  (buffer-substring (region-beginning) (region-end))
	(current-word)))

(defun codesearch-result-find-file ()
  (interactive)
  (let ((entry (tabulated-list-get-entry)))
    (if entry
        (progn (pp entry)
               (find-file (elt entry 2))
               (goto-char (elt entry 3))))))

(define-derived-mode codesearch-results-mode tabulated-list-mode "codesearch-results mode"
  "Major mode browsing codesearch query results"
  (let* ((filepath (car codesearch-query-result))
         (filename (file-name-nondirectory filepath))
         (results (sort (seq-uniq (apply 'append (cdr codesearch-query-result))) '<))
         (max-filename-width (+ (length filename) 5)))
    (setq tabulated-list-format (vector (list "file" max-filename-width (lambda (x y) (< (car x) (car y))))
                                        (list "matching line" 100 t)))
    (let ((entries (with-temp-buffer
                     (insert-file-contents filepath)
                     (seq-map (lambda (pos)
                                (goto-char pos)
                                (let ((line-num (line-number-at-pos pos))
                                      (line-start (line-beginning-position))
                                      (line-end (line-end-position)))
                                  (list pos (vector (format "%s:%d" filename line-num)
                                                    (buffer-substring line-start line-end)
                                                    filepath
                                                    pos))))
                              results))))
      (setq tabulated-list-entries entries))
    (setq tabulated-list-sort-key (cons "file" nil)))
    
  (tabulated-list-init-header)
  (use-local-map (let ((map (make-sparse-keymap)))
                   (set-keymap-parent map tabulated-list-mode-map)
                                        ;(define-key map (kbd "s") 'ide-browser-change-sort-key)
                   (define-key map (kbd "<return>") 'codesearch-result-find-file)
                   (define-key map (kbd "t") (lambda () (interactive) (pp (tabulated-list-get-entry))))
                   map)))

(defvar codesearch-global-data nil)

(defun codesearch-genindex (file)
  (interactive "f")
  (setq codesearch-global-data (codesearch-get-trigrams file)))

(defun codesearch (query is-case-sensitive?)
  "Browse results of codesearch query"
  (interactive (let ((word (codesearch-current-word-or-region)))
                 (list (read-string (concat "Codesearch exp (default: \"" word "\"): ")
                                    nil 'codesearch-history)
                       (y-or-n-p "case sensitive search?"))))

  ;; todo: check that codesearch-global-data is valid
  
  (with-current-buffer (get-buffer-create "*codesearch-results*")
    (let ((codesearch-query-result (codesearch-search query codesearch-global-data is-case-sensitive?)))
      (codesearch-results-mode))
    (tabulated-list-print)
    (switch-to-buffer (current-buffer))))
