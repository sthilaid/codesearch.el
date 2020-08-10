
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generate trigrams data

(cl-defstruct codesearch-result filepath trigrams smallcase-trigrams indices)

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
      (make-codesearch-result :filepath filepath
                              :trigrams trigram-strings
                              :smallcase-trigrams trigrams-smallcase-strings
                              :indices trigram-indices))))

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

  (let* ((tri-queries (split-query query))
         (data-trigrams (if is-case-sensitive? (codesearch-result-trigrams data) (codesearch-result-smallcase-trigrams data)))
         (data-indices (codesearch-result-indices data))
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
  (let* ((file (car codesearch-query-result))
         (results (apply 'append (cdr codesearch-query-result)))
         (max-filename-width (+ (length file) 5)))
    (setq tabulated-list-format (vector (list "file"  max-filename-width t)
                                        (list "text"    100 t)))

    (let ((entries (with-temp-buffer
                     (insert-file-contents-literally file)
                     (let ((lines (mapcar (lambda (pos) (line-number-at-pos pos t)) results)))
                       (seq-mapn (lambda (pos line)
                                   (goto-char pos)
                                   (let ((line-start (line-beginning-position))
                                         (line-end (line-end-position)))
                                     (list pos (vector (format "%s:%d" file line)
                                                       (buffer-substring line-start line-end)
                                                       file
                                                       pos))))
                                 results
                                 lines)))))
      (setq tabulated-list-entries entries))
    (setq tabulated-list-sort-key (cons "file" nil)))
    
  (tabulated-list-init-header)
  (use-local-map (let ((map (make-sparse-keymap)))
                   (set-keymap-parent map tabulated-list-mode-map)
                                        ;(define-key map (kbd "s") 'ide-browser-change-sort-key)
                   (define-key map (kbd "<return>") 'codesearch-result-find-file)
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
    (let ((codesearch-query-result (codesearch-search query codesearch-global-data)))
      (codesearch-results-mode))
    (tabulated-list-print)
    (switch-to-buffer (current-buffer))))
