;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data structures 

(require 'avl-tree)

(cl-defstruct codesearch-genleaf tri positions)
(cl-defstruct codesearch-result files trigrams smallcase-trigrams)
(defun codesearch-result-get-indices (trigrams tri)
  (let ((data (avl-tree-member trigrams
                               (make-codesearch-genleaf :tri tri))))
    (if data (codesearch-genleaf-positions data) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generate trigrams data

(defun codesearch-update-trigrams (filepaths &optional trigrams-data)
  (defun add-to-data (datas tri pos file-index)
    (let ((data (avl-tree-member datas (make-codesearch-genleaf :tri tri))))
      (if data
          (setf (codesearch-genleaf-positions data) (cons (cons file-index pos)
                                                          (codesearch-genleaf-positions data)))
        (avl-tree-enter datas (make-codesearch-genleaf :tri tri :positions (list (cons file-index pos)))))))

  (defun add-list-to-data (datas tri pos-list)
    (let ((data (avl-tree-member datas (make-codesearch-genleaf :tri tri))))
      (if data
          (setf (codesearch-genleaf-positions data) (append pos-list
                                                            (codesearch-genleaf-positions data)))
        (avl-tree-enter datas (make-codesearch-genleaf :tri tri :positions pos-list)))))

  (defun purge-stale-data (trigrams)
    (let* ((datas-files  (codesearch-result-files trigrams-data))
           (file-indices (seq-reduce (lambda (acc file) (let ((pos (seq-position datas-files file 'string=)))
                                                          (if pos (cons pos acc) acc)))
                                     filepaths
                                     '())))
      (avl-tree-mapc (lambda (node) (setf (codesearch-genleaf-positions node)
                                          (seq-filter (lambda (x) (not (seq-contains file-indices (car x))))
                                                      (codesearch-genleaf-positions node))))
                     trigrams)))

  (let* ((new-trigrams
          (if (codesearch-result-p trigrams-data)
              (let ((existing-trigrams (codesearch-result-trigrams trigrams-data)))
                (message (format "purging existing trigrams for reindexed files..."))
                (purge-stale-data existing-trigrams)
                existing-trigrams)
            (avl-tree-create (lambda (x y) (string< (codesearch-genleaf-tri x)
                                                    (codesearch-genleaf-tri y))))))
         (all-filepaths (if (codesearch-result-p trigrams-data)
                            (codesearch-result-files trigrams-data)
                          '()))
         (file-index-offset (length all-filepaths)))

    (let ((filepaths-count (length filepaths)))
     (dotimes (i filepaths-count)
       (let* ((filepath (elt filepaths i))
              (file-index (let ((index (seq-position all-filepaths filepath 'string=)))
                            (if index
                                index
                              (progn (setq all-filepaths (append all-filepaths (list filepath)))
                                     (- (length all-filepaths) 1))))))
         (message (format "generating index for file %s [%d/%d]..." filepath i filepaths-count))
         (with-temp-buffer
           (insert-file-contents filepath)
           (let ((max (- (point-max) 3)))
             (dotimes (i max)
               (let* ((pos (+ i 1))
                      (tri (buffer-substring pos (+ pos 3))))
                 (add-to-data new-trigrams tri pos file-index))))))))
    
    (let* ((smallcase-new-trigrams
            (let ((smallcase-datas (avl-tree-create (lambda (x y) (string< (codesearch-genleaf-tri x)
                                                                           (codesearch-genleaf-tri y))))))
              (avl-tree-mapc (lambda (node) (add-list-to-data smallcase-datas
                                                              (downcase (codesearch-genleaf-tri node))
                                                              (codesearch-genleaf-positions node)))
                             new-trigrams)
              smallcase-datas)))
      (prog1
          (if (codesearch-result-p trigrams-data)
              (progn
                (setf (codesearch-result-files trigrams-data)
                      all-filepaths)
                (setf (codesearch-result-smallcase-trigrams trigrams-data)
                      smallcase-new-trigrams )
                trigrams-data)
            (make-codesearch-result :files      filepaths
                                    :trigrams   new-trigrams
                                    :smallcase-trigrams smallcase-new-trigrams))
        (message "done")))))

;; (setq TEST-DATA (codesearch-update-trigrams (list (buffer-file-name (current-buffer)))))
;; (pp TEST-DATA)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; perform a search query in the data and return results

(defun codesearch-search (query data &optional is-case-sensitive?)
  (defun split-query (query trigrams)
    (let ((split-queries (mapcar (lambda (sub-query) (let ((l (length sub-query))
                                                           (res nil))
                                                       (if (< l 3)
                                                           sub-query ;; processed after the fact (OR statements)
                                                         (dotimes (index (- l 2) res)
                                                           (setq res (cons (cons (substring sub-query index (+ index 3))
                                                                                 index)
                                                                           res))))))
                                 (split-string query " *| *" t))))
      (seq-reduce (lambda (acc q) (if (not (stringp q))
                                      (cons q acc)
                                    (seq-reduce (lambda (acc tri) (let ((pos (string-match q tri)))
                                                                    (if pos (cons (list (cons tri pos)) acc) acc)))
                                                trigrams
                                                acc)))
                  split-queries
                  '())))

  (let* ((data-trigrams (if is-case-sensitive? (codesearch-result-trigrams data) (codesearch-result-smallcase-trigrams data)))
         (tri-queries (split-query (if is-case-sensitive? query (downcase query)) data-trigrams))
         (results
          (mapcar (lambda (tri-query)
                    (let ((queries-indices (mapcar (lambda (tri-q) (vconcat (codesearch-result-get-indices data-trigrams (car tri-q))))
                                                   tri-query)))
                      (if (seq-some 'not queries-indices)
                          nil
                        (let ((queries-res (seq-mapn (lambda (tri-q index)
                                                       (let ((offset (cdr tri-q)))
                                                         (mapcar (lambda (x) (cons (car x) (- (cdr x) offset)))
                                                                 (codesearch-result-get-indices data-trigrams (car tri-q)))))
                                                     tri-query
                                                     queries-indices)))
                          (if (= (length queries-res) 1)
                              (car queries-res)
                            (let* ((sorted-queries-res (sort queries-res (lambda (x y) (< (length x) (length y)))))
                                   (smallest-q-res (car sorted-queries-res)))
                              (seq-reduce (lambda (acc smallest-q-index)
                                            (if (seq-every-p (lambda (q-res)
                                                               ;; if file-index and pos match for every AND queries
                                                               (seq-find (lambda (index) (and (= (car smallest-q-index)
                                                                                                 (car index))
                                                                                              (= (cdr smallest-q-index)
                                                                                                 (cdr index))))
                                                                         q-res))
                                                             (cdr sorted-queries-res))
                                                (cons smallest-q-index acc)
                                              acc))
                                          smallest-q-res
                                          nil)))))))
                  tri-queries)))
    (if (and (> (length results) 0) (seq-some (lambda (x) x) results))
        results
      nil)))

;;(pp (codesearch-search "  string|   seq-" (codesearch-update-trigrams (list (buffer-file-name (current-buffer)))) t))

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

(defvar codesearch-result-highlight-face 'codesearch-result-highlight-face)
(defface codesearch-result-highlight-face
  `((t :weight bold :slant italic :background ,(face-foreground 'default) :foreground ,(face-background 'default)))
  "Used to highlight codesearch results")

(define-derived-mode codesearch-results-mode tabulated-list-mode "codesearch-results mode"
  "Major mode browsing codesearch query results"
  (let* ((results (sort (seq-uniq (apply 'append codesearch-query-result)
                                  (lambda (x y) (and (= (car x) (car y))
                                                     (= (cdr x) (cdr y)))))
                        (lambda (x y) (if (= (car x) (car y))
                                          (< (cdr x) (cdr y))
                                        (< (car x) (car y))))))
         (max-filename-width (+ (seq-reduce (lambda (acc x) (let* ((f (elt (codesearch-result-files codesearch-global-data) (car x)))
                                                                   (display-f (if codesearch-results-show-full-path?
                                                                                  f
                                                                                (file-name-nondirectory f)))
                                                                   (width (length display-f)))
                                                              (if (> width acc) width acc)))
                                            results
                                            0)
                                5)))
    (setq tabulated-list-format (vector (list "file" max-filename-width (lambda (x y) (string< (elt (cadr x) 0) (elt (cadr y) 0))))
                                        (list "matching line" 100 t)))
    (let ((entries (seq-map (lambda (result)
                              (let* ((filepath (elt (codesearch-result-files codesearch-global-data) (car result)))
                                     (filename (if codesearch-results-show-full-path?
                                                   filepath
                                                 (file-name-nondirectory filepath)))
                                     (pos (cdr result)))
                                (with-temp-buffer filepath
                                                  (insert-file-contents filepath)
                                                  (goto-char pos)
                                                  (let ((line-num (line-number-at-pos pos))
                                                        (line-start (line-beginning-position))
                                                        (line-end (line-end-position)))
                                                    (list pos (vector (format "%s:%d" filename line-num)
                                                                      (buffer-substring line-start line-end)
                                                                      filepath
                                                                      pos))))))
                            results)))
      (setq tabulated-list-entries entries))
    (setq tabulated-list-sort-key (cons "file" nil)))
    
  (tabulated-list-init-header)
  (setq codesearch-font-lock-defaults `((,codesearch-last-query . codesearch-result-highlight-face)))
  (setq font-lock-defaults '(codesearch-font-lock-defaults))
  (use-local-map (let ((map (make-sparse-keymap)))
                   (set-keymap-parent map tabulated-list-mode-map)
                   (define-key map (kbd "<return>") 'codesearch-result-find-file)
                   (define-key map (kbd "t") (lambda () (interactive) (pp (tabulated-list-get-entry))))
                   (define-key map (kbd "P") (lambda () (interactive) (progn (setq codesearch-results-show-full-path?
                                                                                   (not codesearch-results-show-full-path?))
                                                                             (codesearch-results-mode)
                                                                             (tabulated-list-print))))
                   map)))

(defvar codesearch-global-data nil)
(defvar codesearch-results-show-full-path? nil)
(defvar codesearch-query-result nil)
(defvar codesearch-last-query nil)

(defun codesearch-genindex-from-file (file)
  (interactive "f")
  (setq codesearch-global-data (codesearch-update-trigrams (list file) codesearch-global-data)))

(defun codesearch-genindex-from-dir (filepaths)
  (interactive "Xfile expression:")
  (setq codesearch-global-data (codesearch-update-trigrams filepaths codesearch-global-data)))

(defun codesearch-purge ()
  (interactive)
  (setq codesearch-global-data nil))

(defun codesearch (query is-case-sensitive?)
  "Browse results of codesearch query"
  (interactive (let* ((word (codesearch-current-word-or-region))
                      (input (read-string (concat "Codesearch exp (default: \"" word "\"): ")
                                          nil 'codesearch-history)))
                 (list (if (string= "" input) word input)
                       (y-or-n-p "case sensitive search?"))))
  ;; todo: check that codesearch-global-data is valid
  
  (with-current-buffer (get-buffer-create "*codesearch-results*")
    (setq codesearch-last-query query)
    (setq codesearch-query-result (codesearch-search query codesearch-global-data is-case-sensitive?))
    (codesearch-results-mode)
    (tabulated-list-print)
    (switch-to-buffer (current-buffer))))

(provide 'codesearch)
