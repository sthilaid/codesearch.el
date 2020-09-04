;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data structures 

(require 'avl-tree)

(cl-defstruct codesearch-genleaf tri positions)
(cl-defstruct codesearch-data files files-md5 trigrams smallcase-trigrams)

(defun codesearch-data-get-indices (trigrams tri)
  (let ((data (avl-tree-member trigrams
                               (make-codesearch-genleaf :tri tri))))
    (if data (codesearch-genleaf-positions data) nil)))

(defun codesarch-create-trigram-tree ()
  (avl-tree-create (lambda (x y) (string< (codesearch-genleaf-tri x)
                                          (codesearch-genleaf-tri y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generate trigrams data

(defun codesearch-md5-file (file) (if (and file (file-exists-p file))
                                      (with-temp-buffer (insert-file-contents file) (md5 (buffer-string)))))

(defun codesearch-add-to-data (datas tri pos file-index)
  (let ((data (avl-tree-member datas (make-codesearch-genleaf :tri tri))))
    (if data
        (setf (codesearch-genleaf-positions data) (cons (cons file-index pos)
                                                        (codesearch-genleaf-positions data)))
      (avl-tree-enter datas (make-codesearch-genleaf :tri tri :positions (list (cons file-index pos)))))))

(defun codesearch-add-list-to-data (datas tri pos-list)
  (let ((data (avl-tree-member datas (make-codesearch-genleaf :tri tri))))
    (if data
        (setf (codesearch-genleaf-positions data) (append pos-list
                                                          (codesearch-genleaf-positions data)))
      (avl-tree-enter datas (make-codesearch-genleaf :tri tri :positions pos-list)))))

(defun codesearch-create-trigrams (all-filepaths files-md5 new-trigrams trigrams-data)
  (let* ((smallcase-new-trigrams
          (let ((smallcase-datas (codesarch-create-trigram-tree)))
            (avl-tree-mapc (lambda (node) (codesearch-add-list-to-data smallcase-datas
                                                                       (downcase (codesearch-genleaf-tri node))
                                                                       (codesearch-genleaf-positions node)))
                           new-trigrams)
            smallcase-datas)))
    (if (codesearch-data-p trigrams-data)
        (progn
          (setf (codesearch-data-files trigrams-data) all-filepaths)
          (setf (codesearch-data-files-md5 trigrams-data) files-md5)
          (setf (codesearch-data-trigrams trigrams-data) new-trigrams )
          (setf (codesearch-data-smallcase-trigrams trigrams-data) smallcase-new-trigrams )
          trigrams-data)
      (make-codesearch-data :files        all-filepaths
                            :files-md5    files-md5
                            :trigrams     new-trigrams
                            :smallcase-trigrams smallcase-new-trigrams))))

(defun codesearch-valid-trigram (tri)
  (and (stringp tri)
       (= (length tri) 3)
       (not (seq-some (lambda (c) (eq c ?\s)) tri))
       (not (seq-some (lambda (c) (eq c ?\t)) tri))))

(defun codesearch-update-trigrams (filepaths &optional trigrams-data)
  (let* ((start-time (float-time))
         (all-filepaths (if (codesearch-data-p trigrams-data)
                            (codesearch-data-files trigrams-data)
                          [])))
    (setq filepaths (seq-filter (lambda (f) (let ((pos (seq-position all-filepaths f 'string=)))
                                              (or (not pos)
                                                  (not (string= (codesearch-md5-file f)
                                                                (elt (codesearch-data-files-md5 trigrams-data) pos))))))
                                filepaths))
    (let* ((new-trigrams
            (if (codesearch-data-p trigrams-data)
                (codesearch-data-trigrams trigrams-data)
              (codesarch-create-trigram-tree)))
           (filepaths-count (length filepaths))
           (intial-filecount (length all-filepaths)))
      (dotimes (i filepaths-count)
        (let* ((filepath (elt filepaths i))
               (file-index (length all-filepaths)))
          (message (format "generating index for file %s [%d/%d]..." filepath i filepaths-count))

          (let ((existing-file-index (seq-position all-filepaths filepath 'string=)))
            (if existing-file-index (setf (elt all-filepaths existing-file-index) nil)))

          (setq all-filepaths (vconcat all-filepaths (vector filepath)))
          (if (file-exists-p filepath)
              (with-temp-buffer
                (insert-file-contents filepath)
                (let ((max (- (point-max) 3)))
                  (dotimes (i max)
                    (let* ((pos (+ i 1))
                           (tri (buffer-substring pos (+ pos 3))))
                      (if (codesearch-valid-trigram tri)
                          (codesearch-add-to-data new-trigrams tri pos file-index)))))))))

      (prog1 (codesearch-create-trigrams all-filepaths
                                         (mapcar 'codesearch-md5-file all-filepaths)
                                         new-trigrams
                                         trigrams-data)
        (let ((warning-msg (let ((stale-files-count (seq-count (lambda (x) (not x)) all-filepaths)))
                             (if (> stale-files-count 10)
                                 (format " (%d stale files in index data, consider running codesearch-cleanup-data)"
                                         stale-files-count)
                               (if (> stale-files-count 0)
                                   (format " (%d stale files in index data)" stale-files-count)
                                 "")))))
          (message (format "done in %.2f secs%s" (- (float-time) start-time) warning-msg)))))))

;; (setq TEST-DATA (codesearch-update-trigrams (list (buffer-file-name (current-buffer)))))
;; (pp TEST-DATA)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; index serialization

(defun codesearch-serialize-index (data out-file)
  (let* ((files (codesearch-data-files data))
         (files-md5 (codesearch-data-files-md5 data))
         (trigrams (codesearch-data-trigrams data))
         (raw-data (list files files-md5 (avl-tree-flatten trigrams))))
    (with-temp-file out-file
        (insert (with-output-to-string (print raw-data))))
    (message (format "index saved in %s" out-file))))

(defun codesearch-deserialize-index (index-filename)
  (if (not (file-exists-p index-filename))
      (error (format "index file %s does not exists... aborting" index-filename)))
  (with-temp-buffer
    (insert-file-contents index-filename)
    (let* ((raw-data-str (buffer-string))
           (raw-data (read raw-data-str))
           (files (elt raw-data 0))
           (files-md5 (elt raw-data 1))
           (trigrams (elt raw-data 2))
           (data (make-codesearch-data)))
      (let ((trigram-tree (codesarch-create-trigram-tree)))
        (dolist (tri-data trigrams)
          (avl-tree-enter trigram-tree tri-data))
        (codesearch-create-trigrams files files-md5 trigram-tree data))
      data)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; perform a search query in the data and return results

(defun codesearch-split-query (query trigrams)
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

(defun codesearch-search (query data &optional is-case-sensitive?)
  (let* ((data-trigrams (if is-case-sensitive? (codesearch-data-trigrams data) (codesearch-data-smallcase-trigrams data)))
         (tri-queries (codesearch-split-query (if is-case-sensitive? query (downcase query)) data-trigrams))
         (results
          (mapcar (lambda (tri-query)
                    (let ((queries-indices (mapcar (lambda (tri-q) (vconcat (codesearch-data-get-indices data-trigrams (car tri-q))))
                                                   tri-query)))
                      (if (seq-some 'not queries-indices)
                          nil
                        (let ((queries-res (seq-mapn (lambda (tri-q index)
                                                       (let ((offset (cdr tri-q)))
                                                         (mapcar (lambda (x) (cons (car x) (- (cdr x) offset)))
                                                                 (codesearch-data-get-indices data-trigrams (car tri-q)))))
                                                     tri-query
                                                     queries-indices)))
                          (if (= (length queries-res) 1)
                              (car queries-res)
                            (let* ((sorted-queries-res (sort queries-res (lambda (x y) (< (length x) (length y)))))
                                   (smallest-q-res (car sorted-queries-res)))
                              (seq-filter (lambda (smallest-q-index)
                                            (seq-every-p (lambda (q-res)
                                                           ;; if file-index and pos match for every AND queries
                                                           (seq-find (lambda (index) (and (= (car smallest-q-index)
                                                                                             (car index))
                                                                                          (= (cdr smallest-q-index)
                                                                                             (cdr index))))
                                                                     q-res))
                                                         (cdr sorted-queries-res)))
                                          smallest-q-res)))))))
                  tri-queries)))
    (if (and (> (length results) 0) (seq-some (lambda (x) x) results))
        (let ((files (codesearch-data-files data)))
            (mapcar (lambda (r) (seq-filter (lambda (hit) (elt files (car hit)))
                                            r))
                    results))
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
        (progn (find-file (elt entry 2))
               (goto-char (elt entry 3))))))

(defcustom codesearch-default-index-file "~/.codesearch-index"
  "File used to store default global index"
  :type 'string
  :group 'codesearch)

(defvar codesearch-font-lock-defaults nil)
(defvar codesearch-global-data nil)
(defvar codesearch-global-data-file nil)
(defvar codesearch-results-show-full-path? nil)
(defvar codesearch-query-result nil)
(defvar codesearch-last-query nil)

(defvar codesearch-result-highlight-face 'codesearch-result-highlight-face)
(defface codesearch-result-highlight-face
  `((t :weight bold :slant italic :background ,(face-foreground 'default) :foreground ,(face-background 'default)))
  "Used to highlight codesearch results"
  :group 'codesearch-faces)

(define-derived-mode codesearch-results-mode tabulated-list-mode "codesearch-results mode"
  "Major mode browsing codesearch query results"
  (let* ((results (sort (seq-uniq (apply 'append codesearch-query-result)
                                  (lambda (x y) (and (= (car x) (car y))
                                                     (= (cdr x) (cdr y)))))
                        (lambda (x y) (if (= (car x) (car y))
                                          (< (cdr x) (cdr y))
                                        (< (car x) (car y))))))
         (max-filename-width (+ (seq-reduce (lambda (acc x) (let* ((f (elt (codesearch-data-files codesearch-global-data) (car x)))
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
                              (let* ((filepath (elt (codesearch-data-files codesearch-global-data) (car result)))
                                     (filename (if codesearch-results-show-full-path?
                                                   filepath
                                                 (file-name-nondirectory filepath)))
                                     (pos (cdr result))
                                     (result-line (if (not (file-exists-p filepath))
                                                      (cons "??" "!unknown file content (file not found)!")
                                                      (with-temp-buffer filepath
                                                                        (insert-file-contents filepath)
                                                                        (goto-char pos)
                                                                        (let ((line-num (line-number-at-pos pos))
                                                                              (line-start (line-beginning-position))
                                                                              (line-end (line-end-position)))
                                                                          (cons line-num (buffer-substring line-start line-end)))))))
                                (list pos (vector (format "%s:%d" filename (car result-line))
                                                  (cdr result-line)
                                                  filepath
                                                  pos))))
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

(defun codesearch-index-add-file (file)
  "Generate a trigram search index for the specified file. Can be
searche with the 'codesearch command."
  (interactive "f")
  (setq codesearch-global-data (codesearch-update-trigrams (list file) codesearch-global-data)))

(defun codesearch-index-add-dir (dir regexp)
  "Generate a trigram search index for all the mathching files in
specified directory. Can be searche with the 'codesearch
command."
  (interactive "Ddir:\nsregexp:")
  (let* ((files (directory-files-recursively dir ".*"))
         (cleaned-up-files (seq-filter (lambda (x) (and (stringp x) (not (string= x ""))
                                                        (not (string-match "/\\." x))
                                                        (not (string-match "/~" x))
                                                        (not (string-match "/#" x))
                                                        (not (string-match "~$" x))
                                                        (string-match regexp x)))
                                       files)))
    (setq codesearch-global-data (codesearch-update-trigrams cleaned-up-files codesearch-global-data))))

(defun codesearch-index-save (&optional index-file)
  "Saves the current global codesearch index in the provided index file (default location in ~/.codesearch-index)."
  (interactive (let ((file-input (ido-read-file-name "save to index file: "
                                                     (file-name-directory (if codesearch-global-data-file
                                                                              codesearch-global-data-file
                                                                            codesearch-default-index-file))
                                                     nil
                                                     nil
                                                     (file-name-nondirectory (if codesearch-global-data-file
                                                                                 codesearch-global-data-file
                                                                               codesearch-default-index-file)))))
                 (list file-input)))
  (codesearch-serialize-index codesearch-global-data index-file))

(defun codesearch-index-load (&optional index-file)
  "Loads the current global codesearch index from the provided index file (default location in ~/.codesearch-index)."
  (interactive (let ((file-input (ido-read-file-name "load index file: "
                                                     (file-name-directory (if codesearch-global-data-file
                                                                              codesearch-global-data-file
                                                                            codesearch-default-index-file))
                                                     nil
                                                     nil
                                                     (file-name-nondirectory (if codesearch-global-data-file
                                                                                 codesearch-global-data-file
                                                                               codesearch-default-index-file)))))
                 (list file-input)))
  (setq codesearch-global-data (codesearch-deserialize-index index-file))
  (setq codesearch-global-data-file index-file)
  (message (format "%d files indexing loaded from %s"
                   (length (codesearch-data-files codesearch-global-data))
                   index-file)))

(defun codesearch-index-update ()
  "Update all modified file listed in the index"
  (interactive)
  (codesearch-update-trigrams (codesearch-data-files codesearch-global-data) codesearch-global-data))

(defun codesearch-index-reset ()
  "Clears completely currently active codesearch index."
  (interactive)
  (setq codesearch-global-data nil)
  (setq codesearch-global-data-file nil)
  (message "current index was reset successfully"))

(defun codesearch-cleanup-data (&optional data)
  "Removes all stale data in the current codesearch index. Will
regenerate all the data so it will take some time."
  (interactive)
  (if (not data) (setq data codesearch-global-data))
  (let* ((files (seq-filter (lambda (x) x) (codesearch-data-files data)))
         (new-data (codesearch-update-trigrams files nil)))
    (setf (codesearch-data-files data) files)
    (setf (codesearch-data-files-md5 data) (codesearch-data-files-md5 new-data))
    (setf (codesearch-data-trigrams data) (codesearch-data-trigrams new-data))
    (setf (codesearch-data-smallcase-trigrams data) (codesearch-data-smallcase-trigrams new-data)))

  ;; -- this should cleanup the data, but is extremely slow and it seems that it
  ;; -- is faster to simply faster to regenerate the index data
  ;; (let* ((old-files (codesearch-data-files data))
  ;;        (new-files (seq-filter (lambda (f) f) old-files))
  ;;        (new-files-indices (mapcar (lambda (f) (seq-position new-files f))
  ;;                                   old-files))
  ;;        (valid-file-indices (cl-loop for f in old-files
  ;;                                     for i from 0 to (length old-files)
  ;;                                     if f collect i))
  ;;        (cleaned-up-entries-count 0))
  ;;   (avl-tree-mapc (lambda (leaf) (setf (codesearch-genleaf-positions leaf)
  ;;                                       (let ((old-positions (codesearch-genleaf-positions leaf))
  ;;                                             (new-positions '()))
  ;;                                         (dotimes (i (length old-positions))
  ;;                                           (let ((pos (elt old-positions i)))
  ;;                                             (if (member (car pos) valid-file-indices)
  ;;                                                 (push (cons (elt new-files-indices (car pos)) (cdr pos))
  ;;                                                       new-positions)
  ;;                                               (incf cleaned-up-entries-count))))
  ;;                                         new-positions)))
  ;;                  (codesearch-data-trigrams data))
  ;;   (prog1 (codesearch-create-trigrams new-files new-files-md5 (codesearch-data-trigrams data) data)
  ;;     (message (format "cleaned up %d trigram entries from %d files"
  ;;                      cleaned-up-entries-count
  ;;                      (- (length old-files) (length new-files))))))
  )

(defun codesearch (query is-case-sensitive?)
  "Perform and browse results of codesearch query. Requires that
a codesearch index is loaded in memory."
  (interactive (let* ((word (codesearch-current-word-or-region))
                      (input (read-string (concat "Codesearch exp (default: \"" word "\"): ")
                                          nil 'codesearch-history)))
                 (list (if (string= "" input) word input)
                       (y-or-n-p "case sensitive search?"))))
  (if (or (not (boundp 'codesearch-global-data))
          (not codesearch-global-data))
      (message (format "global index is not set, please run codesearch-genindex-... function first"))
    (progn
      (with-current-buffer (get-buffer-create "*codesearch-results*")
        (setq codesearch-last-query query)
        (setq codesearch-query-result (codesearch-search query codesearch-global-data is-case-sensitive?))
        (codesearch-results-mode)
        (tabulated-list-print)
        (switch-to-buffer (current-buffer)))
      (let ((result-count (seq-reduce (lambda (acc x) (+ acc (length x)))
                                  codesearch-query-result
                                  0)))
    (message (if (> result-count 0)
                 (format "found %d matches" result-count)
               (format "found no match")))))))

(provide 'codesearch)
