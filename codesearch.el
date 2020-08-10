
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

    (let ((trigram-strings (vconcat (mapcar 'car trigrams)))
          (trigram-indices (vconcat (mapcar 'cdr trigrams))))
      (list trigram-strings trigram-indices))))

(setq test-data (codesearch-get-trigrams "/home/david/emacs-stuff/codesearch.el/codesearch.el"))
(pp (test-data))

(defun codesearch-search (query data)
  (defun split-query (query)
    (let ((l (length query))
          (res nil))
      (if (< l 3)
          (list (cons query 0))
        (dotimes (index (- l 2) res)
          (setq res (cons (cons (substring query index (+ index 3))
                                index)
                          res))))))

  (let* ((tri-queries (split-query query))
         (data-trigrams (elt data 0))
         (data-indices (elt data 1))
         (queries-indices (mapcar (lambda (tri-q) (seq-position data-trigrams (car tri-q))) tri-queries)))

    (if (seq-some 'not queries-indices)
        nil
      (let ((queries-res (seq-mapn (lambda (tri-q index) (let ((offset (cdr tri-q)))
                                                           (mapcar (lambda (x) (- x offset))
                                                                   (elt data-indices index))))
                                   tri-queries
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

(pp (codesearch-search "string" (codesearch-get-trigrams (buffer-file-name (current-buffer)))))
