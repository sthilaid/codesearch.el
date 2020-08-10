
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
    trigrams))

(setq test-data (sort (codesearch-get-trigrams "/home/david/emacs-stuff/codesearch.el/codesearch.el")
                      (lambda (a b) (string< (car a) (car b)))))
(pp test-data)

(defun codesearch-search (query trigrams)
  (defun split-query (query)
    (let ((l (length query))
          (res nil))
      (if (< l 3)
          l
        (dotimes (index (- l 2) res)
          (setq res (cons (cons (substring query index (+ index 3))
                                index)
                          res))))))

  (let* ((tri-queries (split-query query))
         (queries-res (seq-reduce (lambda (acc q) (let ((q-tri (seq-find (lambda (tri) (string= (car tri) (car q))) trigrams)))
                                                    (cons (cons (car q-tri) (mapcar (lambda (x) (- x (cdr q)))
                                                                                    (cdr q-tri)))
                                                          acc)))
                                  tri-queries
                                  nil))
         (valid-res ))
    (if (= (length queries-res) 1)
        queries-res
      (let ((sorted-queries-res (sort queries-res (lambda (x y) (< (length x) (length y))))))
        (seq-reduce (lambda (acc x) (if (seq-every-p (lambda (rest-el) (seq-find (lambda (y) (= x y))
                                                                                 (cdr rest-el)))
                                                     (cdr sorted-queries-res))
                                        (cons x acc)
                                      acc))
                    (cdr (car sorted-queries-res))
                    nil)))))

(pp (codesearch-search "string" test-data))

