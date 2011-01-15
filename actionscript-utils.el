(defun jump-to-as-error ()
  (interactive)
  (let ((line (buffer-subline-no-properties)))
	(let-seq (filename line-no-str nothing col-no-str)
					   (filter (not-f #'empty?) (split-string line "[():\t ]"))
					   (let ((buf (find-file filename)))
						 (with-current-buffer buf
						   (goto-char (point-min))
						   (forward-line (- (string-to-number line-no-str) 1))
						   (forward-char (- (string-to-number col-no-str) 1)))))))

(provide 'actionscript-utils)


