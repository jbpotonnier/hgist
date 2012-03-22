(setq-default display-buffer-reuse-frames t)

(defun test-command ()
  (interactive)
  (compile "runhaskell HGistTest.hs"))

(setq project-directory-substrings
      '("hgist"))

(defun project-save-hook ()
  (dolist (project project-directory-substrings)
    (if (string-match project
		      (buffer-file-name nil))
	(test-command))))

(add-hook 'after-save-hook
	  'project-save-hook)
