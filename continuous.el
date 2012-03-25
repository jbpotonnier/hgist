(setq-default display-buffer-reuse-frames t)

(defun test-command ()
  (interactive)
  (compile "runhaskell -W HGistTest.hs && ghc -W -o hgist Main.hs")
)

(setq project-directory-substrings
      '("hgist"))

(defun project-save-hook ()
  (dolist (project project-directory-substrings)
    (if (string-match project
		      (buffer-file-name nil))
	(test-command))))

(add-hook 'after-save-hook
	  'project-save-hook)
