;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil
  (eval local-set-key
		(kbd "M-e")
		'(lambda nil
		   (interactive)
		   (save-buffer)
		   (compile "make -C ~/Work/Study/in-haste/")))))
