;;; init-ibuffer.el --- Set up ibuffer mode
;;; Commentary:

;; My ibuffer configuration

(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer) ;; Use Ibuffer for Buffer List

(setq ibuffer-saved-filter-groups
      '(("default"
	 ("emacs-config" (or (filename . "/.emacs.d/")
			     (filename . ".emacs.d/init.el")))
	 ("OHS" (filename . "/Google Drive/OHS/"))
	 ("Org" (or (mode . org-mode)
		    (filename . "/Dropbox/org/")
		    ;(mode . org-agenda-mode)
		    ))
	 ("planner" (or
		    (name . "\*Calendar\*")
		    (name . "\*Org Agenda\*")
		    (name . "^diary$")))
	 ("Magit" (name . "\*magit\*"))
	 ("ERC" (mode . erc-mode))
	 ("Help" (or (name . "\*Help\*")
		     (name . "\*info\*")
		     (name . "\*GNU Emacs\*"))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))
(provide 'init-ibuffer)
;; init-ibuffer.el ends here
