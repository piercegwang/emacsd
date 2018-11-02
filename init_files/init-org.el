;;; init-org.el --- Set up Org Mode
;;; Commentary:

;; Basic Org Mode configuration, assuming presence of Evil & Evil Leader.

(require 'org)
(setq org-directory "~/Dropbox/org/")
(setq org-agenda-files (list "~/Dropbox/org/school.org"
			     "~/Dropbox/org/todo.org"
			     "~/Dropbox/org/violin.org"))
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "DONE")))
'((sequence "TODO" "IN-PROGRESS" "DONE"))
(setq org-default-notes-file (concat org-directory "/todo.org"))
(define-key global-map "\C-cc" 'org-capture)
(global-set-key (kbd "C-c o") 
                (lambda () (interactive) (find-file (concat org-directory "/school.org"))))
(global-set-key (kbd "C-c p") 
                (lambda () (interactive) (find-file "~/Google Drive/OHS/10th Grade/Semester 1/")))
(global-set-key (kbd "C-c i") 
                (lambda () (interactive) (find-file (concat org-directory "/todo.org"))))
(global-set-key (kbd "C-c v") 
                (lambda () (interactive) (find-file (concat org-directory "/violin.org"))))
(global-set-key (kbd "C-c m") 
                (lambda () (interactive) (find-file (concat org-directory "/notes/emacs/emacs_notes.org"))))
(global-set-key (kbd "C-c k") 
                (lambda () (interactive) (find-file (concat org-directory "/links.org"))))
(setq org-agenda-overriding-columns-format "%28ITEM %TODO %SCHEDULED %DEADLINE %TAGS")

;;; Quick Capture
(setq org-capture-templates
 '(("t" "Todo" entry (file+headline "~/Dropbox/org/todo.org" "Tasks")
        "* TODO %?")
   ("L" "Link" entry (file "~/Dropbox/org/links.org")
    "* TOREAD %?[[%:link][%:description]] %U\n" :prepend t)))

;;; org-drill
(require 'org-drill)

;;; Agenda key (C-c a) and other settings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

;;; org-format-latex-options
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.4))

(provide 'init-org)
;; init-org.el ends here
