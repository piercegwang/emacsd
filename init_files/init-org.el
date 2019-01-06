;;; init-org.el --- Set up Org Mode
;;; Commentary:

;; Basic Org Mode configuration, assuming presence of Evil & Evil Leader.

(require 'org)
(require 'org-agenda)
(setq org-directory "~/Dropbox/org/")
(setq org-agenda-files (list "~/Dropbox/org/school.org"
			     "~/Dropbox/org/todo.org"
			     "~/Dropbox/org/violin.org"
			     "~/Dropbox/org/inbox.org"))
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "DONE")))
'((sequence "TODO" "IN-PROGRESS" "DONE"))
(setq org-default-notes-file (concat org-directory "/todo.org"))
(define-key global-map "\C-cc" 'org-capture)
(global-set-key (kbd "C-c o") 
                (lambda () (interactive) (find-file (concat org-directory "/school.org"))))
(global-set-key (kbd "C-c p") 
                (lambda () (interactive) (dired "~/Google Drive/OHS/10th Grade/Semester 1/")))
(global-set-key (kbd "C-c i") 
                (lambda () (interactive) (find-file (concat org-directory "/todo.org"))))
(global-set-key (kbd "C-c v") 
                (lambda () (interactive) (find-file (concat org-directory "/violin.org"))))
(global-set-key (kbd "C-c m") 
                (lambda () (interactive) (find-file (concat org-directory "/notes.org"))))
(global-set-key (kbd "C-c k") 
                (lambda () (interactive) (find-file (concat org-directory "/links.org"))))
(setq org-agenda-overriding-columns-format "%28ITEM %TODO %SCHEDULED %DEADLINE %TAGS")

;;; Quick Capture
(setq org-capture-templates
 '(("t" "Todo" entry (file+headline "~/Dropbox/org/todo.org" "General")
        "* TODO %?")
   ("L" "Link" entry (file "~/Dropbox/org/links.org")
    "* TOREAD %?[[%:link][%:description]] %U\n" :prepend t)
   ("b" "Bookmark" entry (file+headline "~/Dropbox/org/notes.org" "Bookmarks")
    "* [[%?%:link][%:description]]\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
   ("j" "Journal Entries")
   ("jj" "Standard Journal" entry
    (file+olp+datetree "~/Dropbox/org/orgjournal.org.gpg")
    "* %?\n\n:PROPERTIES:\n:LOGGED: %U\n:END:\n\n")
   ))

;;; org-drill
(require 'org-drill)

;;; Agenda key (C-c a) and other settings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

;;; org-format-latex-options
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.4))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

;; org-agenda-auto-exclude-function
(defun org-my-auto-exclude-function (tag)
  (string= tag "officehours")
  (concat "-" tag))
(setq org-agenda-auto-exclude-function 'org-my-auto-exclude-function)

(defun org-agenda-redo-tags ()
  "Custom redo then finalize function"
  (interactive)
  (list
   (org-agenda-redo)
   (org-agenda-align-tags)))

(add-hook 'org-agenda-mode-hook
	  (lambda ()
	    (local-set-key (kbd "H-o") [?g ?/ ?- ?o])
	    (local-set-key (kbd "H-r") 'org-agenda-redo-tags)
	    (local-set-key (kbd "r") (lambda () (interactive) (org-agenda-align-tags))))
	    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MobileOrg

;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/Dropbox/Apps/MobileOrg/index.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

(provide 'init-org)
;; init-org.el ends here
