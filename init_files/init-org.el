;;; init-org.el --- Set up Org Mode
;;; Commentary:

;; Basic Org Mode configuration, assuming presence of Evil & Evil Leader.

(require 'org)
(require 'org-agenda)
(setq org-directory "~/Dropbox/org/")
(setq org-agenda-files (list "~/Dropbox/org/school.org"
			     "~/Dropbox/org/gtd.org"
			     "~/Dropbox/org/violin.org"
			     "~/Dropbox/org/inbox.org"
			     "~/Dropbox/org/tickler.org"))
(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
(setq org-default-notes-file (concat org-directory "/inbox.org"))
(define-key global-map "\C-cc" 'org-capture)
(global-set-key (kbd "C-c o") 
                (lambda () (interactive) (find-file (concat org-directory "/school.org"))))
(global-set-key (kbd "C-c p") 
                (lambda () (interactive) (dired "~/Google Drive/OHS/10th Grade/Semester 2/")))
(global-set-key (kbd "C-c i") 
                (lambda () (interactive) (find-file (concat org-directory "/gtd.org"))))
(global-set-key (kbd "C-c v") 
                (lambda () (interactive) (find-file (concat org-directory "/violin.org"))))
(global-set-key (kbd "C-c m") 
                (lambda () (interactive) (find-file (concat org-directory "/notes.org"))))
(global-set-key (kbd "C-c k") 
                (lambda () (interactive) (find-file (concat org-directory "/links.org"))))
;(setq org-agenda-overriding-columns-format "%28ITEM %TODO %SCHEDULED %DEADLINE %TAGS")

(setq org-tag-persistent-alist '((:startgroup . nil)
				 ("@school" . ?s)
				 ("@home" . ?h)
				 ("@music" . ?m)
				 (:endgroup . nil)))

;; Org Refile
(setq pgwang/refile-targets (file-expand-wildcards "~/Dropbox/org/*.org"))
(setq org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9)
			   (pgwang/refile-targets :maxlevel . 9)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Agenda

;; org-agenda-auto-exclude-function
(defun org-my-auto-exclude-function (tag)
  (string= tag "officehours")
  (concat "-" tag))
(setq org-agenda-auto-exclude-function 'org-my-auto-exclude-function)

(defun org-agenda-redo-tags ()
  "Custom redo then finalize function"
  (interactive)
  ;(setq org-agenda-tags-column (string-to-number (concat "-" (number-to-string ((lambda () (interactive) (window-width)))))))
  (setq org-agenda-tags-column (* -1 ((lambda () (interactive) (window-width)))))
  (org-agenda-redo)
  (org-agenda-align-tags)
  (setq org-agenda-tags-column (eval (car (get 'org-agenda-tags-column 'standard-value))))
  )

;(add-hook 'org-agenda-mode-hook
;	  (lambda ()
;	    ;(local-set-key (kbd "H-o") [?g ?/ ?- ?o])
;	    (local-set-key (kbd "r") 'org-agenda-redo-tags)
;	    ))

;; Re-align tags when window shape changes
(add-hook 'org-agenda-mode-hook
          (lambda () (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t)))

;(add-hook 'org-agenda-finalize-hook
;	  'org-agenda-align-tags)

(setq org-deadline-warning-days 7)

(add-hook 'org-agenda-finalize-hook
	  (lambda ()
	    (display-line-numbers-mode 0)
	    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quick capture
(setq org-capture-templates
      '(
("i" "Inbox" entry (file "~/Dropbox/org/inbox.org")
"* TODO %?")
("L" "Link" entry (file "~/Dropbox/org/links.org")
"* TOREAD %?[[%:link][%:description]] %U
" :prepend t)
("b" "Bookmark" entry (file+headline "~/Dropbox/org/notes.org" "Bookmarks")
"* [[%?%:link][%:description]]
:PROPERTIES:
:CREATED: %U
:END:

" :empty-lines 1)
("m" "Manual" entry (file "~/Dropbox/org/notes.org")
"* %?

:PROPERTIES:
:CREATED: %U
:END:" :empty-lines 1)
("j" "Journal" entry
(file+olp+datetree "~/Dropbox/org/orgjournal.org.gpg")
"* %?
:PROPERTIES:
:LOGGED: %U
:END:")
("S" "School Entries")
("Sc" "Chinese Assignment" entry
(file+olp "~/Dropbox/org/school.org" "Class Todos" "_\\ *\\ OCH12 \\* \\_")
"* TODO [#%^{Priority|C|A|B|D}] %^{Assignment Name}
    DEADLINE: %^T
:PROPERTIES:
:LINK: %^{Link}
:END:")
("Sb" "Biology Assignment" entry
(file+olp "~/Dropbox/org/school.org" "Class Todos" "_\\ *\\ OB010 \\* \\_")
"* TODO [#%^{Priority|C|A|B|D}] %^{Assignment Name}
    DEADLINE: %^T
:PROPERTIES:
:LINK: %^{Link}
:END:")
("Se" "MWA Assignment" entry
(file+olp "~/Dropbox/org/school.org" "Class Todos" "_\\ *\\ OE011 \\* \\_")
"* TODO [#%^{Priority|C|A|B|D}] %^{Assignment Name}
    DEADLINE: %^T
:PROPERTIES:
:LINK: %^{Link}
:END:")
("Sm" "Calc BC Assignment" entry
(file+olp "~/Dropbox/org/school.org" "Class Todos" "_\\ *\\ OM4BC \\* \\_")
"* TODO [#%^{Priority|C|A|B|D}] %^{Assignment Name}
    DEADLINE: %^T
:PROPERTIES:
:LINK: %^{Link}
:END:")
("Sp" "Philosophy Assignment" entry
(file+olp "~/Dropbox/org/school.org" "Class Todos" "_\\ *\\ OHSC0 \\* \\_")
"* TODO [#%^{Priority|C|A|B|D}] %^{Assignment Name}
    DEADLINE: %^T
:PROPERTIES:
:LINK: %^{Link}
:END:")
("St" "PE Assignment" entry
(file+olp "~/Dropbox/org/school.org" "Class Todos" "_\\ *\\ OHSPE \\* \\_")
"* TODO [#%^{Priority|C|A|B|D}] %^{Assignment Name}
    DEADLINE: %^T
:PROPERTIES:
:LINK: %^{Link}
:END:")
))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MobileOrg

;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/Dropbox/Apps/MobileOrg/index.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

;; Org entries
(setq org-agenda-max-entries nil)

(provide 'init-org)
;; init-org.el ends here
