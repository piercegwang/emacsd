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
			     "~/Dropbox/org/tickler.org"
                             "~/Dropbox/org/gcal.org"
                             "~/Dropbox/org/events.org"))
(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
(setq org-default-notes-file (concat org-directory "/inbox.org"))
(define-key global-map "\C-cc" 'org-capture)
(global-set-key (kbd "H-c o") 
                (lambda () (interactive) (find-file (concat org-directory "/school.org"))))
(global-set-key (kbd "H-c p") 
                (lambda () (interactive) (dired "~/Google Drive/OHS/11th Grade/Semester 1/")))
(global-set-key (kbd "H-c i") 
                (lambda () (interactive) (find-file (concat org-directory "/gtd.org"))))
(global-set-key (kbd "H-c v") 
                (lambda () (interactive) (find-file (concat org-directory "/violin.org"))))
(global-set-key (kbd "H-c m") 
                (lambda () (interactive) (find-file (concat org-directory "/notes.org"))))
(global-set-key (kbd "H-c k") 
                (lambda () (interactive) (find-file (concat org-directory "/links.org"))))
;(setq org-agenda-overriding-columns-format "%28ITEM %TODO %SCHEDULED %DEADLINE %TAGS")

(setq org-log-done 'time) ; Log when task marked as done


(setq org-tag-persistent-alist '(("tag" . ?t)))

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
(defun pgwang/org-my-auto-exclude-function (tag)
  (if
      (string= tag "officehours")
      (concat "-" tag)))
(setq org-agenda-auto-exclude-function 'pgwang/org-my-auto-exclude-function)

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

(add-hook 'org-mode-hook
          (lambda ()
            (org-cdlatex-mode)
            ))

(setq org-deadline-warning-days 7)

(add-hook 'org-agenda-finalize-hook
	  (lambda ()
	    (linum-mode -1)
	    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quick capture
(defun pgwang/year-month ()
  "Custom function to return date in format: YYYY-MM"
  (format-time-string "%Y-%m"))

(defun pgwang/U ()
  "Custom function to return date in org inactive timestamp format"
  (format-time-string "[%Y-%m-%d %a]"))

(defun pgwang/add-12 ()
  "Custom function return active org timestamp with exactly 24 hour difference"
  (format-time-string "%Y-%m-%d %a %H:%M" (time-add (current-time) 85500)))

(setq org-capture-templates
      '(
("i" "Inbox" entry (file "~/Dropbox/org/inbox.org")
"* TODO %?")
("e" "Event" entry (file "~/Dropbox/org/events.org")
"* %?")
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
:JOY: %^{Rate enjoyment [1-10]}
:END:" :kill-buffer t)
("S" "School")
("Sx" "OHSPE Log" table-line
 (file+function "~/Dropbox/org/notes/OHS/PELog/pelog.org" pgwang/year-month)
 "|%^{Exercise}|%^{Duration of Exercise (HH:MM)}|%U|" :table-line-pos "II-1")
("F" "Fun")
("FR" "RL Create Date" entry
 (file+olp "~/Dropbox/org/notes/nodeka/fun_notes.org" "Rocket League" "Time Logging")
 "*** %u
**** Training
***** Free Play
**** Matches
***** Ranked
|            | W | L |
|------------+---+---|
| Solo Duels | 0 | 0 |
| Duos       | 0 | 0 |
| Standard   | 0 | 0 |
***** Unranked
|            | W | L |
|------------+---+---|
| Solo Duels | 0 | 0 |
| Duos       | 0 | 0 |
| Standard   | 0 | 0 |
| Chaos      | 0 | 0 |
| Rumble     | 0 | 0 |
| Dropshot   | 0 | 0 |
| Other      | 0 | 0 |
" :immediate-finish t)
("Fv" "Voting Log" entry
 (file+olp "~/Dropbox/org/fun.org" "MC Voting" "Casual Craft")
 "* Vote
SCHEDULED: <%(pgwang/add-12)>
%U" :immediate-finish t)
))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MobileOrg

;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/Dropbox/Apps/MobileOrg/index.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

;; Org entries
(setq org-agenda-max-entries nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Crypt
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))

(setq org-crypt-key nil)
  ;; GPG key to use for encryption
  ;; Either the Key ID or set to nil to use symmetric encryption.

(setq auto-save-default nil)
  ;; Auto-saving does not cooperate with org-crypt.el: so you need
  ;; to turn it off if you plan to use org-crypt.el quite often.
  ;; Otherwise, you'll get an (annoying) message each time you
  ;; start Org.

  ;; To turn it off only locally, you can insert this:
  ;;
  ;; # -*- buffer-auto-save-file-name: nil; -*-

(provide 'init-org)
;; init-org.el ends here
