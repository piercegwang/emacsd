;;; -*- lexical-binding: t -*-

(defun tangle-init ()
  "If the current buffer is 'init.org' the code-blocks are
tangled, and the tangled file is compiled."
  (when (equal (buffer-file-name)
               (expand-file-name (concat user-emacs-directory "init.org")))
    ;; Avoid running hooks when tangling.
    (let ((prog-mode-hook nil))
      (org-babel-tangle)
      (byte-compile-file (concat user-emacs-directory "init.el")))))

(add-hook 'after-save-hook 'tangle-init)

(eval-when-compile
  (defvar use-package-expand-minimally byte-compile-current-file))

(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

(defvar better-gc-cons-threshold 16777216 ; 16mb
  "The default value to use for `gc-cons-threshold'.
If you experience freezing, decrease this. If you experience stuttering, increase this.")


(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold better-gc-cons-threshold
          gc-cons-percentage 0.1))) ; Default value for `gc-cons-percentage'

(defun pgw/defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun pgw/restore-garbage-collection-h ()
  "Defer it so that commands launched immediately after will enjoy the benefits."
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold better-gc-cons-threshold))))

(add-hook 'minibuffer-setup-hook #'pgw/defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'pgw/restore-garbage-collection-h)

(require 'package)
(setq package-archives
    '(("melpa" . "https://melpa.org/packages/")
      ("gnu" . "https://elpa.gnu.org/packages/")
      ("org" . "https://orgmode.org/elpa/")
      ))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/custom_load/")

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

(setq use-package-always-ensure t)

(eval-when-compile
  (require 'use-package))
(require 'bind-key)                ;; if you use any :bind variant

(setq byte-compile-warnings '(cl-functions))
(require 'cl-lib)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "Emacs ready in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(setq confirm-kill-emacs 'yes-or-no-p)

(defun append-to-list (list-var elements)
  "Append ELEMENTS to the end of LIST-VAR.

The return value is the new value of LIST-VAR."
  (unless (consp elements)
    (error "ELEMENTS must be a list"))
  (let ((list (symbol-value list-var)))
    (if list
        (setcdr (last list) elements)
      (set list-var elements)))
  (symbol-value list-var))

;;; Increment Numbers
(defun increment-number-at-point ()
  "Increments numbers at cursor"
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

;;; Decrement Numbers
(defun decrement-number-at-point ()
  "Decrements numbers at cursor"
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1- (string-to-number (match-string 0))))))

;;; Binding
(global-set-key (kbd "C-; C-=") 'increment-number-at-point)
(global-set-key (kbd "C-; C--") 'decrement-number-at-point)

(defun insertdirectory ()
  "Insert current directory for macro use"
  (interactive)
  (insert default-directory))

(defun ignore-error-wrapper (fn)
  "Funtion return new function that ignore errors.
     The function wraps a function with `ignore-errors' macro."
  (lexical-let ((fn fn))
    (lambda ()
      (interactive)
      (ignore-errors
        (funcall fn)))))

(defun insert-org-image (&optional swindow)
  "Prompt user for name of file, append time and date string, then use the Mac OSX `screencapture` feature to take a photo and place it in the relative ./figures directory."
  (interactive "P")
  (unless (not (eq system-type 'darwin))
    (let* ((outdir (concat (file-name-directory (buffer-file-name)) "/figures"))
           (namefile (concat (read-string "Enter File Name: ") "_" (format-time-string "%Y%m%d_%H%M%S.jpeg"))))
      (if (char-equal (aref namefile 0) ?_)
          (setq namefile (substring namefile 1 (length namefile))))
      (let* ((outfile (expand-file-name namefile outdir)))
        (unless (file-directory-p outdir)
          (make-directory outdir t))
        (message "Argument: %s" swindow)
        (if swindow
            (call-process "screencapture" nil nil nil "-w" outfile)
          (call-process "screencapture" nil nil nil "-i" outfile))
        (message namefile)
        (insert (concat (concat "[[file:./figures/" (file-name-nondirectory outfile)) "]]"))))
    )
  )

(set-keyboard-coding-system nil)

(defun pgw/dired-open-file ()
  "In dired, open the file named on this line using the default application in the system."
  (interactive)
  (let ((file (dired-get-filename nil t)) ; Full path
        (filename (dired-get-filename t t))) ; File name for display
    (message "Opening %s..." filename)
    (call-process "open" nil 0 nil file)
    (message "Opening %s done" filename)))

(defun pgw/copy-mla-file ()
  "Copy MLA_OrgFile.org to current directory for use in school essays."
  (interactive)
  (copy-file "~/Dropbox/org/templates/school/MLA_OrgFile.org" default-directory)
  )

(when (eq system-type 'darwin)
  (defun pgw/lookup-dictionary ()
    "Function to open a dictionary searching the highlighted word
No spaces are allowed in the input of this function"
    (interactive)
    (let ((word (read-from-minibuffer "Word query: ")))
      (call-process "open" nil nil nil (concat "dict://" word)))
    )
  (global-set-key (kbd "M-#") 'pgw/lookup-dictionary)
  )

(defun internet-up-p (&optional host)
  (= 0 (call-process "ping" nil nil nil "-c" "1" "-W" "1"
                     (if host host "1.1.1.1"))))

(defun pgw/org-get-link-at-point ()
  "Get the link from an org heading"
  (interactive)
  (let* ((context (org-element-context))
         (link (if (eq (car context) 'link)
                   (org-element-property :path context)
                 nil)))
    (if link (kill-new (concat (org-element-property :type context) ":" link)))))

(global-set-key (kbd "C-c s-l") 'pgw/org-get-link-at-point)

(defun make-shell (name)
  "Create a shell buffer named NAME."
  (interactive "sName: ")
  (setq name (concat "$" name))
  (eshell 4)
  (rename-buffer name))

(when (eq system-type 'darwin)
  (with-no-warnings
    (setq mac-option-modifier 'meta)
    (setq mac-control-modifier 'control)
    (setq ns-function-modifier 'hyper)))

(when (eq system-type 'gnu/linux)
  (with-no-warnings (setq x-super-keysym 'hyper)))

(use-package exec-path-from-shell)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;; Change shell process (from bash to zsh)
(setq shell-file-name "/bin/zsh")

;(load-theme 'tango-dark t)
;;; Frame
(add-to-list 'default-frame-alist '(height . 46))
(add-to-list 'default-frame-alist '(width . 146))

;;; Visual line mode (for text wrapping)
(global-set-key (kbd "C-x v v") 'visual-line-mode)

;(global-visual-line-mode t)
(global-linum-mode 0)
(global-display-line-numbers-mode 1)
(setq-default display-line-numbers 'visual)
(setq display-line-numbers-type 'visual)
(set-default 'truncate-lines t)

;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; assuming you are using a dark theme
;; (setq ns-use-proxy-icon nil)
;; (setq frame-title-format nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq visual-line-fringe-indicators '(left-curly-arrow hollow-square)) ;; '(left-curly-arrow right-curly-arrow) for both left and right
;; Testing freetonik's fringe indicator alist
(setq-default fringe-indicator-alist '((truncation left-arrow right-arrow)
 (continuation nil right-arrow)
 (overlay-arrow . right-triangle)
 (up . up-arrow)
 (down . down-arrow)
 (top top-left-angle top-right-angle)
 (bottom bottom-left-angle bottom-right-angle top-right-angle top-left-angle)
 (top-bottom left-bracket right-bracket top-right-angle top-left-angle)
 (empty-line . empty-line)
 (unknown . question-mark)))

(use-package all-the-icons)

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
  ;; may have their own settings.
  (load-theme 'doom-outrun-electric t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Doom themes fontifies #hashtags and @at-tags by default.
  ;; To disable this:
  (setq doom-org-special-tags nil)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  )

(setq frame-resize-pixelwise t)

(when (fboundp 'windmove-default-keybindings)
  (global-set-key (kbd "H-h") (ignore-error-wrapper 'windmove-left))
  (global-set-key (kbd "H-l") (ignore-error-wrapper 'windmove-right))
  (global-set-key (kbd "H-k") (ignore-error-wrapper 'windmove-up))
  (global-set-key (kbd "H-j") (ignore-error-wrapper 'windmove-down))
  )

;; Disabled, Doesn't really work for me - going to use s-left and s-right instead
;; (use-package framemove
;;   :load-path "custom_load"
;;   :config
;;   (require 'framemove)
;;   (global-set-key (kbd "C-e-<down>")  'fm-down-frame)
;;   (global-set-key (kbd "C-s-<up>")    'fm-up-frame)
;;   (global-set-key (kbd "C-s-<left>")  'fm-left-frame)
;;   (global-set-key (kbd "C-s-<right>") 'fm-right-frame)
;;   (setq framemove-hook-into-windmove t)
;;   )

(use-package rotate
  :load-path "site-lisp/emacs-rotate")

(use-package treemacs)
(use-package treemacs-evil)
(use-package treemacs-magit)

(use-package ztree)

(setq pgw/default-font "Menlo")
(setq pgw/default-font-size 12)
(setq pgw/current-font-size pgw/default-font-size)

(setq pgw/font-change-increment 1.1)

(defun pgw/font-code ()
  "Return a string representing the current font (like \"Inconsolata-14\")."
  (concat pgw/default-font "-" (number-to-string pgw/current-font-size)))

(defun pgw/set-font-size ()
  "Set the font to `pgw/default-font' at `pgw/current-font-size'.
Set that for the current frame, and also make it the default for
other, future frames."
  (interactive)
  (let ((font-code (pgw/font-code)))
    (if (assoc 'font default-frame-alist)
        (setcdr (assoc 'font default-frame-alist) font-code)
      (add-to-list 'default-frame-alist (cons 'font font-code)))
    (set-frame-font font-code)))

(defun pgw/reset-font-size ()
  "Change font size back to `pgw/default-font-size'."
  (interactive)
  (setq pgw/current-font-size pgw/default-font-size)
  (pgw/set-font-size))

(defun pgw/increase-font-size ()
  "Increase current font size by a factor of `pgw/font-change-increment'."
  (interactive)
  (setq pgw/current-font-size
        (ceiling (* pgw/current-font-size pgw/font-change-increment)))
  (pgw/set-font-size))

(defun pgw/decrease-font-size ()
  "Decrease current font size by a factor of `pgw/font-change-increment', down to a minimum size of 1."
  (interactive)
  (setq pgw/current-font-size
        (max 1
             (floor (/ pgw/current-font-size pgw/font-change-increment))))
  (pgw/set-font-size))

(define-key global-map (kbd "C-)") 'pgw/reset-font-size)
(define-key global-map (kbd "C-H-0") 'pgw/set-font-size)
(define-key global-map (kbd "C-+") 'pgw/increase-font-size)
(define-key global-map (kbd "C-=") 'pgw/increase-font-size)
(define-key global-map (kbd "C-_") 'pgw/decrease-font-size)
(define-key global-map (kbd "C--") 'pgw/decrease-font-size)

(add-hook 'emacs-startup-hook
          (lambda () (interactive) (pgw/reset-font-size)))

(set-face-attribute 'variable-pitch nil :family "Avenir Next")

(use-package mixed-pitch
  :load-path "custom_load"
  :config
  ;; (set-face-attribute 'variable-pitch :height 160)
  (dolist (face '(line-number line-number-current-line org-list-dt org-link)) (add-to-list 'mixed-pitch-fixed-pitch-faces face))
  ;; (add-hook 'text-mode-hook 'mixed-pitch-mode)
  (global-set-key (kbd "C-x v f") 'mixed-pitch-mode)
  )

;; (require 'epa-file)
(epa-file-enable)
(setf epa-pinentry-mode 'loopback)

(load-file "~/.passwords.el")

(use-package powerline
  :ensure t
  :config (powerline-default-theme))

(size-indication-mode 1)
(line-number-mode -1)

(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  ;; enable this if you want `swiper' to use it
  ;; (setq search-default-mode #'char-fold-to-regexp)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

;(setq org-agenda-include-diary t)
(setq diary-file "~/Dropbox/org/diary")

(appt-activate -1)
(setq appt-message-warning-time 15)
(setq diary-comment-start "##")

(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file (concat org-directory "/inbox.org"))

(setq org-startup-indented t)

(setq org-todo-keywords
      '((sequence "NEXT(n)" "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "DISABLED(c)" "DELEGATED(g)")))

(define-key global-map "\C-cc" 'org-capture)
  (global-set-key (kbd "H-c o") 
                  (lambda () (interactive) (find-file (concat org-directory "/school.org"))))
  (global-set-key (kbd "H-c p") 
                  (lambda () (interactive) (dired "~/Google Drive/OHS/11th Grade/Semester 2/")))
  (global-set-key (kbd "H-c i") 
                  (lambda () (interactive) (find-file (concat org-directory "/projects.org"))))
  (global-set-key (kbd "H-c v") 
                  (lambda () (interactive) (find-file (concat org-directory "/violin.org"))))
  (global-set-key (kbd "H-c n") 
                  (lambda () (interactive) (find-file (concat org-directory "/notes.org"))))
  (global-set-key (kbd "H-c m") 
                  (lambda () (interactive) (find-file (concat org-directory "/music.org"))))
  (global-set-key (kbd "H-c k") 
                  (lambda () (interactive) (find-file (concat org-directory "/links.org"))))

  ;;; Agenda key (C-c a) and other settings
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-cb" 'org-switchb)

  (evil-define-key 'normal org-mode-map "<<" 'org-promote-subtree)
  (evil-define-key 'normal org-mode-map ">>" 'org-demote-subtree)

(setq org-tag-persistent-alist '(("email" . ?e)
                                 ("noexport" . ?N)))

(setq org-log-done 'time) ; Log when task marked as done

(setq pgw/refile-targets (file-expand-wildcards "~/Dropbox/org/*.org"))
(setq org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9)
                           (pgw/refile-targets :maxlevel . 9)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; org-agenda-auto-exclude-function
(defun pgw/org-my-auto-exclude-function (tag)
  (if
      (string= tag "officehours")
      (concat "-" tag)))
(setq org-agenda-auto-exclude-function 'pgw/org-my-auto-exclude-function)

;(setq org-agenda-overriding-columns-format "%28ITEM %TODO %SCHEDULED %DEADLINE %TAGS")

;; Re-align tags when window shape changes
(add-hook 'org-agenda-mode-hook
          (lambda () (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t)))

;(add-hook 'org-agenda-finalize-hook
;	  'org-agenda-align-tags)

(setq org-deadline-warning-days 7)

(add-hook 'org-agenda-finalize-hook
          (lambda ()
            (display-line-numbers-mode -1)
            ))

;; Org entries
(setq org-agenda-max-entries nil)

(setq org-agenda-custom-commands
      '(("o" . "OHS")
        ("of" . "OHS Friend Schedules")
        ("ofa" "Audrey's Schedule" agenda ""
         ((org-agenda-span 7)
          (org-agenda-files
           (file-expand-wildcards "~/Dropbox/org/notes/OHS/202021/audrey_classes.org"))))
        ("ofj" "Josie's Schedule" agenda ""
         ((org-agenda-span 7)
          (org-agenda-files
           (file-expand-wildcards "~/Dropbox/org/notes/OHS/202021/josie_classes.org"))))
        ("ofA" "Ariana Schedule" agenda ""
         ((org-agenda-span 7)
          (org-agenda-files
           (file-expand-wildcards "~/Dropbox/org/notes/OHS/202021/ariana_classes.org"))))
        ("l" "Logging View" agenda ""
         ((org-agenda-span 1)
          (org-agenda-files
           (file-expand-wildcards "~/Dropbox/org/*.org"))))
        ("A" "General Agenda" agenda ""
         ((org-agenda-span 1)
          (org-agenda-sorting-strategy
           '((agenda habit-down time-up deadline-up)))))
        ("D" "College Deadlines" tags-todo "+collegeapps")
        ("Q" . "Custom queries")
        ("Qa" "Query all (Archive included)" search ""
         ((org-agenda-files (append (file-expand-wildcards (concat org-directory "/*.org"))
                                    (file-expand-wildcards (concat org-directory "/*.org_archive"))))))
        ("Ql" "Query Links" search ""
         ((org-agenda-files (list (concat org-directory "/links.org")
                                  (concat org-directory "/links.org_archive")))))))

(setq org-agenda-files (append (file-expand-wildcards "~/Dropbox/org/*.org")
                               (file-expand-wildcards "~/Dropbox/org/calendars/*.org")))

(setq org-agenda-time-grid '((daily today require-timed)
                             (600 800 1000 1200 1400 1600 1800 2000 2200)
                             "......" "----------------"))

(defun pgw/year-month ()
  "Custom function to return date in format: YYYY-MM"
  (format-time-string "%Y-%m"))

(defun pgw/U ()
  "Custom function to return date in org inactive timestamp format"
  (format-time-string "[%Y-%m-%d %a]"))

(defun pgw/add-12 ()
  "Custom function return active org timestamp with exactly 24 hour difference"
  (format-time-string "%Y-%m-%d %a %H:%M" (time-add (current-time) 85500)))

(defun pgw/headline_date ()
  "Function to find the date as headline for Violin capture template"
  (goto-char (point-min))
  (let ((searchresults (search-forward (format-time-string "[%Y-%m-%d %a]") nil t)))
    (if searchresults
        'searchresults
      (error "Not found! Use Vc to create today's practice first.")
      )
    )
  )

(setq org-capture-templates
      '(
("i" "Inbox")
("ii" "Inbox Entry" entry (file "~/Dropbox/org/inbox.org")
"* NEXT %?
")
("il" "Inbox w/ Link" entry (file "~/Dropbox/org/inbox.org")
"* NEXT %?
%a
")
("in" "Quick Note" entry (file "~/Dropbox/org/inbox.org")
"* %?
%U")
("e" "Events")
("ef" "Emacs Event (Not synced)" entry (file "~/Dropbox/org/events.org")
 "* %?")
("ee" "Emacs Calendar (Limbo)" entry (file "~/Dropbox/org/events.org")
 "* %^{Title of event}
SCHEDULED: %^{Scheduled time + duration}T
:PROPERTIES:
:calendar-id: ihfv2u5n9uf5ksj5484vbe7mj4@group.calendar.google.com
:END:
:org-gcal:%?
:END:" :jump-to-captured t)
("ep" "Gmail Calendar (Limbo)" entry (file "~/Dropbox/org/events.org")
 "* %^{Title of event}
SCHEDULED: %^{Scheduled time + duration}T
:PROPERTIES:
:calendar-id: pierce.g.wang@gmail.com
:END:
:org-gcal:%?
:END:" :jump-to-captured t)
("es" "SFCM Calendar (Limbo)" entry (file "~/Dropbox/org/sfcm.org")
 "* %^{Title of event}
SCHEDULED: %^{Scheduled time + duration}T
:PROPERTIES:
:calendar-id: taiu2dsr8o29c09m7nn1n21t9o@group.calendar.google.com
:END:
:org-gcal:%?
:END:" :jump-to-captured t)
("L" "Link" entry (file+headline "~/Dropbox/org/links.org" "!Inbox")
"* [[%?%:link][%:description]]
:PROPERTIES:
:CREATED:  %U
:END:" :prepend t)
("g" "Manual" entry (file "~/Dropbox/org/notes.org")
"* %?
:PROPERTIES:
:CREATED:  %U
:END:" :empty-lines 1)
("b" "Book" entry (file+headline "~/Dropbox/org/notes.org" "Books")
 "* %^{RATING}p%^{Book Title}")
("j" "Journal" entry
(file+olp+datetree "~/Dropbox/org/orgjournal.org.gpg")
"* %?
:PROPERTIES:
:LOGGED: %^{Logged Time}U
:END:
" :kill-buffer t)
("m" "Mail")
("mt" "Mail task" entry
 (file "~/Dropbox/org/inbox.org")
 "* NEXT %? :email:
:PROPERTIES:
:EMAIL: %a
:SENDER: %:from
:END:\n")
("mn" "Mail note" entry
 (file "~/Dropbox/org/inbox.org")
 "* %? :email:
:PROPERTIES:
:EMAIL: %a
:SENDER: %:from
:END:\n")
("S" "School")
("St" "School Tasks")
("Stj" "OCS15 - T/Th" entry
 (file+headline "~/Dropbox/org/school.org" "_OCS15_")
 "* NEXT %?
DEADLINE: %^{Deadline}T\n")
("Stm" "UM51A - T/Th" entry
 (file+headline "~/Dropbox/org/school.org" "_UM51A_")
 "**** NEXT %?
DEADLINE: %^{Deadline}T\n")
("Sts" "OPS10 - M/W" entry
 (file+headline "~/Dropbox/org/school.org" "_OPS10_")
 "* NEXT %?
DEADLINE: %^{Deadline}T\n")
("Stp" "OP051 - M/W" entry
 (file+headline "~/Dropbox/org/school.org" "_OP051_")
 "* NEXT %?
DEADLINE: %^{Deadline}T\n")
("Stc" "OCRA1 - M/W" entry
 (file+headline "~/Dropbox/org/school.org" "_OCRA1_")
 "* NEXT %?
DEADLINE: %^{Deadline}T\n")
("Sr" "School Readings")
("Srj" "OCS15 - T/Th" entry
 (file+headline "~/Dropbox/org/school.org" "_OCS15_")
 "* NEXT %?
DEADLINE: <%<%Y-%m-%d %a 07:15>>\n")
("Srm" "UM51A - T/Th" entry
 (file+headline "~/Dropbox/org/school.org" "_UM51A_")
 "**** NEXT %?
DEADLINE: <%<%Y-%m-%d %a 08:30>>\n")
("Srs" "OPS10 - M/W" entry
 (file+headline "~/Dropbox/org/school.org" "_OPS10_")
 "* NEXT %?
DEADLINE: <%<%Y-%m-%d %a 11:00>>\n")
("Srp" "OP051 - M/W" entry
 (file+headline "~/Dropbox/org/school.org" "_OP051_")
 "* NEXT %?
DEADLINE: <%<%Y-%m-%d %a 14:45>>\n")
("Src" "OCRA1 - M/W" entry
 (file+headline "~/Dropbox/org/school.org" "_OCRA1_")
 "* NEXT %?
DEADLINE: <%<%Y-%m-%d %a 09:45>>\n")
("M" "Music")
("Mc" "Conducting Homework" entry
 (file+headline "~/Dropbox/org/music.org" "Homework")
 "* TODO Conducting Homework
DEADLINE: %^t
- %?")
("V" "Violin")
("Vc" "Create Practice Entry" entry
 (file+olp "~/Dropbox/org/violin.org" "Practice Log")
 "* [%<%Y-%m-%d %a>]
%t%?")
("Vd" "Add practice details" item
 (file+function "~/Dropbox/org/violin.org" pgw/headline_date)
 "%?")
("f" "Fun")
("fc" "Cookie Clicker Save" entry
 (file+olp "~/Dropbox/org/notes.org" "Cookie Clicker Run [2020-09-14 Mon]")
 "* %U
%?
: %c
")
("q" "Quotes" entry
 (file+olp "~/Dropbox/org/notes.org" "Quotes")
 "* %?
:PROPERTIES:
:DATE:     %U
:END:
%^{FROM}p
")))

;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/Dropbox/Apps/MobileOrg/index.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

(use-package org-crypt
  :load-path "elpa/org-9.2.3"
  :config
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))

  (setq org-crypt-key "3C44F187958295E4")
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
  :bind (("C-c s-d e" . org-encrypt-entry)
         ("C-c s-d d" . org-decrypt-entry))
  )

(with-eval-after-load 'org
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((python . t)
                                 )))

;;; org-drill
(use-package org-drill
  :load-path "custom_load")

(require 'ox-latex)

(use-package cdlatex
  :after org
  :config
  (define-key org-cdlatex-mode-map (kbd "H-d") 'cdlatex-dollar)
  (define-key cdlatex-mode-map (kbd "H-d") 'cdlatex-dollar)
  (add-hook 'org-mode-hook #'org-cdlatex-mode)
  )

(setq org-format-latex-options
      ;; '(:foreground "#000000" :background default ;; light theme
      '(:foreground "#d6d6d4" :background default ;; dark theme
                    :scale 1.4
                    :html-foreground "Black" :html-background "Transparent"
                    :html-scale 1.0 
                    :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

(let ((dvipng--plist (alist-get 'dvipng org-preview-latex-process-alist)))
  (plist-put dvipng--plist :use-xcolor t)
  (plist-put dvipng--plist :image-converter '("dvipng -D %D -T tight -o %O %f")))

(use-package org-superstar
  :config
  (setq org-superstar-prettify-item-bullets 'nil)
  :hook (org-mode . org-superstar-mode))

(setq org-export-async-init-file "~/.emacs.d/orgasyncinit.el")

(require 'ox-publish)
(setq org-publish-project-alist
      '(("pages-notes"
         :base-directory "~/Dropbox/org_publish/"
         :base-extension "org"
         :publishing-directory "~/Documents/Projects/Github/github_pages/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4             ; Just the default for this project.
         ;; :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"css/style.css\"/>"
         :auto-preamble t
         )
        ("pages-static"
         :base-directory "~/Dropbox/org_publish/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|jpeg"
         :publishing-directory "~/Documents/Projects/Github/github_pages/"
         :recursive t
         :publishing-function org-publish-attachment
         )
        ("pages" :components ("pages-notes" "pages-static"))
        ))

(setq org-html-validation-link nil)

(use-package org-noter
  :after org
  :ensure t
  :config
  (setq org-noter-default-notes-file-names '("notes.org")
        org-noter-notes-search-path '("~/Dropbox/org/notes"))
  )

(use-package org-gcal
  :ensure t
  :demand t
  :bind (("C-c s-g p" . org-gcal-post-at-point)
         ("C-c s-g s" . org-gcal-sync)
         ("C-c s-g f" . org-gcal-fetch)
         ("C-c s-g d" . org-gcal-delete-at-point)
         ("C-c s-g b s" . org-gcal-sync-buffer)
         ("C-c s-g b f" . org-gcal-sync-buffer))
  :config
  (setq org-gcal-client-id "439150530674-aab9ti8n7t80r001qmccgb2i52005f18.apps.googleusercontent.com"
        org-gcal-client-secret "5gUN_ML-yaAgdS6eg4hAZ9qo"
        org-gcal-file-alist '(("pierce.g.wang@gmail.com" .  "~/Dropbox/org/calendars/cal_gmail.org")
                              ("ihfv2u5n9uf5ksj5484vbe7mj4@group.calendar.google.com" . "~/Dropbox/org/calendars/cal_emacs.org")
                              ("taiu2dsr8o29c09m7nn1n21t9o@group.calendar.google.com" . "~/Dropbox/org/calendars/cal_sfcm.org")))
  (setq org-gcal-notify-p nil)
  (setq org-gcal-remove-api-cancelled-events t))

(setq org-reveal-root "file:///Users/piercewang/Documents/projects/revealjs/reveal.js-4.1.0")

(setq TeX-engine 'xetex)
(setq latex-run-command "xetex")

(use-package tex
  :ensure auctex
  :defer t
  :config
  (setq TeX-auto-save t))

(unless (with-no-warnings (find "Times" org-latex-classes :key 'car
                                :test 'equal))
  (add-to-list 'org-latex-classes
               '("Times"
                 "\\documentclass[12pt]{article}
\\usepackage{fontspec}
\\setmainfont{Times New Roman}
\\usepackage{hyperref}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(use-package evil
  :config
  (evil-mode t)
  (dolist (mode '(dired-mode calendar-mode image-mode timer-list-mode messages-buffer-mode bufler-list-mode)) (add-to-list 'evil-emacs-state-modes mode)))

;; (define-key evil-normal-state-map (kbd "<S-return>") [?m ?` ?o escape ?` ?`])
;; (define-key evil-normal-state-map (kbd "<s-S-return>") [?m ?` ?O escape ?` ?`])
(define-key evil-motion-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "g k") 'evil-previous-line)
(define-key evil-motion-state-map (kbd "g j") 'evil-next-line)
(define-key evil-insert-state-map (kbd "C-a") 'beginning-of-visual-line)
(define-key evil-insert-state-map (kbd "C-e") 'end-of-visual-line)

(add-hook 'prog-mode-hook #'hs-minor-mode)
(electric-pair-mode)

(use-package python-docstring
  :load-path "site-lisp/python-docstring-mode")

(elpy-enable)
(add-hook 'elpy-mode-hook
          'python-docstring-mode)
(setq elpy-rpc-backend "jedi")

(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))
(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))

(use-package projectile)
(use-package flycheck)
(use-package yasnippet :config (yas-global-mode))
(use-package lsp-mode :hook ((lsp-mode . lsp-enable-which-key-integration))
  :config (setq lsp-completion-enable-additional-text-edit nil))
(use-package hydra)
(use-package company)
(use-package lsp-ui)
(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
(use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
(use-package dap-java :ensure nil)
(use-package lsp-ivy)
(use-package lsp-treemacs)

(fset 'setupworkspace
   [?\C-c ?a ?A ?. ?\C-x ?0 ?\C-x ?3 ?\H-l ?\H-\C-x ?o ?\C-x ?2 ?\C-u ?7 ?\C-x ?^ ?\H-j ?\H-c ?i ?\H-h ?\H-c ?o ?\H-l])
(global-set-key (kbd "C-x C-k 1") 'setupworkspace)

;;(fset 'OHSFigureSave
;;   [?# ?+ ?C ?A ?P ?T ?I ?O ?N ?: ?  ?\C-x ?Q return return tab ?\[ ?\[ ?f ?i ?l ?e ?: ?. ?/ ?W ?e ?e ?k ?  ?\C-x ?Q return ?/ ?\C-x ?Q return ?_ ?\C-u ?\M-! ?d ?a ?t ?e ?  ?+ ?% ?H ?% ?M ?% ?S return escape ?e ?a ?. ?p ?n ?g escape ?v ?B ?F ?/ ?l ?y escape ?A ?\] ?\] return escape ?p ?0 ?i ?\M-x ?i ?n ?s ?e ?r ?t ?d ?i ?r ?e ?c ?t ?o ?r ?y return escape ?V ?d ?i ?\C-x ?\C-f ?\C-  ?\C-a backspace ?/ ?U ?s ?e ?r ?s ?/ ?p ?i ?e ?r ?c ?e ?w ?a ?n ?g ?/ ?S ?c ?r ?e ?e ?n ?s ?h ?o ?t ?s return ?s ?\M-< ?\C-z ?/ ?S ?c ?r ?e ?e ?n ?  ?S ?h ?o ?t return ?R ?\C-  ?\C-a backspace ?\s-v backspace return ?\C-x ?k return])
;;(global-set-key (kbd "<f9>") 'OHSFigureSave)

;; (defun pgw/disable-helm ()
;;   "Disable Helm"
;;   (interactive)
;;   (helm-mode 0))
;; (defun pgw/enable-helm ()
;;   "Enable Helm"
;;   (interactive)
;;   (helm-mode))
;; (global-set-key (kbd "H-x H-h d") 'pgw/disable-helm)
;; (global-set-key (kbd "H-x H-h e") 'pgw/enable-helm)

;; ;(fset 'importChine
seFlashcards
;   [return ?\C-p ?* ?* ?  ?I ?t ?e ?m ?\C-c ?\C-c ?d ?r ?i ?l ?l return ?\C-n ?\C-a ?\C-z ?f ?= ?x ?x ?\C-z ?\C-k ?\C-n ?\C-a return return ?\C-p ?* ?* ?  ?A ?n ?s ?w ?e ?r ?\C-a ?* ?\C-n ?\C-a ?\C-y ?\; ?  ?\C-a ?\C-n ?\C-n])
(fset 'convertQuizlet
   [?I ?* ?* ?\S-  ?I ?t ?e ?m ?  ?: ?d ?r ?i ?l ?l ?: return escape ?/ ?= ?= return ?x ?x ?i return return ?* ?* ?* ?  ?A ?n ?s ?w ?e ?r return escape ?\M-\}])
(global-set-key (kbd "<f6>") 'convertQuizlet)

(fset 'addqtest1
   [?\C-s ?a ?d ?d ?q ?\( return ?\C-a ?\C-  ?\C-\M-f ?\C-\M-f ?\C-f ?\C-\M-$ ?\C-q ?\C-j ?\[ ?  ?\] ?* return return ?\C-e ?\C-r ?a ?d ?d ?q ?\( return ?\C-x ?r ?  ?a ?\C-  ?\M-f ?\C-\M-f ?\C-f ?\C-x ?r ?  ?e ?\C-\M-$ ?\[ ?^ ?\\ ?\\ ?\] ?\\ ?\{ ?2 ?\\ ?\} ?' ?, ?  return ?\" ?, ?  return ?\C-x ?r ?j ?a ?\C-  ?\C-x ?r ?j ?e ?\C-\M-$ ?, ?  ?\[ ?\' ?\| ?\" ?\] return ?n ?i ?l ?e ?x ?i ?s ?t return ?\C-e ?\C-r ?\( return ?\C-a ?\C-s ?\( return ?\C-0 ?\C-k ?\{ return ?\" ?s ?e ?r ?v ?e ?r ?\" ?  ?: ?  ?\C-s ?n ?i ?l ?e ?x ?i ?s ?t return ?\C-u ?8 backspace ?, return ?\" ?q ?u ?e ?s ?t ?i ?o ?n ?\" ?  ?: ?  ?\" ?\C-s ?n ?i ?l ?e ?x ?i ?s ?t return ?\C-u ?8 backspace ?, return ?\" ?a ?n ?s ?w ?e ?r ?\" ?  ?: ?  ?\" ?\C-s ?n ?i ?l ?e ?x ?i ?s ?t return ?\C-u ?8 backspace ?, return ?\" ?q ?_ ?c ?o ?m ?p ?o ?n ?e ?n ?t ?s ?\" ?  ?: ?  ?\[ ?\" ?\C-e ?\C-b ?\C-r ?, return ?\] ?\C-f ?\C-  ?\C-a ?\C-\M-$ ?n ?i ?l ?e ?x ?i ?s ?t return ?, ?  ?\" return ?\C-e ?\C-r ?, ?\C-f return ?\" ?f ?a ?i ?l ?\" ?  ?: ?\C-k ?  ?T ?r ?u ?e return ?\}])
(global-set-key (kbd "C-x C-k 2") 'addqtest1)

(fset 'convert_time_to_clock
   [?f ?\[ ?f ?\[ ?d ?0 ?I tab ?C ?L ?O ?C ?K ?: ?  escape ?j ?d ?0 ?i backspace ?- ?- ?\C-c ?\C-c escape ?0 ?j])
(global-set-key (kbd "C-x C-k 3") 'convert_time_to_clock)

(fset 'journal_convert
   [?\C-  ?\M-f ?\M-f ?\M-f ?\M-w ?\M-! ?e ?c ?h ?o ?  ?\" ?* ?  ?\s-v ?\" ?  ?> ?> ?  ?j ?o ?u ?r ?n ?a ?l ?. ?o ?r ?g return ?! ?p ?a ?n ?d ?o ?c ?  ?- ?f ?  ?d ?o ?c ?x ?  ?- ?t ?  ?o ?r ?g ?  ?? ?  ?> ?> ?  ?j ?o ?u ?r ?n ?a ?l ?. ?o ?r ?g return ?g ?n])
(global-set-key (kbd "C-x C-k 4") 'journal_convert)

(defun my-macro-query (arg)
  "Prompt for input using minibuffer during kbd macro execution.
With prefix argument, allows you to select what prompt string to use.
If the input is non-empty, it is inserted at point."
  (interactive "P")
  (let* ((query (lambda () (kbd-macro-query t)))
         (prompt (if arg (read-from-minibuffer "PROMPT: ") "Input: "))
         (input (unwind-protect
                    (progn
                      (add-hook 'minibuffer-setup-hook query)
                      (read-from-minibuffer prompt))
                  (remove-hook 'minibuffer-setup-hook query))))
    (unless (string= "" input) (insert input))))
(global-set-key "\C-xQ" 'my-macro-query)

(use-package magit
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package yasnippet
  :config
  (yas-global-mode 1) ;; or M-x yas-reload-all if you've started YASnippet already.
  )

(setq backup-directory-alist '(("." . "~/org/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

;;(if 'server-process
;;    (server-start))
(load "server")
(unless (server-running-p) (server-start))

(global-auto-revert-mode 1)

(setq calendar-latitude 37.759995)
(setq calendar-longitude -122.427046)
(setq calendar-location-name "Fremont, CA")
(add-hook 'calendar-mode-hook
    (lambda ()
      (evil-emacs-state)
      ))

(calendar-set-date-style 'iso)

(add-hook 'artist-mode-hook
          (lambda ()
            (display-line-numbers-mode -1)
            (evil-emacs-state)
            (local-set-key (kbd "<f1>") 'artist-select-op-poly-line)
            (local-set-key (kbd "<f2>") 'artist-select-op-pen-line)
            (local-set-key (kbd "<f3>") 'artist-select-op-line)
            (local-set-key (kbd "<f4>") 'artist-select-op-square)
            (local-set-key (kbd "<f5>") 'artist-select-op-ellipse))
          )

(add-hook 'image-mode-hook
          (lambda ()
            (display-line-numbers-mode -1)
            (evil-emacs-state))
          )

(require 'tetris)
(define-key tetris-mode-map (kbd "z") 'tetris-rotate-prev)
(define-key tetris-mode-map (kbd "x") 'tetris-rotate-next)

(use-package retris
  :load-path "site-lisp/retris")

(desktop-save-mode 1)
(setq desktop-restore-frames nil)
(setq desktop-restore-eager 5)
(setq desktop-path (list "~/emacs/desktopsave/"))
(setq desktop-files-not-to-save "\\(^/[^/:]*:\\|(ftp)$\\|\.gpg$\\|\.org_archive\\)")

(setq erc-log-channels-directory "~/logs/")
(setq erc-save-buffer-on-part t)
(global-set-key (kbd "H-M-e") (lambda () (interactive) (erc :server "irc.freenode.net" :port 6667 :nick "tesrodome" :password passwords_ERC)))

;;; replace-regexp
(global-set-key (kbd "C-M-$") 'replace-regexp)

;;; Open .emacs.d
(global-set-key (kbd "H-C-M-e") (lambda () (interactive) (dired "~/.emacs.d/")))

;;; Regular find-file
(global-set-key (kbd "H-C-x o") (lambda () (interactive) (switch-to-buffer "*Org Agenda*")))


;;; Close window
(global-set-key (kbd "s-0") 'delete-window)

(global-set-key (kbd "<f8>") 'insert-org-image)

(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-location 'bottom)
  )

(global-set-key (kbd "C-v") (lambda () (interactive) (scroll-up-command 1)))
(global-set-key (kbd "M-v") (lambda () (interactive) (scroll-down-command 1)))

(defhydra hydra-windowmanage (global-map "H-c ^")
  "Hydra for window management."
  ("=" enlarge-window "+Vertical")
  ("-" (enlarge-window -1) "-Vertical")
  ("]" enlarge-window-horizontally "+Horizontal")
  ("[" shrink-window-horizontally "-Horizontal")
  ("q" nil "Quit"))

(global-set-key (kbd "C-c C-6") 'hydra-windowmanage/body)

(setq user-full-name "Pierce Wang"
      user-mail-address "pierce.g.wang@gmail.com")

(use-package bufler
  :bind (("C-x C-b" . bufler)
         ("C-x b" . bufler-switch-buffer))
  :config
  (setf bufler-groups
        (bufler-defgroups
          (group
           ;; Subgroup collecting all named workspaces.
           (auto-workspace))
          (group
           ;; Subgroup collecting all `help-mode' and `info-mode' buffers.
           (group-or "*Help/Info*"
                     (mode-match "*Help*" (rx bos "help-"))
                     (mode-match "*Info*" (rx bos "info-"))))
          (group
           ;; Subgroup collecting all special buffers (i.e. ones that are not
           ;; file-backed), except `magit-status-mode' buffers (which are allowed to fall
           ;; through to other groups, so they end up grouped with their project buffers).
           (group-and "*Special*"
                      (lambda (buffer)
                        (unless (or (funcall (mode-match "Magit" (rx bos "magit-status"))
                                             buffer)
                                    (funcall (mode-match "Dired" (rx bos "dired"))
                                             buffer)
                                    (funcall (auto-file) buffer))
                          "*Special*")))
           (group
            ;; Subgroup collecting these "special special" buffers
            ;; separately for convenience.
            (name-match "**Special**"
                        (rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace") "*")))
           (group
            ;; Subgroup collecting all other Magit buffers, grouped by directory.
            (mode-match "*Magit* (non-status)" (rx bos (or "magit" "forge") "-"))
            (auto-directory))
           ;; Remaining special buffers are grouped automatically by mode.
           (auto-mode))
          ;; All buffers under "~/.emacs.d" (or wherever it is).
          (dir user-emacs-directory)
          (group
           ;; Subgroup collecting buffers in `org-directory' (or "~/org" if
           ;; `org-directory' is not yet defined).
           (dir (if (bound-and-true-p org-directory)
                    org-directory
                  "~/org"))
           (dir "~/Dropbox/org/notes/")
           (dir "~/Dropbox/org/notes/college/essays/" 1)
           (group
            ;; Subgroup collecting indirect Org buffers, grouping them by file.
            ;; This is very useful when used with `org-tree-to-indirect-buffer'.
            (auto-indirect)
            (auto-file))
           ;; Group remaining buffers by whether they're file backed, then by mode.
           (group-not "*special*" (auto-file))
           (auto-mode))
          (group
           ;; Subgroup for OHS things
           (dir "~/Google Drive/OHS/")
           (dir "~/Google Drive/OHS/12th Grade/Classes/" 1)
           (dir "~/Google Drive/OHS/11th Grade/" 2)
           ;; Group remaining buffers by whether they're file backed, then by mode.
           (group-not "*special*" (auto-file))
           (auto-mode))
          (dir "/Volumes/" 1)
          (group
           ;; Subgroup collecting buffers in a projectile project.
           (auto-projectile))
          (group
           ;; Subgroup collecting buffers in a version-control project,
           ;; grouping them by directory.
           (auto-project))
          ;; Group remaining buffers by directory, then major mode.
          (auto-directory)
          (auto-mode))))

(setq delete-by-moving-to-trash t)
(setq trash-directory "~/.Trash")
(setq insert-directory-program "/usr/local/bin/gls"
      dired-use-ls-dired t)

(setq dired-dwim-target t)

(define-key dired-mode-map (kbd "P") nil)

(define-key dired-mode-map (kbd "O") 'pgw/dired-open-file)

(use-package dired-quick-sort
  :load-path "custom_load"
  :config
  (dired-quick-sort-setup)
  )

(setq dired-listing-switches "-alh")

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'list-timers 'disabled nil)

(setq browse-url-firefox-program "/Applications/Firefox.app/Contents/MacOS/firefox-bin")

; add the source shipped with mu to load-path
(add-to-list 'load-path (expand-file-name "/usr/local/Cellar/mu/1.4.13/share/emacs/site-lisp/mu/mu4e/"))

; require mu4e
(require 'mu4e)

(setq mu4e-maildir (expand-file-name "~/Maildir"))

; get mail
(setq mu4e-get-mail-command "mbsync -c ~/.emacs.d/mu4e/.mbsyncrc -a"
  ;; mu4e-html2text-command "w3m -T text/html" ;;using the default mu4e-shr2text
  mu4e-view-prefer-html t
  mu4e-update-interval 300
  mu4e-headers-auto-update t
  mu4e-compose-signature-auto-include nil
  mu4e-compose-format-flowed t); tell mu4e to use w3m for html rendering

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; enable inline images
(setq mu4e-view-show-images t)

;; from info manual
(add-to-list 'mu4e-view-actions
             '("ViewInBrowser" . mu4e-action-view-in-browser) t)


;; <tab> to navigate to links, <RET> to open them in browser
(add-hook 'mu4e-view-mode-hook
          (lambda()
            ;; try to emulate some of the eww key-bindings
            (local-set-key (kbd "<RET>") 'mu4e~view-browse-url-from-binding)
            (local-set-key (kbd "<tab>") 'shr-next-link)
            (local-set-key (kbd "<backtab>") 'shr-previous-link)))

;; from https://www.reddit.com/r/emacs/comments/bfsck6/mu4e_for_dummies/elgoumx
(add-hook 'mu4e-headers-mode-hook
      (defun my/mu4e-change-headers ()
        (interactive)
        (setq mu4e-headers-fields
              `((:human-date . 25) ;; alternatively, use :date
                (:flags . 6)
                (:from . 22)
                (:thread-subject . ,(- (window-body-width) 70)) ;; alternatively, use :subject
                (:size . 7)))))

;; if you use date instead of human-date in the above, use this setting
;; give me ISO(ish) format date-time stamps in the header list
;(setq mu4e-headers-date-format "%Y-%m-%d %H:%M")

;; spell check
(add-hook 'mu4e-compose-mode-hook
(defun pgw/do-compose-stuff ()
       "My settings for message composition."
       (visual-line-mode)
       (org-mu4e-compose-org-mode)
           (use-hard-newlines -1)
           (flyspell-mode)))

(add-hook 'mu4e-view-mode-hook #'visual-line-mode)

;; every new email composition gets its own frame!
(setq mu4e-compose-in-new-frame nil)

(require 'smtpmail)

;;rename files when moving
;;NEEDED FOR MBSYNC
(setq mu4e-change-filenames-when-moving t)

;;set up queue for offline email
;;use mu mkdir  ~/Maildir/acc/queue to set up first
(setq smtpmail-queue-mail nil)  ;; start in normal mode

;;from the info manual
(setq mu4e-attachment-dir  "~/Documents")

(setq message-kill-buffer-on-exit t)
(setq mu4e-compose-dont-reply-to-self t)

(require 'org-mu4e)

;; convert org mode to HTML automatically
(setq org-mu4e-convert-to-html t)

;;from vxlabs config
;; show full addresses in view message (instead of just names)
;; toggle per name with M-RET
(setq mu4e-view-show-addresses 't)

;; don't ask when quitting
(setq mu4e-confirm-quit nil)

;; mu4e-context
(setq mu4e-context-policy 'pick-first)
(setq mu4e-compose-context-policy 'always-ask)
(setq mu4e-contexts
  (list
   (make-mu4e-context
    :name "personal" ;;for pierce.g.wang
    :enter-func (lambda () (mu4e-message "Entering context personal"))
    :leave-func (lambda () (mu4e-message "Leaving context personal"))
    :match-func (lambda (msg)
                  (when msg
                (mu4e-message-contact-field-matches
                 msg '(:from :to :cc :bcc) "pierce.g.wang@gmail.com")))
    :vars '((user-mail-address . "pierce.g.wang@gmail.com")
            (user-full-name . "Pierce Wang")
            (mu4e-sent-folder . "/pierce.g.wang/[pierce.g.wang].Sent Mail")
            (mu4e-drafts-folder . "/pierce.g.wang/[pierce.g.wang].drafts")
            (mu4e-trash-folder . "/pierce.g.wang/[pierce.g.wang].Trash")
            (mu4e-refile-folder . "/pierce.g.wang/[pierce.g.wang].All Mail")
            (mu4e-compose-signature . (concat "Formal Signature\n" "Emacs 27, org-mode 9, mu4e 1.14\n"))
            (mu4e-compose-format-flowed . t)
            (smtpmail-queue-dir . "~/Maildir/pierce.g.wang/queue/cur")
            (message-send-mail-function . smtpmail-send-it)
            (smtpmail-smtp-user . "pierce.g.wang")
            (smtpmail-starttls-credentials . (("smtp.gmail.com" 587 nil nil)))
            (smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg"))
            (smtpmail-default-smtp-server . "smtp.gmail.com")
            (smtpmail-smtp-server . "smtp.gmail.com")
            (smtpmail-smtp-service . 587)
            (smtpmail-debug-info . t)
            (smtpmail-debug-verbose . t)
            (mu4e-maildir-shortcuts . ( ("/pierce.g.wang/INBOX"            . ?i)
                                        ("/pierce.g.wang/[pierce.g.wang].Sent Mail" . ?s)
                                        ("/pierce.g.wang/[pierce.g.wang].Trash"     . ?t)
                                        ("/pierce.g.wang/[pierce.g.wang].All Mail"  . ?a)
                                        ("/pierce.g.wang/[pierce.g.wang].Starred"   . ?r)
                                        ("/pierce.g.wang/[pierce.g.wang].drafts"    . ?d)
                                        ))))
   (make-mu4e-context
    :name "OHS" ;;for pgwang@ohs.stanford.edu
    :enter-func (lambda () (mu4e-message "Entering context, OHS"))
    :leave-func (lambda () (mu4e-message "Leaving context, OHS"))
    :match-func (lambda (msg)
                  (when msg
                (mu4e-message-contact-field-matches
                 msg '(:from :to :cc :bcc) "pgwang@ohs.stanford.edu")))
    :vars '((user-mail-address . "pgwang@ohs.stanford.edu")
            (user-full-name . "Pierce Wang")
            (mu4e-sent-folder . "/pierce.g.wang/[pierce.g.wang].Sent Mail")
            (mu4e-drafts-folder . "/pierce.g.wang/[pierce.g.wang].drafts")
            (mu4e-trash-folder . "/pierce.g.wang/[pierce.g.wang].Trash")
            (mu4e-refile-folder . "/pierce.g.wang/[pierce.g.wang].All Mail")
            (mu4e-compose-signature . (concat "Formal Signature\n" "Emacs 27, org-mode 9, mu4e 1.14\n"))
            (mu4e-compose-format-flowed . t)
            (smtpmail-queue-dir . "~/Maildir/pierce.g.wang/queue/cur")
            (message-send-mail-function . smtpmail-send-it)
            (smtpmail-smtp-user . "pierce.g.wang")
            (smtpmail-starttls-credentials . (("smtp.gmail.com" 587 nil nil)))
            (smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg"))
            (smtpmail-default-smtp-server . "smtp.gmail.com")
            (smtpmail-smtp-server . "smtp.gmail.com")
            (smtpmail-smtp-service . 587)
            (smtpmail-debug-info . t)
            (smtpmail-debug-verbose . t)
            (mu4e-maildir-shortcuts . ( ("/pierce.g.wang/INBOX"            . ?i)
                                        ("/pierce.g.wang/[pierce.g.wang].Sent Mail" . ?s)
                                        ("/pierce.g.wang/[pierce.g.wang].Trash"     . ?t)
                                        ("/pierce.g.wang/[pierce.g.wang].All Mail"  . ?a)
                                        ("/pierce.g.wang/[pierce.g.wang].Starred"   . ?r)
                                        ("/pierce.g.wang/[pierce.g.wang].drafts"    . ?d)
                                        ))))
      (make-mu4e-context
       :name "work" ;;for pierce.wang.violin
       :enter-func (lambda () (mu4e-message "Entering context work"))
       :leave-func (lambda () (mu4e-message "Leaving context work"))
       :match-func (lambda (msg)
                     (when msg
                       (mu4e-message-contact-field-matches
                        msg '(:from :to :cc :bcc) "pierce.wang.violin@gmail.com")))
       :vars '((user-mail-address . "pierce.wang.violin@gmail.com")
               (user-full-name . "Pierce Wang")
               (mu4e-sent-folder . "/pierce.wang.violin/[pierce.wang.violin].Sent Mail")
               (mu4e-drafts-folder . "/pierce.wang.violin/[pierce.wang.violin].drafts")
               (mu4e-trash-folder . "/pierce.wang.violin/[pierce.wang.violin].Trash")
               (mu4e-refile-folder . "/pierce.wang.violin/[pierce.wang.violin].All Mail")
               (mu4e-compose-signature . (concat "Formal Signature\n" "Emacs 27, org-mode 9, mu4e 1.14\n"))
               (mu4e-compose-format-flowed . t)
               (smtpmail-queue-dir . "~/Maildir/pierce.wang.violin/queue/cur")
               (message-send-mail-function . smtpmail-send-it)
               (smtpmail-smtp-user . "pierce.wang.violin")
               (smtpmail-starttls-credentials . (("smtp.gmail.com" 587 nil nil)))
               (smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg"))
               (smtpmail-default-smtp-server . "smtp.gmail.com")
               (smtpmail-smtp-server . "smtp.gmail.com")
               (smtpmail-smtp-service . 587)
               (smtpmail-debug-info . t)
               (smtpmail-debug-verbose . t)
               (mu4e-maildir-shortcuts . ( ("/pierce.wang.violin/INBOX"            . ?i)
                                           ("/pierce.wang.violin/[pierce.wang.violin].Sent Mail" . ?s)
                                           ("/pierce.wang.violin/[pierce.wang.violin].Trash"     . ?t)
                                           ("/pierce.wang.violin/[pierce.wang.violin].All Mail"  . ?a)
                                           ("/pierce.wang.violin/[pierce.wang.violin].Starred"   . ?r)
                                           ("/pierce.wang.violin/[pierce.wang.violin].drafts"    . ?d)
                                           ))))
      ))

(use-package mu4e-alert
  :ensure t
  :after mu4e
  :init
  (setq mu4e-alert-interesting-mail-query
        (concat
         "flag:unread maildir:/pierce.wang.violin/INBOX "
         "OR "
         "flag:unread maildir:/pierce.g.wang/INBOX"
         ))
  (mu4e-alert-set-default-style 'notifier)
  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
  (defun pgw/fetch-mail-and-mu4e ()
    (interactive)
    (if (internet-up-p)
        (mu4e-update-mail-and-index t))
    )
  ;; (run-with-timer 60 300 'pgw/fetch-mail-and-mu4e)
  )

(global-unset-key (kbd "C-x m"))
(global-set-key (kbd "C-x m n") (lambda () "Open mu4e in a new frame" (interactive) (make-frame '((name . "Mail: mu4e"))) (mu4e)))
(global-set-key (kbd "C-x m b") (lambda () "Open mu4e in the background" (interactive) (mu4e t)))
(global-set-key (kbd "C-x m m") 'mu4e)
(global-set-key (kbd "C-x m c") 'mu4e-compose-new)

(defun pgw/date-block (absolute y1 m1 d1 y2 m2 d2)
  "Block date entry. An adapted version of the `diary-block'
function from the diary-lib."
  (let ((date1 (calendar-absolute-from-gregorian
                (diary-make-date y1 m1 d1)))
        (date2 (calendar-absolute-from-gregorian
                (diary-make-date y2 m2 d2)))
        (d absolute))
    (and (<= date1 d) (<= d date2))))

(defun pgw/date-date (absolute year month day)
  "Check for equality of date"
  (equal absolute (calendar-absolute-from-gregorian (diary-make-date year month day))))

(defun pgw/check-ohs-class (absolute classname semesters days times fallstart fallend springstart mononfri springend holidays noclasses)
  "Returns a list with formatted strings: (classname curdate
headline). These can then be used to create the headline. The curdate
is in the form of a list"
  (let* ((dayname (calendar-day-of-week (calendar-gregorian-from-absolute absolute)))
         (curdate (calendar-gregorian-from-absolute absolute))
         (periods '("06:00-07:15"
                    "07:15-08:30"
                    "08:30-09:45"
                    "09:45-11:00"
                    "11:00-12:15"
                    "12:15-13:30"
                    "13:30-14:45"
                    "14:45-16:00"
                    "16:00-17:15"
                    "17:15-18:30"
                    "18:30-19:45"
                    "19:45-21:00"
                    "21:00-22:15"))
         (time (if (equal (type-of times) 'integer) ;; Checks if the times argument is an integer or list of times as strings
                   (nth (1- times) periods)
               (nth (- (length days) (length (memq dayname days))) times))))
    (when (and (cond ((equal days '(1 3)) (or (memq dayname '(1 3)) (pgw/date-date absolute (nth 0 mononfri) (nth 1 mononfri) (nth 2 mononfri)))) ;; Account for MLK Monday on Friday
                     (t (memq dayname days)))
               (or (if (memq 1 semesters) (pgw/date-block absolute (nth 0 fallstart) (nth 1 fallstart) (nth 2 fallstart)
                                                         (nth 0 fallend) (nth 1 fallend) (nth 2 fallend)))
                   (if (memq 2 semesters) (pgw/date-block absolute (nth 0 springstart) (nth 1 springstart) (nth 2 springstart)
                                                          (nth 0 springend) (nth 1 springend) (nth 2 springend)))))
      (if (equal (type-of times) 'integer) ;; Classes will always be in periods, OH and other events will not
          (when (and (not (memq 't
                                (mapcar (lambda (holiday) (if (> (length holiday) 3)
                                                              (pgw/date-block absolute (nth 0 holiday) (nth 1 holiday) (nth 2 holiday) (nth 3 holiday) (nth 4 holiday) (nth 5 holiday))
                                                            (pgw/date-date absolute (nth 0 holiday) (nth 1 holiday) (nth 2 holiday))))
                                        holidays)))
                     (not (memq 't
                                (mapcar (lambda (noclass) (if (> (length noclass) 3)
                                                              (pgw/date-block absolute (nth 0 noclass) (nth 1 noclass) (nth 2 noclass) (nth 3 noclass) (nth 4 noclass) (nth 5 noclass))
                                                            (pgw/date-date absolute (nth 0 noclass) (nth 1 noclass) (nth 2 noclass))))
                                        noclasses))))
            (list classname curdate time))
        (when (not (memq 't
                         (mapcar (lambda (holiday) (if (> (length holiday) 3)
                                                       (pgw/date-block absolute (nth 0 holiday) (nth 1 holiday) (nth 2 holiday) (nth 3 holiday) (nth 4 holiday) (nth 5 holiday))
                                                     (pgw/date-date absolute (nth 0 holiday) (nth 1 holiday) (nth 2 holiday))))
                                 holidays)))
          (list classname curdate time))))))

(defun pgw/create-entry (classname semesters days times &optional desc)
  "Creates headlines for class schedule.
CLASSNAME: a string with the class name (to appear on agenda)
SEMESTERS: a list of integers. e.g. for both just a first semester:
'(1) or for both semesters '(1 2)
DAYS: the days of the class. Normally it will be M/W or T/Th but in
order to have flexibility, the function takes an input of another list
of integers representing days of the week. Monday starts on 1 and
Sunday is 0
TIMES: Either an integer with the period number or a cons list
containing a list of the times which should be the same length as the
list of days

optional DESC: string containing a description for the event

This function uses the variable `pgw/ohs-schoolyear-dates' for the value of holidays"
  (let ((current (calendar-absolute-from-gregorian (diary-make-date 2020 8 19)))
        (desc (if desc (setq desc (format "\n%s\n" desc)) (setq desc "")))
        (fallstart (gethash "fallstart" pgw/ohs-schoolyear-dates))
        (fallend (gethash "fallend" pgw/ohs-schoolyear-dates))
        (springstart (gethash "springstart" pgw/ohs-schoolyear-dates))
        (mononfri (gethash "mononfri" pgw/ohs-schoolyear-dates))
        (springend (gethash "springend" pgw/ohs-schoolyear-dates))
        (holidays (gethash "holidays" pgw/ohs-schoolyear-dates))
        (noclasses (gethash "noclasses" pgw/ohs-schoolyear-dates)))
    (goto-char (point-max))
    (insert (format "\n* %s" classname))
    (while (pgw/date-block current (nth 0 fallstart) (nth 1 fallstart) (nth 2 fallstart)
                           (nth 0 springend) (nth 1 springend) (nth 2 springend)) ; Make sure we're within starting and ending dates of school
      (let ((info (pgw/check-ohs-class current classname semesters days times fallstart fallend springstart mononfri springend holidays noclasses)))
        (when info
          (let* ((headline (nth 0 info))
                 (days-of-week '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
                 (fulldate (nth 1 info))
                 (year (nth 2 fulldate))
                 (month (nth 0 fulldate))
                 (day (nth 1 fulldate))
                 (dayofweek (nth (calendar-day-of-week fulldate) days-of-week))
                 (time (nth 2 info)))
            (goto-char (point-max))
            ;; (insert (format "\n** %s\n:PROPERTIES:\n:TIMEZONE: UTC\n:END:\n<%d-%02d-%02d %s %s>\n%s"
            ;;                 headline year month day dayofweek time desc)))))
            (insert (format "\n** %s\n<%d-%02d-%02d %s %s>\n%s"
                            headline year month day dayofweek time desc)))))
      (setq current (+ current 1)))))

(setq pgw/ohs-schoolyear-dates
      #s(hash-table
         size 7
         test equal
         data ("fallstart" (2020 8 19)
               "fallend" (2020 12 19)
               "springstart" (2021 1 4)
               "mononfri" (2021 1 19)
               "springend" (2021 5 13)
               "holidays" ((2020 9 7 2020 9 8) ;; Labor Day
                           (2020 11 25 2020 11 27) ;; Thanksgiving Holiday
                           (2020 12 19 2021 1 3) ;; Winter Closure
                           (2021 1 18 2021 1 19) ;; MLK Holiday
                           (2021 2 15) ;; Presidents Day
                           (2021 2 16) ;; Reading Day (No classes)
                           (2021 3 22 2021 3 26) ;; Spring Break
                           (2021 5 31 2021 6 1)) ;; Memorial Day Holiday
               "noclasses" ((2020 10 28 2020 10 30) ;; Parent-Teacher Conferences (no classes)
                            (2020 12 9 2020 12 11) ;; Study Days (no classes)
                            (2020 12 14 2020 12 19) ;; Fall Semester Finals
                            (2021 1 4 2021 1 8) ;; Reading Week
                            (2021 5 17 2021 5 19) ;; Study Days
                            (2021 5 20 2021 5 21) ;; Spring Semester Finals
                            (2021 5 24 2021 5 27) ;; Spring Semester Finals
                            ))))

(pgw/create-entry "UM51A: Linear Algebra" '(1 2) '(1 3) 3 "Adobe connect class link")

(global-set-key (kbd "C-c s-g o") (lambda () (interactive) (start-process-shell-command "Running ~/QScripts/syncgcal.sh" nil "bash ~/QScripts/syncgcal.sh")))

(defun pgw/turn-on-flyspell-hook ()
  (if (or (string-match "^/Users/piercewang/Google Drive/OHS/" (if (eq buffer-file-name nil) "" buffer-file-name))
          (string-match "^/Users/piercewang/Dropbox/org/notes/college/" (if (eq buffer-file-name nil) "" buffer-file-name)))
      (flyspell-mode 1)))

(add-hook 'text-mode-hook 'pgw/turn-on-flyspell-hook)
