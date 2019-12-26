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
  (setq use-package-expand-minimally byte-compile-current-file))

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
  "Moves image from Dropbox folder to ./media, inserting org-mode link"
  (interactive "P")
  (unless (not (eq system-type 'darwin))
    (let* ((outdir (concat (file-name-directory (buffer-file-name)) "/figures"))
           (namefile (concat (read-string "Enter File Name: ") "_" (format-time-string "%Y%m%d%k%M%S.png")))
           (outfile (expand-file-name namefile outdir)))
      (unless (file-directory-p outdir)
        (make-directory outdir t))
      (message "Argument: %s" swindow)
      (if swindow
          (call-process "screencapture" nil nil nil "-w" outfile)
        (call-process "screencapture" nil nil nil "-i" outfile))
      (insert (concat (concat "[[./figures/" (file-name-nondirectory outfile)) "]]"))))
  )

(set-keyboard-coding-system nil)

(defun pgwang/org-open-link-prop-at-point ()
  "This function opens the link pointed to by the link property \":LINK:\" at a given org node at point"
  (interactive)
  (let ((link (plist-get (org-element--get-node-properties) :LINK)))
    (if (eq link nil)
        (error "Not on a node with :LINK: property!")
      (browse-url-firefox link)
      ))
  )

(defun pgwang/dired-open-file ()
  "In dired, open the file named on this line using the default application in the system."
  (interactive)
  (let ((file (dired-get-filename nil t))
        (filename (dired-get-filename t t)))
    (message "Opening %s..." filename)
    (call-process "open" nil 0 nil file)
    (message "Opening %s done" filename)))

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

(require 'package)
(setq package-archives
    '(("melpa-stable" . "https://stable.melpa.org/packages/")
      ("gnu" . "https://elpa.gnu.org/packages/")
      ("org" . "http://orgmode.org/elpa/")
      ))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/custom_load/")

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

(setq use-package-always-ensure t)

(when (eq system-type 'darwin)
    (setq mac-option-modifier 'meta)
    (setq mac-control-modifier 'control)
    (setq ns-function-modifier 'hyper))

(when (eq system-type 'gnu/linux)
  (setq x-super-keysym 'hyper))

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
(global-set-key (kbd "H-v") 'visual-line-mode)

;(global-visual-line-mode t)
(global-linum-mode 0)
(global-display-line-numbers-mode 1)
(setq-default display-line-numbers 'visual)
(setq display-line-numbers-type 'visual)
(set-default 'truncate-lines t)

;; Make title bar dark
;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; assuming you are using a dark theme
;;(setq ns-use-proxy-icon nil)
;;(setq frame-title-format nil)
(menu-bar-mode -1)
(tool-bar-mode -1)

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
  ;; (load-theme 'doom-solarized-light t)
  (load-theme 'doom-molokai t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  )

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

(use-package treemacs)
(use-package treemacs-evil)
(use-package treemacs-magit)

(set-face-attribute 'variable-pitch nil :family "Avenir Book")

(use-package mixed-pitch
  :load-path "site-lisp/mixed-pitch"
  :config
  ;; (set-face-attribute 'variable-pitch :height 160)
  (setq mixed-pitch-fixed-pitch-faces '(diff-added diff-context diff-file-header diff-function diff-header diff-hunk-header diff-removed font-latex-math-face font-latex-sedate-face font-latex-warning-face font-latex-sectioning-5-face font-lock-builtin-face font-lock-comment-delimiter-face font-lock-constant-face font-lock-doc-face font-lock-function-name-face font-lock-keyword-face font-lock-negation-char-face font-lock-preprocessor-face font-lock-regexp-grouping-backslash font-lock-regexp-grouping-construct font-lock-string-face font-lock-type-face font-lock-variable-name-face markdown-code-face markdown-gfm-checkbox-face markdown-inline-code-face markdown-language-info-face markdown-language-keyword-face markdown-math-face message-header-name message-header-to message-header-cc message-header-newsgroups message-header-xheader message-header-subject message-header-other mu4e-header-key-face mu4e-header-value-face mu4e-link-face mu4e-contact-face mu4e-compose-separator-face mu4e-compose-header-face org-block org-block-begin-line org-block-end-line org-document-info-keyword org-code org-latex-and-related org-checkbox org-meta-line org-table org-verbatim))
  (append-to-list 'mixed-pitch-fixed-pitch-faces '(line-number line-number-current-line org-list-dt org-link))
  ;; (add-hook 'text-mode-hook 'mixed-pitch-mode)
  (global-set-key (kbd "H-f") 'mixed-pitch-mode)
  )

;; (require 'epa-file)
(epa-file-enable)
(setf epa-pinentry-mode 'loopback)

(load-file "~/.passwords.el")

(use-package smart-mode-line
  :config
  ;; (setq sml/theme 'powerline)
  ;(setq sml/theme 'dark)
  (add-to-list 'sml/replacer-regexp-list '("^~/Google Drive/OHS/\\([0-9]\\{2\\}\\)th Grade/Classes/\\([0-9A-Z]*\\)/" ":\\2:"))
  (add-hook 'after-init-hook 'sml/setup)
  )

(size-indication-mode 1)
(line-number-mode -1)

(use-package helm
  :config
  (require 'helm-config)
  (helm-mode 1)
  (define-key global-map [remap find-file] 'helm-find-files)
  (define-key global-map [remap occur] 'helm-occur)
  (define-key global-map [remap list-buffers] 'helm-buffers-list)
  (define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
  (define-key global-map [remap execute-extended-command] 'helm-M-x)
  (unless (boundp 'completion-in-region-function)
    (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
    (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))
  )

(use-package org)
;(use-package org-agenda)

(setq org-directory "~/Dropbox/org/")
(setq org-agenda-files (list "~/Dropbox/org/school.org"
                             "~/Dropbox/org/gtd.org"
                             "~/Dropbox/org/violin.org"
                             "~/Dropbox/org/inbox.org"
                             "~/Dropbox/org/tickler.org"
                             "~/Dropbox/org/gcal.org"
                             "~/Dropbox/org/events.org"))
(setq org-default-notes-file (concat org-directory "/inbox.org"))

(setq org-startup-indented t)

(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

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

;;; Agenda key (C-c a) and other settings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

(setq org-tag-persistent-alist '(("OHS" . ?S)
				 ("noexport" . ?N)))

(setq org-log-done 'time) ; Log when task marked as done

(setq pgwang/refile-targets (file-expand-wildcards "~/Dropbox/org/*.org"))
(setq org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9)
                           (pgwang/refile-targets :maxlevel . 9)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; org-agenda-auto-exclude-function
(defun pgwang/org-my-auto-exclude-function (tag)
  (if
      (string= tag "officehours")
      (concat "-" tag)))
(setq org-agenda-auto-exclude-function 'pgwang/org-my-auto-exclude-function)

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
      '(("q" . "Custom Agenda Views")
        ("qs" "General Agenda" agenda ""
         ((org-agenda-span 1)
          (org-agenda-sorting-strategy
           '((agenda habit-down time-up deadline-up)))
          )
         )
        ("qo" "OHS (Test)"
         ((agenda "" ((org-agenda-span 1)
                      (org-deadline-warning-days 3)
                      ))
          (tags-todo "gcal|class"
                     ((org-agenda-span 5)
                     )))
         ((org-agenda-sorting-strategy '((agenda habit-down time-up deadline-up)
                                         ;; (todo ts-up todo-state-down)
                                         (tags ts-up todo-state-down) 
                                         ;; (search timestamp-up)
                                         )
                                       )))
        )
      )

(defun pgwang/year-month ()
  "Custom function to return date in format: YYYY-MM"
  (format-time-string "%Y-%m"))

(defun pgwang/U ()
  "Custom function to return date in org inactive timestamp format"
  (format-time-string "[%Y-%m-%d %a]"))

(defun pgwang/add-12 ()
  "Custom function return active org timestamp with exactly 24 hour difference"
  (format-time-string "%Y-%m-%d %a %H:%M" (time-add (current-time) 85500)))

(defun pgwang/headline_date ()
  "Function to find the date as headline for Violin capture template"
  (beginning-of-buffer)
  (let ((searchresults (search-forward (format-time-string "[%Y-%m-%d %a]") nil t)))
    (if searchresults
        'searchresults
      (error "Not found! Use Vc to create today's practice first.")
      )
    )
  )

(setq org-capture-templates
      '(
("i" "Inbox" entry (file "~/Dropbox/org/inbox.org")
"* TODO %?")
("n" "Quick Note" entry
 (file "~/Dropbox/org/inbox.org")
 "* %?
%U")
("e" "Event" entry (file "~/Dropbox/org/events.org")
"* %?
%^t")
("L" "Link" entry (file+headline "~/Dropbox/org/links.org" "!Inbox")
"* [[%?%:link][%:description]]
:PROPERTIES:
:CREATED: %U
:END:" :prepend t)
("m" "Manual" entry (file "~/Dropbox/org/notes.org")
"* %?
:PROPERTIES:
:CREATED: %U
:END:" :empty-lines 1)
("b" "Book" entry (file+headline "~/Dropbox/org/notes.org" "Books")
 "* %^{RATING}p%^{Book Title}")
("j" "Journal" entry
(file+olp+datetree "~/Dropbox/org/orgjournal.org.gpg")
"* %^{RATING}p%?
:PROPERTIES:
:LOGGED: %^{Logged Time}U
:END:
" :kill-buffer t)
("S" "School")
("Se" "OE020B" entry
 (file+headline "~/Dropbox/org/school.org" "_\\ *sOE020B* \\_")
 "* TODO %?
DEADLINE: <%<%Y-%m-%d %a 13:30>>")
("Sp" "OP005" entry
 (file+headline "~/Dropbox/org/school.org" "_\\ *sOP005* \\_")
 "* TODO %?
DEADLINE: <%<%Y-%m-%d %a 14:45>>")
("Sd" "ODFRL" entry
 (file+headline "~/Dropbox/org/school.org" "_\\ *sODFRL* \\_")
 "* TODO %?
DEADLINE: <%<%Y-%m-%d %a 16:00>>")
("Sh" "OH011A" entry
 (file+headline "~/Dropbox/org/school.org" "_\\ *sOH011A* \\_")
 "* TODO %?
DEADLINE: <%<%Y-%m-%d %a 08:30>>")
("Sm" "UM52A" entry
 (file+headline "~/Dropbox/org/school.org" "_\\ *sUM52A* \\_")
 "**** TODO %?
DEADLINE: <%<%Y-%m-%d %a 13:30>>")
("m" "Music")
("mM" "Musicianship Homework" entry
 (file+headline "~/Dropbox/org/gtd.org" "Musicianship")
 "* TODO Musicianship Homework [/]
DEADLINE: %^t
- [ ] Written: %^{Written Homework}
- [ ] Singing: %^{Singing}
- [ ] Rhythm: %^{Rhythm}
- [ ] Keyboard: %^{Keyboard}")
("mc" "Conducting Homework" entry
 (file+headline "~/Dropbox/org/music.org" "Homework")
 "* TODO Conducting Homework
DEADLINE: %^t
- ")
("V" "Violin")
("Vc" "Create Practice Entry" entry
 (file+olp "~/Dropbox/org/violin.org" "Practice Log")
 "* [%<%Y-%m-%d %a>]
%t%?"
 :clock-in t :clock-keep t)
("Vd" "Add practice details" item
 (file+function "~/Dropbox/org/violin.org" pgwang/headline_date)
 "%?"
 :clock-in t)
))

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
  )

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

;;; org-drill
(use-package org-drill
  :load-path "custom_load")

(require 'ox-latex)

(use-package cdlatex
  :config
  (define-key org-cdlatex-mode-map (kbd "H-d") 'cdlatex-dollar)
  (define-key cdlatex-mode-map (kbd "H-d") 'cdlatex-dollar)
  (add-hook 'org-mode-hook
            (lambda ()
              (org-cdlatex-mode)
              ))
  )

(setq org-format-latex-options
      '(:foreground "#d6d6d4" :background default 
                    :scale 1.4
                    :html-foreground "Black" :html-background "Transparent"
                    :html-scale 1.0 
                    :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

(let ((dvipng--plist (alist-get 'dvipng org-preview-latex-process-alist)))
  (plist-put dvipng--plist :use-xcolor t)
  (plist-put dvipng--plist :image-converter '("dvipng -D %D -T tight -o %O %f")))

(use-package org-bullets
    :hook (org-mode . org-bullets-mode))

(setq org-export-async-init-file "~/.emacs.d/orgasyncinit.el")

(setq TeX-engine 'xetex)
(setq latex-run-command "xetex")

(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t))

(unless (find "Times" org-latex-classes :key 'car
              :test 'equal)
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
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("MLA"
                 "\\documentclass[12pt]{article}
%
%Margin - 1 inch on all sides
%
\\usepackage[letterpaper]{geometry}
\\usepackage{fontspec}
\\setmainfont{Times New Roman}
\\geometry{top=1.0in, bottom=1.0in, left=1.0in, right=1.0in}

%
%Doublespacing
%
\\usepackage{setspace}
\\doublespacing

%
%Rotating tables (e.g. sideways when too long)
%
\\usepackage{rotating}


%
%Fancy-header package to modify header/page numbering (insert last name)
%
\\usepackage{fancyhdr}
\\pagestyle{fancy}
\\lhead{} 
\\chead{} 
\\rhead{Wang \\thepage} 
\\lfoot{} 
\\cfoot{} 
\\rfoot{} 
\\renewcommand{\\headrulewidth}{0pt} 
\\renewcommand{\\footrulewidth}{0pt} 
%To make sure we actually have header 0.5in away from top edge
%12pt is one-sixth of an inch. Subtract this from 0.5in to get headsep value
\\setlength\\headsep{0.333in}

%
%Works cited environment
%(to start, use \\begin{workscited...}, each entry preceded by \\bibent)
% - from Ryan Alcock's MLA style file
%
\\newcommand{\\bibent}{\\noindent \\hangindent 40pt}
\\newenvironment{workscited}{\\newpage \\begin{center} Works Cited \\end{center}}{\\newpage }


%
%Begin document
%
\\begin{document}
%commented until I can add this in the org-latex-export function using advice
%\\begin{flushleft}

%%%%First page name, class, etc
Pierce Wang\\\\
Professor\\\\
Class\\\\
February 11 2019\\\\


%%%%Title
\\begin{center}
Paper Title
\\end{center}


%%%%Changes paragraph indentation to 0.5in
\\setlength{\\parindent}{0.5in}
%%%%Begin body of paper here
[NO-DEFAULT-PACKAGES]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
               ))

;Probably not needed
;(add-to-list 'load-path "~/.emacs.d/site-lisp/evil")
(use-package evil
  :config
  (evil-mode t)
  (add-hook 'dired-mode-hook 'evil-emacs-state)
  (add-hook 'calendar-mode-hook 'evil-emacs-state)
  (add-hook 'calendar-load-hook 'evil-emacs-state)
  (add-hook 'display-time-hook 'evil-emacs-state)
  )

(define-key evil-normal-state-map (kbd "<S-return>") [?m ?` ?o escape ?` ?`])
(define-key evil-normal-state-map (kbd "<s-S-return>") [?m ?` ?O escape ?` ?`])
(define-key evil-motion-state-map (kbd "k") 'previous-line)
(define-key evil-motion-state-map (kbd "j") 'next-line)

(elpy-enable)

(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))
(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))

(fset 'setupworkspace
   [?\C-c ?a ?q ?s ?. ?\C-x ?0 M-f10 ?\C-x ?3 ?\H-l ?\H-\C-x ?o ?\C-x ?2 ?\C-u ?7 ?\C-x ?^ ?\H-j ?\H-c ?i ?\H-h ?\H-c ?o ?\H-l])
(global-set-key (kbd "C-x C-k 1") 'setupworkspace)

;;(fset 'OHSFigureSave
;;   [?# ?+ ?C ?A ?P ?T ?I ?O ?N ?: ?  ?\C-x ?Q return return tab ?\[ ?\[ ?f ?i ?l ?e ?: ?. ?/ ?W ?e ?e ?k ?  ?\C-x ?Q return ?/ ?\C-x ?Q return ?_ ?\C-u ?\M-! ?d ?a ?t ?e ?  ?+ ?% ?H ?% ?M ?% ?S return escape ?e ?a ?. ?p ?n ?g escape ?v ?B ?F ?/ ?l ?y escape ?A ?\] ?\] return escape ?p ?0 ?i ?\M-x ?i ?n ?s ?e ?r ?t ?d ?i ?r ?e ?c ?t ?o ?r ?y return escape ?V ?d ?i ?\C-x ?\C-f ?\C-  ?\C-a backspace ?/ ?U ?s ?e ?r ?s ?/ ?p ?i ?e ?r ?c ?e ?w ?a ?n ?g ?/ ?S ?c ?r ?e ?e ?n ?s ?h ?o ?t ?s return ?s ?\M-< ?\C-z ?/ ?S ?c ?r ?e ?e ?n ?  ?S ?h ?o ?t return ?R ?\C-  ?\C-a backspace ?\s-v backspace return ?\C-x ?k return])
;;(global-set-key (kbd "<f9>") 'OHSFigureSave)

(defun pgwang/disable-helm ()
  "Disable Helm"
  (interactive)
  (helm-mode 0))
(defun pgwang/enable-helm ()
  "Enable Helm"
  (interactive)
  (helm-mode))
(global-set-key (kbd "H-x H-h d") 'pgwang/disable-helm)
(global-set-key (kbd "H-x H-h e") 'pgwang/enable-helm)

;(fset 'importChineseFlashcards
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

(fset 'getLink
   [?\C-c ?\C-l ?\C-  ?\C-a ?\M-w return return])
(global-set-key (kbd "C-c s-l") 'getLink)

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

;; Auto close gpg buffers
;(run-with-idle-timer 60 t (lambda ()
;                         (let ((victim (get-buffer "orgjournal.org.gpg")))
;                           (when (and victim (not (buffer-modified-p victim))) (message "Killing buffer %s" (buffer-name victim)
;                                                                                        (kill-buffer victim))))))

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

(setq calendar-latitude 37.550201)
(setq calendar-longitude -121.980827)
(setq calendar-location-name "Fremont, CA")
(add-hook 'calendar-mode-hook
    (lambda ()
      (evil-emacs-state)
      ))

(add-hook 'artist-mode-hook
    (lambda ()
      (local-set-key (kbd "<f1>") 'org-mode)
      (local-set-key (kbd "<f2>") 'artist-select-op-pen-line) ; f2 = pen mode
      (local-set-key (kbd "<f3>") 'artist-select-op-line)     ; f3 = line
      (local-set-key (kbd "<f4>") 'artist-select-op-square)   ; f4 = rectangle
      (local-set-key (kbd "<f5>") 'artist-select-op-ellipse)  ; f5 = ellipse
      (display-line-numbers-mode -1)
      (evil-emacs-state)
      ))

(add-hook 'tetris-mode-hook (lambda ()
                              (define-key tetris-mode-map "z" 'tetris-rotate-prev)
                              (define-key tetris-mode-map "x" 'tetris-rotate-next)))

(desktop-save-mode 1)
(setq desktop-restore-frames nil)
(setq desktop-path (list "~/emacs/desktopsave/"))

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

(global-set-key (kbd "H-c H-o") 'pgwang/org-open-link-prop-at-point)

(global-set-key (kbd "<f8>") 'insert-org-image)

(defhydra hydra-windowmanage (global-map "H-c ^")
  "Hydra for window management."
  ("=" enlarge-window "+Vertical")
  ("-" (enlarge-window -1) "-Vertical")
  ("]" enlarge-window-horizontally "+Horizontal")
  ("[" shrink-window-horizontally "-Horizontal")
  ("q" nil "Quit"))

(global-set-key (kbd "C-c C-6") 'hydra-windowmanage/body)

;;; Email
(setq user-mail-address "pierce.g.wang@gmail.com")

(use-package ibuffer
  :config
  (global-set-key (kbd "C-x C-b") 'ibuffer))
(setq ibuffer-saved-filter-groups
      '(("default"
         ("emacs-config" (or (filename . "/.emacs.d/")
                             (filename . ".emacs.d/init.el")))
         ("OHS" (filename . "/Google Drive/OHS/"))
         ("Org" (filename . "/Dropbox/org/"))
         ("planner" (or
                    (name . "\*Calendar\*")
                    (name . "\*Org Agenda\*")
                    (name . "^diary$")))
         ("Helm" (name . "\*helm.*"))
         ("Magit" (mode . Magit))
         ("ERC" (mode . erc-mode))
         ("Help" (or (name . "\*Help\*")
                     (name . "\*info\*")
                     (name . "\*GNU Emacs\*"))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))
(define-key ibuffer-mode-map (kbd "P") nil)

(setq delete-by-moving-to-trash t)
(setq insert-directory-program "gls")

(setq dired-dwim-target t)

(define-key dired-mode-map (kbd "P") nil)

(define-key dired-mode-map (kbd "O") 'pgwang/dired-open-file)

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

(setq browse-url-firefox-program "/Applications/Firefox.app/Contents/MacOS/firefox-bin")

(put 'narrow-to-region 'disabled nil)

(defun pgwang/turn-on-flyspell-hook ()
  (cond ((string-match "^/Users/piercewang/Google Drive/OHS/11th Grade/Classes/" (if (eq buffer-file-name nil) "" buffer-file-name))
         (flyspell-mode 1))))

(add-hook 'text-mode-hook 'pgwang/turn-on-flyspell-hook)
