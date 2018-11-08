;; init.el by Pierce Wang

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages
(require 'package)
(setq package-archives
    '(("melpa-stable" . "https://stable.melpa.org/packages/")
      ("gnu" . "https://elpa.gnu.org/packages/")
      ))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; include paths / dirs
(add-to-list 'load-path "~/.emacs.d/custom_load/")
(setq init-dir "~/.emacs.d/init_files/")
(add-to-list 'load-path (expand-file-name "init_files" user-emacs-directory))
(setq custom-file (concat init-dir "custom.el"))
(load custom-file)
;;; exec-path-from-shell-initialize
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(load-file "~/.passwords.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evil mode
(add-to-list 'load-path "~/.emacs.d/site-lisp/evil")
(require 'evil)
(evil-mode t)
(add-hook 'dired-mode-hook #'evil-emacs-state)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fonts
(require 'init-fonts)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elpy
(elpy-enable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Configuration
(require 'init-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cdlatex
(load "~/.emacs.d/custom_load/cdlatex.el")
(define-key org-cdlatex-mode-map (kbd "C-c M-d") 'cdlatex-dollar)
(define-key cdlatex-mode-map (kbd "C-c M-d") 'cdlatex-dollar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visuals
;;; Frame
(add-to-list 'default-frame-alist '(height . 46))
(add-to-list 'default-frame-alist '(width . 146))

;;; Visual line mode (for text wrapping)
(global-visual-line-mode t)
(global-linum-mode t)
(set-default 'truncate-lines t)

;;; smart-mode-line
(require 'smart-mode-line)
(load-theme 'tango-dark t)
(setq sml/theme 'powerline)
(sml/setup)
(add-to-list 'sml/replacer-regexp-list '("^~/Google Drive/OHS/\\([0-9]\\{2\\}\\)th Grade/Semester [0-9]/\\([0-9A-Z]*\\)/" ":\\2:"))

;; Make title bar dark
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; assuming you are using a dark theme
;;(setq ns-use-proxy-icon nil)
;;(setq frame-title-format nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GPG
(require 'epa-file)
(epa-file-enable)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(setf epa-pinentry-mode 'loopback)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Tweaks

;;; Change shell process (from bash to zsh)
(setq shell-file-name "/bin/zsh")
(put 'scroll-left 'disabled nil)

;;; default major mode
(setq-default major-mode 'org-mode)

;;; Email
(setq user-mail-address "pierce.g.wang@gmail.com")

;;;
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

(menu-bar-mode -1)
(tool-bar-mode -1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Functions

;;; Increment Numbers
(defun increment-number-at-point ()
  "Increments numbers at cursor"
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))
(global-set-key (kbd "C-; C-=") 'increment-number-at-point)

(defun insertdirectory ()
  "Insert current directory for macro use"
  (interactive)
  (insert default-directory))

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

;;; Switching Frames
(defun hyper-window-left (count)
  "Move the cursor to new COUNT-th window left of the current one."
  :repeat nil
  (interactive "p")
  (dotimes (i count)
    (windmove-left)))

(defun hyper-window-right (count)
  "Move the cursor to new COUNT-th window right of the current one."
  :repeat nil
  (interactive "p")
  (dotimes (i count)
    (windmove-right)))

(defun hyper-window-up (count)
  "Move the cursor to new COUNT-th window above the current one."
  :repeat nil
  (interactive "p")
  (dotimes (i (or count 1))
    (windmove-up)))

(defun hyper-window-down (count)
  "Move the cursor to new COUNT-th window below the current one."
  :repeat nil
  (interactive "p")
  (dotimes (i (or count 1))
    (windmove-down)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard Macros
(fset 'setupworkspace
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("aao23iooopo" 0 "%d")) arg)))
(global-set-key (kbd "C-x C-k 1") 'setupworkspace)

(fset 'OHSFigureSave
   [?# ?+ ?C ?A ?P ?T ?I ?O ?N ?: ?  ?\C-x ?Q return return tab ?\[ ?\[ ?f ?i ?l ?e ?: ?. ?/ ?W ?e ?e ?k ?  ?\C-x ?Q return ?/ ?\C-x ?Q return ?_ ?\C-u ?\M-! ?d ?a ?t ?e ?  ?+ ?% ?H ?% ?M ?% ?S return escape ?e ?a ?. ?p ?n ?g escape ?v ?B ?F ?/ ?l ?y escape ?A ?\] ?\] return escape ?p ?0 ?i ?\M-x ?i ?n ?s ?e ?r ?t ?d ?i ?r ?e ?c ?t ?o ?r ?y return escape ?V ?d ?i ?\C-x ?\C-f ?\C-  ?\C-a backspace ?/ ?U ?s ?e ?r ?s ?/ ?p ?i ?e ?r ?c ?e ?w ?a ?n ?g ?/ ?S ?c ?r ?e ?e ?n ?s ?h ?o ?t ?s return ?s ?\M-< ?\C-z ?/ ?S ?c ?r ?e ?e ?n ?  ?S ?h ?o ?t return ?R ?\C-  ?\C-a backspace ?\s-v backspace return ?\C-x ?k return])
(global-set-key (kbd "<f9>") 'OHSFigureSave)

(fset 'importChineseFlashcards
   [return ?\C-p ?* ?* ?  ?I ?t ?e ?m ?\C-c ?\C-c ?d ?r ?i ?l ?l return ?\C-n ?\C-a ?\C-z ?f ?= ?x ?x ?\C-z ?\C-k ?\C-n ?\C-a return return ?\C-p ?* ?* ?  ?A ?n ?s ?w ?e ?r ?\C-a ?* ?\C-n ?\C-a ?\C-y ?\; ?  ?\C-a ?\C-n ?\C-n])
(global-set-key (kbd "<f6>") 'importChineseFlashcards)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run Emacs as Daemon
;;(if 'server-process
;;    (server-start))
(load "server")
(unless (server-running-p) (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ERC
(setq erc-log-channels-directory "~/logs/")
(setq erc-save-buffer-on-part t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybinds
;; Set keys only for Mac OSX
(when (eq system-type 'darwin)
    (setq mac-option-modifier 'meta)
    (setq mac-control-modifier 'control)
    (setq ns-function-modifier 'hyper))

;;; ERC Keybinds
(global-set-key (kbd "H-M-e") (lambda () (interactive) (erc :server "irc.freenode.net" :port 6667 :nick "pgwang" :password passwords_ERC)))
                              

;;; Moving Windows with Hyper (Fn)
(global-set-key (kbd "H-h") 'hyper-window-left)
(global-set-key (kbd "H-l") 'hyper-window-right)
(global-set-key (kbd "H-k") 'hyper-window-up)
(global-set-key (kbd "H-j") 'hyper-window-down)

;;; Visual Line Mode
(global-set-key (kbd "H-v") 'visual-line-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Artist-mode
(add-hook 'artist-mode-hook
    (lambda ()
      (local-set-key (kbd "<f1>") 'org-mode)
      (local-set-key (kbd "<f2>") 'artist-select-op-pen-line) ; f2 = pen mode
      (local-set-key (kbd "<f3>") 'artist-select-op-line)     ; f3 = line
      (local-set-key (kbd "<f4>") 'artist-select-op-square)   ; f4 = rectangle
      (local-set-key (kbd "<f5>") 'artist-select-op-ellipse)  ; f5 = ellipse
      ))
(put 'dired-find-alternate-file 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calendar Mode
(setq calendar-latitude 37.550201)
(setq calendar-longitude -121.980827)
(setq calendar-location-name "Fremont, CA")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
