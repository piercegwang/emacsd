(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(custom-enabled-themes (quote (smart-mode-line-powerline)))
 '(custom-safe-themes
   (quote
    ("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(default-input-method "TeX")
 '(epg-gpg-program "/usr/local/bin/gpg")
 '(erc-modules
   (quote
    (autojoin button completion fill irccontrols list log match menu move-to-prompt netsplit networks noncommands notifications readonly ring stamp track)))
 '(org-agenda-files
   (list "~/Dropbox/org/school.org" "~/Dropbox/org/todo.org" "~/Dropbox/org/violin.org"))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m org-drill org-learn)))
 '(org-structure-template-alist
   (quote
    (("s" "#+BEGIN_SRC ?

#+END_SRC")
     ("e" "#+BEGIN_EXAMPLE
?
#+END_EXAMPLE")
     ("q" "#+BEGIN_QUOTE
?
#+END_QUOTE")
     ("v" "#+BEGIN_VERSE
?
#+END_VERSE")
     ("V" "#+BEGIN_VERBATIM
?
#+END_VERBATIM")
     ("c" "#+BEGIN_CENTER
?
#+END_CENTER")
     ("C" "#+BEGIN_COMMENT
?
#+END_COMMENT")
     ("l" "#+BEGIN_EXPORT latex
?
#+END_EXPORT")
     ("L" "#+LaTeX: ")
     ("h" "#+BEGIN_EXPORT html
?
#+END_EXPORT")
     ("H" "#+HTML: ")
     ("a" "#+BEGIN_EXPORT ascii
?
#+END_EXPORT")
     ("A" "#+ASCII: ")
     ("i" "#+INDEX: ?")
     ("I" "#+INCLUDE: %file ?"))))
 '(package-selected-packages
   (quote
    (elpy exec-path-from-shell smart-mode-line-powerline-theme smart-mode-line latex auctex evil-visual-mark-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(require 'package)
;;Install melpa
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)
;;evil mode
(add-to-list 'load-path "~/.emacs.d/site-lisp/evil")
(require 'evil)
(evil-mode t)

;; Chinese Fonts
(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (set-fontset-font "fontset-default"
                                    'chinese-gbk "WenQuanYi Micro Hei Mono 14"))))
  (set-fontset-font "fontset-default" 'chinese-gbk "WenQuanYi Micro Hei Mono 14"))

;; elpy
(elpy-enable)

;; Org Configuration
(require 'org)
(setq org-directory "~/Dropbox/org/")
(setq org-agenda-files (list "~/Dropbox/org/school.org"
			     "~/Dropbox/org/todo.org"
			     "~/Dropbox/org/violin.org"))
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "DONE")))
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

;; org-drill
(add-to-list 'load-path "~/.emacs.d/custom_load/")
(require 'org-drill)
;; org-capture-templates

(setq org-capture-templates
 '(("t" "Todo" entry (file+headline "~/Dropbox/org/todo.org" "Tasks")
;;        "* TODO %?\n  %i\n  %a")
        "* TODO %?")
   ("L" "Link" entry (file "~/Dropbox/org/links.org")
        "* TOREAD %?[[%:link][%:description]] %U\n" :prepend t)))

;; Setting default frame settings
(add-to-list 'default-frame-alist '(height . 46))
(add-to-list 'default-frame-alist '(width . 146))
(set-default-font "Source Code Pro")
(set-face-attribute 'variable-pitch
                     nil
                     :family "Source Code Pro")

;; Agenda key (C-c a) and other settings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

;; Visual line mode (for text wrapping)
(global-visual-line-mode t)
(global-linum-mode t)

;; Include Texbin in PATH
;;(setq exec-path (append exec-path '("/Library/TeX/texbin")) )
;;(setenv "PATH" "/usr/local/bin:/Library/TeX/texbin:$PATH" t)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.4))

;; smart-mode-line
(require 'smart-mode-line)
(load-theme 'tango-dark t)
(setq sml/theme 'powerline)
(sml/setup)

;; GPG
(require 'epa-file)

(epa-file-enable)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; In case future me wants to change bash version
(setq shell-file-name "/bin/zsh")
(put 'scroll-left 'disabled nil)

;; Kbd Macros
(fset 'setupworkspace
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("aao23iooopo" 0 "%d")) arg)))
(global-set-key (kbd "C-x C-k 1") 'setupworkspace)

;; Run Emacs as Daemon
(server-start)

;; ERC
(setq erc-log-channels-directory "~/logs/")
(setq erc-save-buffer-on-part t)
