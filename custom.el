(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(custom-safe-themes
   '("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "b9e9ba5aeedcc5ba8be99f1cc9301f6679912910ff92fdf7980929c2fc83ab4d" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(debug-on-error nil)
 '(default-input-method "TeX")
 '(epg-gpg-program "/usr/local/bin/gpg")
 '(erc-modules
   '(autojoin button completion fill irccontrols list log match menu move-to-prompt netsplit networks noncommands readonly ring stamp track))
 '(helm-mode t)
 '(ibuffer-saved-filter-groups
   '(("Coursera"
      ("Coursera"
       (filename . "coursera"))
      ("emacs-config"
       (or
        (filename . "/.emacs.d/")
        (filename . ".emacs.d/init.el")))
      ("OHS"
       (filename . "/Google Drive/OHS/"))
      ("Org"
       (filename . "/Dropbox/org/"))
      ("planner"
       (or
        (name . "*Calendar*")
        (name . "*Org Agenda*")
        (name . "^diary$")))
      ("Helm"
       (name . "*helm.*"))
      ("Magit"
       (mode . Magit))
      ("ERC"
       (mode . erc-mode))
      ("Help"
       (or
        (name . "*Help*")
        (name . "*info*")
        (name . "*GNU Emacs*"))))
     ("default"
      ("emacs-config"
       (or
        (filename . "/.emacs.d/")
        (filename . ".emacs.d/init.el")))
      ("OHS"
       (filename . "/Google Drive/OHS/"))
      ("Org"
       (filename . "/Dropbox/org/"))
      ("planner"
       (or
        (name . "*Calendar*")
        (name . "*Org Agenda*")
        (name . "^diary$")))
      ("Helm"
       (name . "*helm.*"))
      ("Magit"
       (mode . Magit))
      ("ERC"
       (mode . erc-mode))
      ("Help"
       (or
        (name . "*Help*")
        (name . "*info*")
        (name . "*GNU Emacs*"))))) t)
 '(ibuffer-saved-filters
   '(("programming"
      (or
       (derived-mode . prog-mode)
       (mode . ess-mode)
       (mode . compilation-mode)))
     ("text document"
      (and
       (derived-mode . text-mode)
       (not
        (starred-name))))
     ("TeX"
      (or
       (derived-mode . tex-mode)
       (mode . latex-mode)
       (mode . context-mode)
       (mode . ams-tex-mode)
       (mode . bibtex-mode)))
     ("web"
      (or
       (derived-mode . sgml-mode)
       (derived-mode . css-mode)
       (mode . javascript-mode)
       (mode . js2-mode)
       (mode . scss-mode)
       (derived-mode . haml-mode)
       (mode . sass-mode)))
     ("gnus"
      (or
       (mode . message-mode)
       (mode . mail-mode)
       (mode . gnus-group-mode)
       (mode . gnus-summary-mode)
       (mode . gnus-article-mode)))))
 '(indent-tabs-mode nil)
 '(org-modules
   '(org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m org-drill org-learn))
 '(package-selected-packages
   '(all-the-icons mu4e-alert ztree org-superstar treemacs-magit treemacs-evil treemacs tramp org-gcal json-mode org-mode use-package cdlatex org-noter artist-mode python-docstring markdown-mode epa-file framemove doom-themes htmlize lua-mode helm magit elpy exec-path-from-shell smart-mode-line latex auctex evil-visual-mark-mode))
 '(show-paren-mode nil)
 '(sml/pre-modes-separator (propertize " " 'face 'sml/modes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(line-number ((t (:inherit default :foreground "gray40" :strike-through nil :underline nil :slant normal :weight normal)))))
