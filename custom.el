(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-safe-themes
   '("730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "7b3ce93a17ce4fc6389bba8ecb9fee9a1e4e01027a5f3532cc47d160fe303d5a" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "b9e9ba5aeedcc5ba8be99f1cc9301f6679912910ff92fdf7980929c2fc83ab4d" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(debug-on-error nil)
 '(default-input-method "TeX")
 '(elpy-rpc-python-command "python3")
 '(epg-gpg-program "/usr/local/bin/gpg")
 '(erc-modules
   '(autojoin button completion fill irccontrols list log match menu move-to-prompt netsplit networks noncommands readonly ring stamp track))
 '(global-font-lock-mode t)
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
        (name . "*GNU Emacs*"))))))
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
 '(org-agenda-files
   '("~/Dropbox/org/SOHS_GradWeekend_2021.org" "~/Dropbox/org/classes.org" "~/Dropbox/org/events.org" "~/Dropbox/org/inbox.org" "~/Dropbox/org/init.org" "~/Dropbox/org/links.org" "~/Dropbox/org/music.org" "~/Dropbox/org/notes.org" "~/Dropbox/org/ohs_gcal.org" "~/Dropbox/org/packing.org" "~/Dropbox/org/people.org" "~/Dropbox/org/projects.org" "~/Dropbox/org/random_thoughts.org" "~/Dropbox/org/school.org" "~/Dropbox/org/someday.org" "~/Dropbox/org/tickler.org" "~/Dropbox/org/violin.org" "~/Dropbox/org/20210403093601-conducting_homework.org.gpg" "~/Dropbox/org/people_notes.org.gpg" "~/Dropbox/org/calendars/cal_emacs.org" "~/Dropbox/org/calendars/cal_gmail.org" "~/Dropbox/org/calendars/cal_sfcm.org"))
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-eww ol-gnus org-habit ol-info ol-irc ol-mhe ol-rmail ol-w3m))
 '(package-selected-packages
   '(unicode-fonts org-roam lsp-treemacs dap-mode yasnippet rotate 2048-game bufler powerline ox-reveal modus-operandi-theme lsp-ivy lsp-java which-key lsp-ui lsp-mode flycheck projectile company-jedi jedi company counsel "ivy" ivy gnuplot all-the-icons ztree org-superstar treemacs-magit treemacs-evil treemacs tramp org-gcal json-mode org-mode use-package cdlatex org-noter artist-mode python-docstring markdown-mode epa-file framemove doom-themes htmlize lua-mode magit elpy exec-path-from-shell smart-mode-line latex auctex evil-visual-mark-mode))
 '(python-shell-interpreter "python3")
 '(safe-local-variable-values
   '((org-roam-db-location . "/Users/piercewang/Google Drive/OHS/12th Grade/Classes/OCRA1/notes/org-roam.db")
     (org-roam-directory . "/Users/piercewang/Google Drive/OHS/12th Grade/Classes/OCRA1/notes/")
     (org-roam-encrypt-files)
     (org-roam-db-update-method . immediate)
     (org-roam-dailies-capture-templates
      ("c" "Class" plain #'org-roam-capture--get-point "%?" :file-name "daily/class-%<%Y%m%d%H%M%S>" :head "#+setupfile: ~/.emacs.d/org-mode/templates/general/note.org
#+setupfile: ~/.emacs.d/org-mode/templates/school/name_email.org
#+setupfile: ~/.emacs.d/org-mode/templates/school/writing_setup.org
#+setupfile: ~/.emacs.d/org-mode/templates/general/web.org
#+roam_tags: discussion
#+options: toc:nil
#+title: " :unnarrowed t)
      ("o" "Office Hours" plain #'org-roam-capture--get-point "%?" :file-name "daily/oh-%<%Y%m%d%H%M%S>" :head "#+setupfile: ~/.emacs.d/org-mode/templates/general/note.org
#+setupfile: ~/.emacs.d/org-mode/templates/school/name_email.org
#+setupfile: ~/.emacs.d/org-mode/templates/school/writing_setup.org
#+setupfile: ~/.emacs.d/org-mode/templates/general/web.org
#+roam_tags: office_hours
#+options: toc:nil
#+title: " :unnarrowed t))
     (org-roam-capture-templates
      ("r" "Readings" plain #'org-roam-capture--get-point "%?" :file-name "%<%Y%m%d%H%M%S>-${slug}" :head "#+setupfile: ~/.emacs.d/org-mode/templates/general/note.org
#+setupfile: ~/.emacs.d/org-mode/templates/school/name_email.org
#+setupfile: ~/.emacs.d/org-mode/templates/school/writing_setup.org
#+setupfile: ~/.emacs.d/org-mode/templates/general/web.org
#+roam_tags: readings
#+options: toc:nil
#+title: ${title}
" :unnarrowed t)
      ("t" "Topic" plain #'org-roam-capture--get-point "%?" :file-name "%<%Y%m%d%H%M%S>-${slug}" :head "#+roam_tags: topic
#+title: ${title}
" :unnarrowed t))
     (org-roam-db-location . "~/Google Drive/OHS/12th Grade/Classes/OCRA1/notes/org-roam.db")
     (org-roam-directory . "~/Google Drive/OHS/12th Grade/Classes/OCRA1/notes/")))
 '(sml/pre-modes-separator (propertize " " 'face 'sml/modes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
