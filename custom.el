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
   '("730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "7b3ce93a17ce4fc6389bba8ecb9fee9a1e4e01027a5f3532cc47d160fe303d5a" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "b9e9ba5aeedcc5ba8be99f1cc9301f6679912910ff92fdf7980929c2fc83ab4d" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(debug-on-error nil)
 '(default-input-method "TeX")
 '(elpy-rpc-python-command "python3")
 '(epg-gpg-program "/usr/bin/gpg")
 '(erc-modules
   '(autojoin button completion fill irccontrols list log match menu move-to-prompt netsplit networks noncommands readonly ring stamp track))
 '(fci-rule-color "#BA45A3")
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
 '(jdee-db-active-breakpoint-face-colors (cons "#131033" "#1ea8fc"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#131033" "#a7da1e"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#131033" "#546A90"))
 '(objed-cursor-color "#e61f44")
 '(org-agenda-files
   '("~/Dropbox/org/classes.org" "~/Dropbox/org/events.org" "~/Dropbox/org/inbox.org" "~/Dropbox/org/init.org" "~/Dropbox/org/journal_temp.org" "~/Dropbox/org/links.org" "~/Dropbox/org/music.org" "~/Dropbox/org/notes.org" "~/Dropbox/org/ohs_gcal.org" "~/Dropbox/org/packing.org" "~/Dropbox/org/people.org" "~/Dropbox/org/projects.org" "~/Dropbox/org/random_thoughts.org" "~/Dropbox/org/school.org" "~/Dropbox/org/someday.org" "~/Dropbox/org/tickler.org" "~/Dropbox/org/violin.org" "~/Dropbox/org/calendars/a_event.org" "~/Dropbox/org/calendars/cal_emacs.org" "~/Dropbox/org/calendars/cal_gmail.org" "~/Dropbox/org/calendars/cal_music.org" "~/Dropbox/org/calendars/cal_sfcm.org" "~/Dropbox/org-roam/20210212150510-pixestra_meeting.org" "~/Dropbox/org-roam/20210213122902-alan_gilbert_programming_and_music.org" "~/Dropbox/org-roam/20210214105125-resonate_church.org" "~/Dropbox/org-roam/20210216184100-conducting_lesson.org" "~/Dropbox/org-roam/20210223095759-mouse_keys_mode_v4_karabiner.org" "~/Dropbox/org-roam/20210225173653-conducting_lesson_20210225.org" "~/Dropbox/org-roam/20210226100629-peter_van_inwagen_talk.org" "~/Dropbox/org-roam/20210227095446-installing_emacs_on_linux_mint.org" "~/Dropbox/org-roam/20210228104427-act_preparation_course.org" "~/Dropbox/org-roam/20210228104553-asus_wireless_card.org" "~/Dropbox/org-roam/20210301072514-broken_picturephone.org" "~/Dropbox/org-roam/20210302134933-bar_form_presentation.org" "~/Dropbox/org-roam/20210303235518-sfcm_benefit_concert_performers.org" "~/Dropbox/org-roam/20210117102352-sermon_20210117.org.gpg" "~/Dropbox/org-roam/20210117102753-resonate_sermon_series_shook.org.gpg" "~/Dropbox/org-roam/20210118185730-codesign_gdb_fix_for_macos.org.gpg" "~/Dropbox/org-roam/20210124102508-sermon_acts_5_17_32_40_42.org.gpg" "~/Dropbox/org-roam/20210207123911-curtis_practice_plan.org.gpg" "~/Dropbox/org-roam/20210207124510-curtis_audition_best_practices_webinar.org.gpg" "~/Dropbox/org-roam/20210208162808-minecraft_server.org.gpg" "~/Dropbox/org-roam/20210209174531-conducting_lesson.org.gpg" "~/Dropbox/org-roam/20210209184429-conducting.org.gpg" "~/Dropbox/org-roam/20210209184447-stefano_flavoni.org.gpg" "~/Dropbox/org-roam/20210210090304-rice_college_visit.org.gpg" "~/Dropbox/org-roam/20210210090330-college_visits_ohs.org.gpg" "~/Dropbox/org-roam/20210210090439-breadth_and_depth_macalaster.org.gpg" "~/Dropbox/org-roam/20210210090626-bu_tanglewood_institute_and_from_the_top_audition_workshop_and_panel_discussion.org.gpg" "~/Dropbox/org-roam/20210210090840-emacs.org.gpg" "~/Dropbox/org-roam/20210210091401-emacs_calc.org.gpg" "~/Dropbox/org-roam/20210210091543-calc_unit_table.org.gpg" "~/Dropbox/org-roam/20210210091637-calling_calc_from_programs.org.gpg" "~/Dropbox/org-roam/20210210091722-wharton_investment_competition.org.gpg" "~/Dropbox/org-roam/20210210091749-wharton_stock_interest_tracking.org.gpg" "~/Dropbox/org-roam/20210210091942-wharton_meeting.org.gpg" "~/Dropbox/org-roam/20210210092024-wharton_important_dates.org.gpg" "~/Dropbox/org-roam/20210210092049-wharton_sector_research.org.gpg" "~/Dropbox/org-roam/20210210092159-wharton_meeting_20200915.org.gpg" "~/Dropbox/org-roam/20210210092300-wharton_meeting_20200922.org.gpg" "~/Dropbox/org-roam/20210210092335-wharton_stocktrak_login_info.org.gpg" "~/Dropbox/org-roam/20210210092439-wharton_meeting_20200929.org.gpg" "~/Dropbox/org-roam/20210210092627-wharton_meeting_20201006.org.gpg" "~/Dropbox/org-roam/20210210092725-college.org.gpg" "~/Dropbox/org-roam/20210210092753-harvard_notes.org.gpg" "~/Dropbox/org-roam/20210210092822-nec.org.gpg" "~/Dropbox/org-roam/20210210092904-ucla_herb_alpert_school_of_music_application_information.org.gpg" "~/Dropbox/org-roam/20210210093024-listen4life.org.gpg" "~/Dropbox/org-roam/20210210093050-listen4life_meeting_20201023.org.gpg" "~/Dropbox/org-roam/20210210093213-mozilla_hubs_rooms.org.gpg" "~/Dropbox/org-roam/20210210093232-mozilla_hubs.org.gpg" "~/Dropbox/org-roam/20210210093515-checkin_with_dr_lips_20201109.org.gpg" "~/Dropbox/org-roam/20210210093710-trick_to_writing_from_mom.org.gpg" "~/Dropbox/org-roam/20210210093722-writing.org.gpg" "~/Dropbox/org-roam/20210210093841-conducting_lesson_20210202.org.gpg" "~/Dropbox/org-roam/20210210094002-stanford_online_high_school.org.gpg" "~/Dropbox/org-roam/20210210094112-bible_study_call_20210123.org.gpg" "~/Dropbox/org-roam/20210210095006-violin.org.gpg" "~/Dropbox/org-roam/20210210095015-back_to_bach.org.gpg" "~/Dropbox/org-roam/20210210095038-btb_meeting_20190807.org.gpg" "~/Dropbox/org-roam/20210210095105-t_shirts_10_per.org.gpg" "~/Dropbox/org-roam/20210210095130-back_to_bach_meeting_with_josie_20200905.org.gpg" "~/Dropbox/org-roam/20210210095206-btb_region_leadershipt_meeting.org.gpg" "~/Dropbox/org-roam/20210210095247-expenses.org.gpg" "~/Dropbox/org-roam/20210210095312-btb_meeting_with_josie_20201001.org.gpg" "~/Dropbox/org-roam/20210210095340-possible_orchestra_to_join.org.gpg" "~/Dropbox/org-roam/20210210095434-dounis_collection.org.gpg" "~/Dropbox/org-roam/20210210095454-shifting_tip.org.gpg" "~/Dropbox/org-roam/20210210095544-clarity_in_soft_passages.org.gpg" "~/Dropbox/org-roam/20210210095612-philosophy.org.gpg" "~/Dropbox/org-roam/20210210095620-faith.org.gpg" "~/Dropbox/org-roam/20210210095628-christianity.org.gpg" "~/Dropbox/org-roam/20210210095712-ravi_zacharias_on_christianity_and_determinism.org.gpg" "~/Dropbox/org-roam/20210210095733-determinism.org.gpg" "~/Dropbox/org-roam/20210210095933-sermon_20201115.org.gpg" "~/Dropbox/org-roam/20210210100216-vim.org.gpg" "~/Dropbox/org-roam/20210210100340-java.org.gpg" "~/Dropbox/org-roam/20210210100401-java_in_emacs_using_lsp.org.gpg" "~/Dropbox/org-roam/20210210100520-compile_java_files_with_org_junit_tests.org.gpg" "~/Dropbox/org-roam/20210210100542-terminal.org.gpg" "~/Dropbox/org-roam/20210210100633-fafsa.org.gpg" "~/Dropbox/org-roam/20210210100649-finance.org.gpg" "~/Dropbox/org-roam/20210210100718-fafsa_confirmation_page_email.org.gpg" "~/Dropbox/org-roam/20210210101001-sfcm_piano_quintet_2020_21.org.gpg" "~/Dropbox/org-roam/20210210101052-stocks_investing.org.gpg" "~/Dropbox/org-roam/20210210101105-stock_options_call_put.org.gpg" "~/Dropbox/org-roam/20210210101210-buying_gold_info.org.gpg" "~/Dropbox/org-roam/20210210101239-stock_ideas_from_dad.org.gpg" "~/Dropbox/org-roam/20210213123010-what_s_not_there_christian_reif.org.gpg" "~/Dropbox/org-roam/20210214105059-sermon_underestimated.org.gpg" "~/Dropbox/org-roam/20210228103625-linux.org.gpg" "~/Dropbox/org-roam/20210314150407-sfcm_benefit_concert_audio_settings_guide.org.gpg" "~/Dropbox/org-roam/20210316151543-omnibus_progressions.org.gpg" "~/Dropbox/org-roam/20210325005234-friendship_lamp_color_meanings.org.gpg" "~/Dropbox/org-roam/20210327160345-fishing_in_summer.org.gpg" "~/Dropbox/org-roam/20210330160322-brahms_symphonies.org.gpg" "~/Dropbox/org-roam/20210403093601-conducting_homework.org.gpg" "~/Dropbox/org-roam/20210403093821-college_decision_deliberation.org.gpg" "~/Dropbox/org-roam/20210405162907-questions_about_columbia_and_columbia_juilliard.org.gpg" "~/Dropbox/org-roam/20210412101743-amazon_gift_cards.org.gpg" "~/Dropbox/org-roam/20210412110345-columbia_general_info_sheet_for_new_students.org.gpg" "~/Dropbox/org-roam/20210412113817-book_review.org.gpg" "~/Dropbox/org-roam/20210412170501-politics_economics_and_pandemics_a_pep_talk_for_the_class_of_2025_with_economics_senior_lecturer_sunil_gulati.org.gpg"))
 '(org-modules
   '(org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m org-drill org-learn))
 '(package-selected-packages
   '(org-roam lsp-treemacs dap-mode yasnippet rotate 2048-game bufler powerline ox-reveal modus-operandi-theme lsp-ivy lsp-java which-key lsp-ui lsp-mode flycheck projectile company-jedi jedi company counsel "ivy" ivy gnuplot all-the-icons mu4e-alert ztree org-superstar treemacs-magit treemacs-evil treemacs tramp org-gcal json-mode org-mode use-package cdlatex org-noter artist-mode python-docstring markdown-mode epa-file framemove doom-themes htmlize lua-mode magit elpy exec-path-from-shell smart-mode-line latex auctex evil-visual-mark-mode))
 '(python-shell-interpreter "python3")
 '(rustic-ansi-faces
   ["#0c0a20" "#e61f44" "#a7da1e" "#ffd400" "#1ea8fc" "#ff2afc" "#42c6ff" "#f2f3f7"])
 '(safe-local-variable-values
   '((org-roam-dailies-capture-templates
      ("c" "Class" plain #'org-roam-capture--get-point "%?" :file-name "daily/%<%Y%m%d%H%M%S>-${slug}" :head "#+setupfile: ~/.emacs.d/org-mode/templates/general/note.org
#+setupfile: ~/.emacs.d/org-mode/templates/school/name_email.org
#+setupfile: ~/.emacs.d/org-mode/templates/school/writing_setup.org
#+setupfile: ~/.emacs.d/org-mode/templates/general/web.org
#+roam_tags: discussion
#+options: toc:nil
#+title: ${title}
" :unnarrowed t)
      ("o" "Office Hours" plain #'org-roam-capture--get-point "%?" :file-name "%<%Y%m%d%H%M%S>-${slug}" :head "#+setupfile: ~/.emacs.d/org-mode/templates/general/note.org
#+setupfile: ~/.emacs.d/org-mode/templates/school/name_email.org
#+setupfile: ~/.emacs.d/org-mode/templates/school/writing_setup.org
#+setupfile: ~/.emacs.d/org-mode/templates/general/web.org
#+roam_tags: office_hours
#+options: toc:nil
#+title: ${title}
" :unnarrowed t))
     (org-roam-capture-templates
      ("d" "Default" plain #'org-roam-capture--get-point "%?" :file-name "%<%Y%m%d%H%M%S>-${slug}" :head "#+setupfile: ~/.emacs.d/org-mode/templates/general/note.org
#+setupfile: ~/.emacs.d/org-mode/templates/school/name_email.org
#+setupfile: ~/.emacs.d/org-mode/templates/school/writing_setup.org
#+setupfile: ~/.emacs.d/org-mode/templates/general/web.org
#+roam_tags: 
#+options: toc:nil
#+title: ${title}
" :unnarrowed t)
      ("r" "Readings" plain #'org-roam-capture--get-point "%?" :file-name "%<%Y%m%d%H%M%S>-${slug}" :head "#+setupfile: ~/.emacs.d/org-mode/templates/general/note.org
#+setupfile: ~/.emacs.d/org-mode/templates/school/name_email.org
#+setupfile: ~/.emacs.d/org-mode/templates/school/writing_setup.org
#+setupfile: ~/.emacs.d/org-mode/templates/general/web.org
#+roam_tags: reading
#+options: toc:nil
#+title: ${title}
" :unnarrowed t)
      ("t" "Topic" plain #'org-roam-capture--get-point "%?" :file-name "daily/%<%Y%m%d%H%M%S>-${slug}" :head "#+roam_tags: topic
#+title: ${title}
" :unnarrowed t))
     (org-roam-db-location . "~/Documents/OHS/12th Grade/Classes/OCRA1/projects/research_paper/org-roam.db")
     (org-roam-directory . "~/Documents/OHS/12th Grade/Classes/OCRA1/projects/research_paper/")
     (org-roam-dailies-capture-templates
      ("c" "Class" plain #'org-roam-capture--get-point "%?" :file-name "daily/%<%Y%m%d%H%M%S>-${slug}" :head "#+setupfile: ~/.emacs.d/org-mode/templates/general/note.org
#+setupfile: ~/.emacs.d/org-mode/templates/school/name_email.org
#+setupfile: ~/.emacs.d/org-mode/templates/school/writing_setup.org
#+setupfile: ~/.emacs.d/org-mode/templates/general/web.org
#+roam_tags: discussion
#+options: toc:nil
#+title: ${title}
" :unnarrowed t)
      ("o" "Office Hours" plain #'org-roam-capture--get-point "%?" :file-name "daily/%<%Y%m%d%H%M%S>-${slug}" :head "#+setupfile: ~/.emacs.d/org-mode/templates/general/note.org
#+setupfile: ~/.emacs.d/org-mode/templates/school/name_email.org
#+setupfile: ~/.emacs.d/org-mode/templates/school/writing_setup.org
#+setupfile: ~/.emacs.d/org-mode/templates/general/web.org
#+roam_tags: office_hours
#+options: toc:nil
#+title: ${title}
" :unnarrowed t))
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
     (org-roam-db-location . "~/Documents/OHS/12th Grade/Classes/OCRA1/notes/org-roam.db")
     (org-roam-directory . "~/Documents/OHS/12th Grade/Classes/OCRA1/notes/")
     (org-roam-db-location . "~/Google Drive/OHS/12th Grade/Classes/OCRA1/notes/org-roam.db")
     (org-roam-directory . "~/Google Drive/OHS/12th Grade/Classes/OCRA1/notes/")
     (org-roam-encrypt-files)
     (org-roam-db-update-method . immediate)
     (org-roam-dailies-capture-templates
      ("c" "Class" plain #'org-roam-capture--get-point "%?" :file-name "daily/%<%Y%m%d%H%M%S>-${slug}" :head "#+setupfile: /Users/piercewang/Dropbox/org/templates/general/note.org
#+setupfile: /Users/piercewang/Dropbox/org/templates/school/name_email.org
#+setupfile: /Users/piercewang/Dropbox/org/templates/school/writing_setup.org
#+setupfile: /Users/piercewang/Dropbox/org/templates/general/web.org
#+roam_tags: discussion
#+options: toc:nil
#+title: ${title}
" :unnarrowed t)
      ("o" "Office Hours" plain #'org-roam-capture--get-point "%?" :file-name "daily/%<%Y%m%d%H%M%S>-${slug}" :head "#+setupfile: /Users/piercewang/Dropbox/org/templates/general/note.org
#+setupfile: /Users/piercewang/Dropbox/org/templates/school/name_email.org
#+setupfile: /Users/piercewang/Dropbox/org/templates/school/writing_setup.org
#+setupfile: /Users/piercewang/Dropbox/org/templates/general/web.org
#+roam_tags: office_hours
#+options: toc:nil
#+title: ${title}
" :unnarrowed t))
     (org-roam-capture-templates
      ("r" "Readings" plain #'org-roam-capture--get-point "%?" :file-name "%<%Y%m%d%H%M%S>-${slug}" :head "#+setupfile: /Users/piercewang/Dropbox/org/templates/general/note.org
#+setupfile: /Users/piercewang/Dropbox/org/templates/school/name_email.org
#+setupfile: /Users/piercewang/Dropbox/org/templates/school/writing_setup.org
#+setupfile: /Users/piercewang/Dropbox/org/templates/general/web.org
#+roam_tags: readings
#+options: toc:nil
#+title: ${title}
" :unnarrowed t)
      ("t" "Topic" plain #'org-roam-capture--get-point "%?" :file-name "%<%Y%m%d%H%M%S>-${slug}" :head "#+roam_tags: topic
#+title: ${title}
" :unnarrowed t))
     (org-roam-db-location . "/Users/piercewang/Google Drive/OHS/12th Grade/Classes/OCRA1/notes/org-roam.db")
     (org-roam-directory . "/Users/piercewang/Google Drive/OHS/12th Grade/Classes/OCRA1/notes/")))
 '(show-paren-mode nil)
 '(sml/pre-modes-separator (propertize " " 'face 'sml/modes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-current-match ((t (:extend t :background "gray21"))))
 '(line-number ((t (:inherit default :foreground "gray40" :strike-through nil :underline nil :slant normal :weight normal))))
 '(org-habit-alert-face ((t (:foreground "#f3efde"))))
 '(org-habit-overdue-face ((t (:foreground "#f0dde5"))))
 '(org-habit-ready-face ((t (:foreground "#dcebf7"))))
 '(org-time-grid ((t (:foreground "#f9e062")))))
