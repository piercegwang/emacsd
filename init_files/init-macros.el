;;; init-macros.el --- Set up macros
;;; Commentary:

;; My macros

(fset 'setupworkspace
   [?\C-c ?a ?a ?\C-x ?0 M-f10 ?\C-x ?3 ?\H-l ?\H-\C-x ?o ?\H-h ?\C-x ?2 ?\H-c ?o S-tab S-tab ?\H-j ?\H-c ?i S-tab S-tab ?\H-l])
(global-set-key (kbd "C-x C-k 1") 'setupworkspace)

;;(fset 'OHSFigureSave
;;   [?# ?+ ?C ?A ?P ?T ?I ?O ?N ?: ?  ?\C-x ?Q return return tab ?\[ ?\[ ?f ?i ?l ?e ?: ?. ?/ ?W ?e ?e ?k ?  ?\C-x ?Q return ?/ ?\C-x ?Q return ?_ ?\C-u ?\M-! ?d ?a ?t ?e ?  ?+ ?% ?H ?% ?M ?% ?S return escape ?e ?a ?. ?p ?n ?g escape ?v ?B ?F ?/ ?l ?y escape ?A ?\] ?\] return escape ?p ?0 ?i ?\M-x ?i ?n ?s ?e ?r ?t ?d ?i ?r ?e ?c ?t ?o ?r ?y return escape ?V ?d ?i ?\C-x ?\C-f ?\C-  ?\C-a backspace ?/ ?U ?s ?e ?r ?s ?/ ?p ?i ?e ?r ?c ?e ?w ?a ?n ?g ?/ ?S ?c ?r ?e ?e ?n ?s ?h ?o ?t ?s return ?s ?\M-< ?\C-z ?/ ?S ?c ?r ?e ?e ?n ?  ?S ?h ?o ?t return ?R ?\C-  ?\C-a backspace ?\s-v backspace return ?\C-x ?k return])
;;(global-set-key (kbd "<f9>") 'OHSFigureSave)

(defun pgwang/dired-screenshots ()
  "Dired Screenshots"
  (interactive)
  (dired "/Users/piercewang/Screenshots"))
(defun pgwang/disable-helm ()
  "Disable Helm"
  (interactive)
  (helm-mode 0))
(defun pgwang/enable-helm ()
  "Enable Helm"
  (interactive)
  (helm-mode))
(global-set-key (kbd "H-m H-s") 'pgwang/dired-screenshots)
(global-set-key (kbd "H-x H-h d") 'pgwang/disable-helm)
(global-set-key (kbd "H-x H-h e") 'pgwang/enable-helm)
(fset 'OHSFigureSave
   [?\H-x ?\H-h ?d ?\[ ?\[ ?f ?i ?l ?e ?: ?. ?/ ?W ?e ?e ?k ?  ?\C-x ?Q return ?/ ?\C-x ?Q return ?_ ?\C-u ?\M-! ?d ?a ?t ?e ?  ?+ ?% ?H ?% ?M ?% ?S return escape ?e ?a ?. ?p ?n ?g escape ?v ?B ?F ?/ ?l ?y escape ?A ?\] ?\] return escape ?p ?0 ?i ?\M-x ?i ?n ?s ?e ?r ?t ?d ?i ?r ?e ?c ?t ?o ?r ?y return escape ?V ?d ?i ?\H-m ?\H-s ?s ?\M-< ?\C-z ?\C-s ?S ?c ?r ?e ?e ?n ?  ?S ?h ?o ?t return ?R ?\C-  ?\C-a backspace ?\s-v backspace return ?\C-x ?k return tab ?\H-x ?\H-h ?e])
(global-set-key (kbd "<f8>") 'OHSFigureSave)

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
(global-set-key (kbd "<f7>") 'getLink)

(provide 'init-macros)

;; init-macros.el ends here
