;; -*- lexical-binding: t; -*-
;;. Emacs settings
;; General convenient settings for emacs

;;.. Tidy up initial messages
(use-package emacs
  :init
  (setq initial-scratch-message nil)
  ; disable the splash screen
  (setq inhibit-startup-message t)
  (setq ring-bell-function 'ignore)
  (defun display-startup-echo-area-message ()
    (message "")))

;; (push '(fullscreen . maximized) default-frame-alist)
(add-to-list 'default-frame-alist '(fullscreen . maximized)) 
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(font . "Roboto Mono 12")) 

;;.. General convenience
(use-package emacs
  :init
  (defalias 'yes-or-no-p 'y-or-n-p)
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit))
(savehist-mode t)
;; Activate recent file mode, and save every ten minutes.
(recentf-mode t)
(run-at-time nil 600 'recentf-save-list)

;;.. Backup files
(setq backup-directory-alist `(("." . "~/.emacs_backups"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      auto-save-default t
      auto-save-timeout 20
      auto-save-interval 200)

;;.. Locale / UTF8
(use-package emacs
  :init
  (set-charset-priority 'unicode)
  (setq locale-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix)))

;;.. Tabs vs Spaces
(use-package emacs
  :init
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2))
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;;.. MacOS compatibility
;; MacOS keybinds
(use-package emacs
  :init
  (when (eq system-type 'darwin)
    (setq mac-command-modifier 'super)
    (setq mac-option-modifier 'meta)
    (setq mac-control-modifier 'control)
    (setq ns-right-alternate-modifier (quote none))))

;;. Straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)

(straight-use-package 'use-package)
(straight-use-package 'org)
(setq straight-use-package-by-default t)
(setq use-package-always-defer t)


;;. Org mode
(use-package org-roam
  :straight t)

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(use-package citar
  :bind (("C-c b" . citar-insert-citation)
         :map minibuffer-local-map
         ("M-b" . citar-insert-preset))
  :custom
  (citar-bibliography '("~/bib/paperpile.bib")))

(use-package org-journal)

;; My Org Roam setup I was using before with Doom
(setq org-directory "~/org/")
(setq org-roam-directory "~/org/roam/")
(setq org-journal-dir "~/org/journal")
(setq org-journal-file-type 'monthly)
(setq denote-directory "~/org/denote")
(setq org-agenda-files '("~/org/agenda"))

;; Appearance in org-mode - setting up and handling mixed fonts
;; Improve org mode looks
(setq org-startup-indented t
  org-pretty-entities t
  org-hide-emphasis-markers t
  org-startup-with-inline-images t
  org-image-actual-width '(300))

;; Increase size of LaTeX fragment previews
;; (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

(add-hook 'org-mode-hook
  (lambda ()
    (setq line-spacing 6)))

(add-hook 'org-mode-hook
           #'visual-line-mode)
           ; #'variable-pitch-mode)

;; Org Roam capture template
(setq org-roam-capture-templates '(("d" "default" plain "%?"
                                    :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                                       "#+title: ${title}\n")
                                    :unnarrowed t)))
(add-to-list 'org-roam-capture-templates
             '("e" "encrypted" plain "%?"
               :target (file+head "${slug}.org.gpg"
                                  "#+title: ${title}\n")
               :unnarrowed t))
(add-to-list 'org-roam-capture-templates
               '("l" "latex-ready" plain "%?" :target
                  (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+LATEX_CLASS_OPTIONS: [12pt]
#+LATEX_HEADER: \\PassOptionsToPackage{style=nature}{biblatex}
#+LATEX_HEADER: \\usepackage[natbib=true,backend=biber]{biblatex} \\addbibresource{~/bib/paperpile.bib}
#+LATEX_HEADER: \\hypersetup{colorlinks,linkcolor=red,citecolor=blue,urlcolor=blue}
#+LATEX_HEADER: \\usepackage{parskip}
#+LATEX_HEADER: \\usepackage{charter} \\renewcommand\\familydefault{bch}
#+LATEX_HEADER: \\usepackage[margin=1.2in]{geometry}
#+title: ${title}
#+filetags: %^g
")
                 :unnarrowed t))

;; Add file tag properties to vertico display and search
(setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:50}" 'face 'org-tag)))

;; Set calendar start of the week to Monday
(setq calendar-week-start-day 1)

; (add-hook 'org-mode-hook #'mixed-pitch-mode)
; (add-hook 'org-mode-hook #'solaire-mode)

; (add-hook 'mixed-pitch-mode-hook #'solaire-mode-reset)

(setq org-html-head-extra "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://gongzhitaao.org/orgcss/org.css\"/>")
(setq org-html-head-include-default-style nil)

;; Select languages to activate in org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)))


;;.. Export via pandoc
(use-package ox-pandoc
  :after org
  :ensure t)

;;. Outlining

;;.. outli
(use-package outli
  :straight (:host github :repo "jdtsmith/outli")
  ;; :after lispy ; uncomment only if you use lispy; it also sets speed keys on headers!
  :hook ((prog-mode text-mode conf-space-mode
                    conf-xdefaults-mode conf-unix-mode
                    tex-mode yaml-mode) . outli-mode)
  :config
  ;; Adjust only the emacs-lisp-mode entry in outli-heading-config
  (setf (alist-get 'emacs-lisp-mode outli-heading-config nil nil 'equal)
        '(";;" ?. t)))


;;.. outline
(use-package outline
  :bind (:map outline-minor-mode-map
          ;; Outline previous and next visible heading
          ("M-p" . outline-previous-visible-heading)
          ("M-n" . outline-next-visible-heading)
          ;; Outline previous and next visible heading on same level
          ("s-p" . outline-backward-same-level)
          ("s-n" . outline-forward-same-level)
          ;; Outline move subtree up or down
          ("M-s-<up>" . outline-move-subtree-up)
          ("M-s-<down>" . outline-move-subtree-down)
          ;; Outline collapse/expand subtree
          ("C-c C-d" . outline-hide-subtree)
          ("C-c C-s" . outline-show-subtree)))


;;. Evil mode
;; Remember, can toggle evil mode with C-z
(use-package evil
  :demand q; no lazy loading
  :init (setq evil-want-keybinding nil)
  :config (evil-mode 1))

;; evil-collection: Expands evil mode into subsystems, e.g. M-x calendar
(use-package evil-collection
  :after (evil)
  :demand
  :config
  (evil-collection-init))

;; evil-lion: For aligning text in columns
;; gl MOTION CHAR (left align selection within 'motion' using 'char' as anchor)
;; gL MOTION CHAR (right align...)
(use-package evil-lion
  :ensure t
  :config
  (evil-lion-mode))

(evil-set-undo-system 'undo-redo)

;;.. Fix the insert cursor in the terminal in evil mode
(unless window-system
  (progn (add-hook 'evil-insert-state-entry-hook (lambda () (send-string-to-terminal "\033[5 q")))
         (add-hook 'evil-insert-state-exit-hook  (lambda () (send-string-to-terminal "\033[2 q")))))

;;. Treemacs
;; This is a full-featured example config from the github readme. Most of these config options
;; are defaults.
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-nerd-icons
  :after (treemacs nerd-icons)
  :ensure t
  :config
  (treemacs-load-theme "nerd-icons"))
;; (use-package treemacs-projectile
;;   :after (treemacs projectile)
;;   :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

;; (use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
;;   :after (treemacs persp-mode) ;;or perspective vs. persp-mode
;;   :ensure t
;;   :config (treemacs-set-scope-type 'Perspectives))

;; (use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
;;   :after (treemacs)
;;   :ensure t
;;   :config (treemacs-set-scope-type 'Tabs))


;;. Theme
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-peacock t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package ef-themes
  :ensure t)

;; (setq cycle-themes-theme-list
;;               '(doom-material
;;                 doom-material-dark
;;                 ef-arbutus
;;                 ef-cyprus
;;                 ef-day
;;                 ef-melissa-dark
;;                 ef-melissa-light
;;                 ef-rosa
;;                 ef-spring
;;                 ef-summer
;;                 doom-Iosvkem
;;                 doom-ayu-dark
;;                 doom-ayu-light
;;                 doom-ayu-mirage
;;                 doom-bluloco-light
;;                 doom-tokyo-night
;;                 doom-flatwhite
;;                 doom-laserwave
;;                 doom-monokai-pro
;;                 doom-nord-aurora
;;                 doom-nord-light
;;                 doom-nord
;;                 doom-oceanic-next
;;                 doom-one
;;                 doom-one-light
;;                 doom-opera-light
;;                 doom-peacock
;;                 doom-plain-dark
;;                 doom-plain
;;                 doom-rouge
;;                 doom-snazzy
;;                 doom-sourcerer
;;                 doom-spacegrey
;;                 misterioso
;;                 modus-operandi
;;                 modus-operandi-tinted
;;                 modus-vivendi
;;                 modus-vivendi-tinted))

(defun load-themes (theme-list)
  "Function to load themes from a theme-list."
  (dolist (theme theme-list)
    (load-theme theme t t)))

;;(load-themes cycle-themes-theme-list)
;; Prefer brighter comments
(setq doom-peacock-brighter-comments t
      doom-peacock-comment-bg nil)
(load-themes '(doom-peacock))

(use-package catppuccin-theme
  :ensure t)
;; (load-theme 'catppuccin :no-confirm)
;; (setq catppuccin-flavor 'macchiato) ;; 'latte, 'frappe, 'macchiato or 'mocha
;; (catppuccin-reload)
;; (set-frame-font "Roboto Mono 12" nil t)


(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))


;;. Nerd icons
(use-package nerd-icons)

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :after marginalia
  :config (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))


;;. All the Icons
(use-package all-the-icons
  :if (display-graphic-p)
  :config
  (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts t)))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))


;;. Key bindings and leader keys
;; The General package provides a nice way to define keybindings
;; Here I use it to set up a leader key like in Doom emacs
;; Also I set up which-key, which provides a popup to help complete partially entered key commands
(use-package general
  :demand
  :config
  (general-evil-setup)

  (general-create-definer leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-define-key
   :keymaps 'ess-r-mode-map
   "C-c C-d" 'ess-eval-line-and-step)

  (leader-keys
    "x" '(execute-extended-command :which-key "execute command")
    "r" '(restart-emacs :which-key "restart emacs")
    "i" '((lambda () (interactive) (find-file user-init-file)) :which-key "open init file")
    "s" 'consult-line
    "f r" 'consult-recent-file
    "f f" 'find-file
    "c" 'consult-flymake

    ;; Buffer
    "b" '(:ignore t :which-key "buffer")
    ;; Don't show an error because SPC b ESC is undefined, just abort
    "b <escape>" '(keyboard-escape-quit :which-key t)
    "bd"  'kill-current-buffer
    
    ;; Comment line
    "/" '((lambda (n) (interactive "p") (save-excursion (comment-line n))) :which-key "comment line")))
  

;; Which key, shows what keys do
(use-package which-key
  :demand
  :init
  (setq which-key-idle-delay 0.5) ; Open after .5s instead of 1s
  :config
  (which-key-mode))

;;.. Window switching with ace-window
(evil-define-key 'normal global-map (kbd "C-p") 'ace-window)
(evil-define-key 'visual global-map (kbd "C-p") 'ace-window)
(evil-define-key 'insert global-map (kbd "C-p") 'ace-window)

(defun my-vterm-mode-setup ()
  "Set up keybindings to work in vterm mode"
  (evil-define-key 'normal vterm-mode-map (kbd "C-p") 'ace-window)
  (evil-define-key 'visual vterm-mode-map (kbd "C-p") 'ace-window)
  (evil-define-key 'insert vterm-mode-map (kbd "C-p") 'ace-window)
  (define-key vterm-mode-map (kbd "C-p") 'ace-window))

(add-hook 'vterm-mode-hook 'my-vterm-mode-setup)
(global-set-key (kbd "C-p") 'ace-window)
  

;;. Minibuffer
;;.. Marginalia
(use-package marginalia
  :general
  (:keymaps 'minibuffer-local-map
   "M-A" 'marginalia-cycle)
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'left)
  
  :init
  (marginalia-mode))

;;.. Vertico
(use-package vertico
  :demand t                             ; Otherwise won't get loaded immediately
  :straight (vertico :files (:defaults "extensions/*") ; Special recipe to load extensions conveniently
                     :includes (vertico-indexed
                                vertico-flat
                                vertico-grid
                                vertico-mouse
                                vertico-quick
                                vertico-buffer
                                vertico-repeat
                                vertico-reverse
                                vertico-directory
                                vertico-multiform
                                vertico-unobtrusive))
                                
  :general
  (:keymaps '(normal insert visual motion)
   "M-." #'vertico-repeat)
   
  (:keymaps 'vertico-map
   "<tab>" #'vertico-insert ; Set manually otherwise setting `vertico-quick-insert' overrides this
   "?" #'minibuffer-completion-help
   "C-M-n" #'vertico-next-group
   "C-M-p" #'vertico-previous-group
   ;; Multiform toggles
   "<backspace>" #'vertico-directory-delete-char
   "C-w" #'vertico-directory-delete-word
   "C-<backspace>" #'vertico-directory-delete-word
   "RET" #'vertico-directory-enter
   "C-i" #'vertico-quick-insert
   "C-o" #'vertico-quick-exit
   "M-o" #'kb/vertico-quick-embark
   "M-G" #'vertico-multiform-grid
   "M-F" #'vertico-multiform-flat
   "M-R" #'vertico-multiform-reverse
   "M-U" #'vertico-multiform-unobtrusive
   "C-l" #'kb/vertico-multiform-flat-toggle)
   
  :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy) ; Clean up file path when typing
         (minibuffer-setup . vertico-repeat-save)) ; Make sure vertico state is saved
         
  :custom
  (vertico-count 10)
  (vertico-resize nil)
  (vertico-cycle nil)
  ;; Extensions
  (vertico-grid-separator "       ")
  (vertico-grid-lookahead 50)
  (vertico-buffer-display-action '(display-buffer-reuse-window))
  (vertico-multiform-categories
   '((file indexed)
     (consult-grep buffer)
     (consult-location)
     (imenu buffer)
     (library  indexed)
     (org-roam-node indexed)
     (t reverse)))
     
  (vertico-multiform-commands
   '(("flyspell-correct-*" grid reverse)
     (org-refile grid reverse indexed)
     (consult-yank-pop indexed)
     (consult-flycheck)
     (consult-lsp-diagnostics)))
     
  :init
  (defun kb/vertico-multiform-flat-toggle ()
    "Toggle between flat and reverse."
    (interactive)
    (vertico-multiform--display-toggle 'vertico-flat-mode)
    (if vertico-flat-mode
        (vertico-multiform--temporary-mode 'vertico-reverse-mode -1)
      (vertico-multiform--temporary-mode 'vertico-reverse-mode 1)))
  (defun kb/vertico-quick-embark (&optional arg)
    "Embark on candidate using quick keys."
    (interactive)
    (when (vertico-quick-jump)
      (embark-act arg)))

  ;; Workaround for problem with `tramp' hostname completions. This overrides
  ;; the completion style specifically for remote files! See
  ;; https://github.com/minad/vertico#tramp-hostname-completion
  (defun kb/basic-remote-try-completion (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-try-completion string table pred point)))
  (defun kb/basic-remote-all-completions (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-all-completions string table pred point)))
  (add-to-list 'completion-styles-alist
               '(basic-remote           ; Name of `completion-style'
                 kb/basic-remote-try-completion kb/basic-remote-all-completions nil))
  :config
  (vertico-mode)
  ;; Extensions
  (vertico-multiform-mode)

  ;; Prefix the current candidate with “» ”. From
  ;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
  (advice-add #'vertico--format-candidate :around
                                          (lambda (orig cand prefix suffix index _start)
                                            (setq cand (funcall orig cand prefix suffix index _start))
                                            (concat
                                             (if (= vertico--index index)
                                                 (propertize "» " 'face 'vertico-current)
                                               "  ")
                                             cand))))


;;.. Orderless
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;;.. Consult
(use-package consult)

;;. Corfu
;;.. Corfu
(use-package corfu
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-auto-prefix 2)          ;; Show auto completion automatically even if prefix is 2 chars
  ;; (corfu-auto-delay 0.1)         ;; Hide auto completion after 0.8 seconds
  ;; (corfu-echo-documentation 0.25) ;; Show documentation after 0.25 seconds
  (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (setq corfu-popupinfo-delay 0.2)
  (corfu-popupinfo-mode)
  (global-corfu-mode))

;;.. Use Dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode))

;;.. Cape
;; Add extensions
(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-elisp-symbol)
         ("C-c p e" . cape-elisp-block)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p :" . cape-emoji)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)


;;. Magit
;;.. diff-hl: Git diffs
(use-package diff-hl
  :init
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode))

;;.. Magit
(use-package magit
  :general
  (leader-keys
    "g" '(:ignore t :which-key "git")
    "g <escape>" '(keyboard-escape-quit :which-key t)
    "g g" '(magit-status :which-key "status")
    "g l" '(magit-log :which-key "log"))
  (general-nmap
    "<escape>" #'transient-quit-one))


;;. PDF tools
(use-package pdf-tools
  :ensure t
  :after (evil)
  :pin manual ;; manually update
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  ;; initialise
  (pdf-tools-install)
  ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-page)
  ;; automatically annotate highlights
  (setq pdf-annot-activate-created-annotations t))

(general-def pdf-view-mode-map
  "M-n" 'pdf-view-next-page
  "M-p" 'pdf-view-previous-page)

;; Open PDFs in emacs mode
(evil-set-initial-state 'pdf-view-mode 'emacs)
(add-hook 'pdf-view-mode-hook
  (lambda ()
    (set (make-local-variable 'evil-emacs-state-cursor) (list nil)))
  (blink-cursor-mode -1))


;;. Rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook
  (lisp-mode . rainbow-delimiters-mode))


;;. Parinfer
(use-package parinfer-rust-mode
  :hook ((emacs-lisp-mode . parinfer-rust-mode)
         (clojure-mode . parinfer-rust-mode)
         (common-lisp-mode . parinfer-rust-mode)
         (scheme-mode . parinfer-rust-mode)
         (lisp-mode . parinfer-rust-mode)
         (racket-mode . parinfer-rust-mode)
         (racket-repl-mode . parinfer-rust-mode))
  :init
  (setq parinfer-rust-auto-download nil)
  :config
  (define-key parinfer-rust-mode-map (kbd "C-c C-n") 'custom-parinfer/cycle-modes))

(defun custom-parinfer/cycle-modes ()
  "Cycles through the parinfer modes: paren -> indent -> smart (-> paren...)" 
    (interactive)
    (cond
     ((string= parinfer-rust--mode "paren")
      (message "Switching to indent mode")
      (parinfer-rust--switch-mode "indent"))
     ((string= parinfer-rust--mode "indent")
      (message "Switching to smart mode")
      (parinfer-rust--switch-mode "smart"))
     (t
      (message "Switching to paren mode")
      (parinfer-rust--switch-mode "paren"))))


;;. Smartparens
;;.. Smartparens setup
(defun my-enable-evil-move-beyond-eol ()
  (setq-local evil-move-beyond-eol t))

(defun my-disable-evil-move-beyond-eol ()
  (setq-local evil-move-beyond-eol nil))

(use-package smartparens
  :ensure t
  :defer t
  :init
  :bind (:map smartparens-mode-map
              ("C-M-f" . forward-sexp)
              ("C-M-b" . backward-sexp)
              ("C-M-e" . sp-up-sexp)
              ("C-M-d" . sp-down-sexp)
              ("C-M-u" . sp-backward-up-sexp)
              ("C-M-a" . sp-backward-down-sexp)
              ("C-M-t" . sp-transpose-sexp)
              ("C-M-s" . sp-splice-sexp)
              ("C-c (" . sp-wrap-round)
              ("C-c [" . sp-wrap-square)
              ("C-c {" . sp-wrap-curly)
              ("C-M-<backspace>" . sp-backward-kill-sexp)
              ("C-M-<delete>" . sp-kill-sexp)
              ("<M-backspace>" . sp-backward-unwrap-sexp)
              ("<M-delete>" . sp-unwrap-sexp)
              ("s-<right>" . sp-forward-slurp-sexp)
              ("s-<left>" . sp-forward-barf-sexp)
              ("M-<left>" . sp-backward-slurp-sexp)
              ("M-<right>" . sp-backward-barf-sexp))
  :config
  (add-hook 'smartparens-enabled-hook #'my-enable-evil-move-beyond-eol)
  (add-hook 'smartparens-disabled-hook #'my-disable-evil-move-beyond-eol)
  :hook
  (lisp-mode . smartparens-mode)
  (emacs-lisp-mode . smartparens-mode)
  (clojure-mode . smartparens-mode))

;; (use-package paredit
;;   :ensure t
;;   :defer t
;;   :init
;;   (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
;;   (add-hook 'clojure-mode-hook 'paredit-mode)
;;   :config
;;   (define-key paredit-mode-map (kbd "C-c k") 'paredit-copy-as-kill))


;;.. Custom Smartparens functions
(defun sp-custom/drag-preceding-sexp-backward ()
  "Moves the preceding form towards the start of the list.
   Place the point at the end of the form you want to move."
  (interactive)
  (transpose-sexps -1))

(defun sp-custom/drag-preceding-sexp-forward ()
  "Moves the preceding form towards the start of the list. (Same as transpose-sexps)
   Place the point at the end of the form you want to move."
  (interactive)
  (transpose-sexps 1))

(defun sp-custom/drag-sexp-forward ()
  "Moves the following form towards the end of the list.
   Place the point at the start of the form you want to move."
  (interactive)
  (forward-sexp)
  (transpose-sexps 1)
  (backward-sexp))

(defun sp-custom/drag-sexp-backward ()
  "Moves the following form towards the start of the list.
   Place the point at the start of the form you want to move."
  (interactive)
  (transpose-sexps 1)
  (backward-sexp 2))

(define-key smartparens-mode-map (kbd "C-M-t") 'sp-custom/drag-preceding-sexp-forward)
(define-key smartparens-mode-map (kbd "C-M-s-t") 'sp-custom/drag-preceding-sexp-backward)
(define-key smartparens-mode-map (kbd "C-M-y") 'sp-custom/drag-sexp-forward)
(define-key smartparens-mode-map (kbd "C-M-s-y") 'sp-custom/drag-sexp-backward)

;;. Lispy
;; (use-package lispy
;;   :ensure t
;;   :hook
;;   (emacs-lisp-mode . lispy-mode)
;;   (lisp-mode . lispy-mode)
;;   (scheme-mode . lispy-mode)
;;   (racket-mode . lispy-mode)
;;   (clojure-mode . lispy-mode)
;;   (clojurescript-mode . lispy-mode)
;;   (clojurec-mode . lispy-mode)
;;   (hy-mode . lispy-mode)
;;   (lfe-mode . lispy-mode)
;;   (scheme-mode . lispy-mode)
;;   (lisp-mode . lispy-mode)
;;   (ielm-mode . lispy-mode)
;;   (eval-expression-minibuffer-setup . lispy-mode)
;;   :config
;;   (setq lispy-close-quotes-at-end-p t))

;; (use-package lispyville
;;   :ensure t
;;   :hook
;;   (lispy-mode . lispyville-mode)
;;   :config
;;   (lispyville-set-key-theme
;;    '((operators normal)
;;      c-w
;;      (prettify insert)
;;      (atom-movement normal visual)
;;      slurp/barf-lispy
;;      additional
;;      additional-insert)))

;;. Shell (vterm)
(use-package vterm
  :ensure t)


;;. LSP and languages
(use-package eglot
  :config
  (define-key eglot-mode-map
              (kbd "C-c C-t") #'eldoc-print-current-symbol-info)
  :hook
  ((tuareg-mode . eglot-ensure)))
  ;; :config
  ;; (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer"))))

(use-package tree-sitter
  :ensure t
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(setq treesit-language-source-alist
  '((bash "https://github.com/tree-sitter/tree-sitter-bash")
    (c "https://github.com/tree-sitter/tree-sitter-c")
    (cmake "https://github.com/uyha/tree-sitter-cmake")
    (common-lisp "https://github.com/theHamsta/tree-sitter-commonlisp")
    (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
    (css "https://github.com/tree-sitter/tree-sitter-css")
    (csharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
    (elisp "https://github.com/Wilfred/tree-sitter-elisp")
    (go "https://github.com/tree-sitter/tree-sitter-go")
    (go-mod "https://github.com/camdencheek/tree-sitter-go-mod")
    (html "https://github.com/tree-sitter/tree-sitter-html")
    (js . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
    (json "https://github.com/tree-sitter/tree-sitter-json")
    (lua "https://github.com/Azganoth/tree-sitter-lua")
    (make "https://github.com/alemuller/tree-sitter-make")
    (markdown "https://github.com/ikatyang/tree-sitter-markdown")
    (python "https://github.com/tree-sitter/tree-sitter-python")
    (r "https://github.com/r-lib/tree-sitter-r")
    (rust "https://github.com/tree-sitter/tree-sitter-rust")
    (toml "https://github.com/tree-sitter/tree-sitter-toml")
    (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
    (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
    (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;;.. Python
(use-package python-mode
  :ensure nil
  :hook
  (python-mode . eglot-ensure)
  :custom
  (python-shell-interpreter "python3"))

(use-package micromamba
  :straight t)


;;.. Elixir
(use-package elixir-ts-mode
  :ensure t)


;;.. Sly
(load (expand-file-name "~/.roswell/helper.el"))

(use-package sly
  :defer t
  :config
  (setf sly-lisp-implementations
        '((sbcl ("sbcl" "--dynamic-space-size 4096") :coding-system utf-8-unix)
          (roswell ("ros" "-Q" "run")  :coding-system utf-8-unix)
          (ccl ("ros" "-Q" "run" "-L" "ccl-bin"))))
  (setf sly-default-lisp 'roswell))


;;.. ESS
(use-package ess
  :ensure t
  :init (require 'ess-site))


;;.. Ocaml
(use-package tuareg
  :ensure t
  :mode (("\\.ocamlinit\\'" . tuareg-mode)))

(use-package merlin
  :ensure t
  :config
  (add-hook 'tuareg-mode-hook #'merlin-mode))

(use-package utop
  :ensure t
  :config
  (add-hook 'tuareg-mode-hook #'utop-minor-mode))


;;.. Rust
(use-package rust-mode
  :ensure nil
  :hook
  (rust-mode . eglot-ensure)
  :init
  (setq rust-mode-treesitter-derive t))

(use-package flymake-clippy
  :hook
  (rust-mode . flymake-clippy-setup-backend))


;;.. Racket
(use-package racket-mode
  :ensure t
  :defer
  :mode "\\.rkt\\'")

(use-package sicp
  :ensure t)

(use-package ob-racket
  :after org
  :config (add-hook 'ob-racket-pre-runtime-library-load-hook
                    #'ob-racket-raco-make-runtime-library)
  :straight (:host github :repo "hasu/emacs-ob-racket")
  :ensure t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((racket . t)))


;;.. Clojure
(use-package clojure-mode
  :ensure t
  :defer
  :mode "\\.clj\\'")

(use-package cider
  :ensure t
  :defer
  :hook
  (clojure-mode . cider-mode))

;;. Breadcrumb mode
(use-package breadcrumb
  :ensure t
  :config
  (breadcrumb-mode 1))


;;. Copilot
(use-package copilot
  :straight (:repo "copilot-emacs/copilot.el" :host github :files ("*.el" "dist"))
  :hook (prog-mode . copilot-mode)
  :bind (("C-\\" . 'copilot-accept-completion-by-word)
         ("C-;" . 'copilot-accept-completion-by-line)
         ("M-;" . 'copilot-accept-completion)
         ("M-s-]" . 'copilot-next-completion)
         ("M-s-[" . 'copilot-previous-completion)
         :map copilot-completion-map))

(use-package copilot-chat
  :straight (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
  :after (request org markdown-mode))

;;. Remote R session
;; From https://stackoverflow.com/a/22703777
;; Allow emacs to connect to persistent R sessions running remotely
;; 1: Separately to emacs, ssh onto the farm
;; 2: Launch a shell using dtach:
;;    dtach -A .dtach-(session-name) $SHELL
;; 3: Launch an interactive R jobs from this shell:
;;    bsubmem MMMM -Is R (--no-readline) [optionally now use C-\ to detach]
;; 4: In emacs, run M-x R-remote to connect to the session
(defvar R-remote-host "farm22")
(defvar R-remote-session "R")
(defvar R-remote-directory "~")
(defun R-remote (&optional remote-host session directory)
  "Connect to the remote-host's dtach session running R."
  (interactive (list
                (read-from-minibuffer "R remote host: " R-remote-host)
                (read-from-minibuffer "R remote session: " R-remote-session)
                (read-from-minibuffer "R remote directory: " R-remote-directory)))
  (pop-to-buffer (make-comint (concat "remote-" session)
                              "ssh" nil "-t" "-t" remote-host
                              "cd" directory ";"
                              "/nfs/dog_n_devil/kevin/gentoo/usr/bin/dtach" "-A" (concat ".dtach-" session)
                              "-z" "-E" "-r" "none"
                              inferior-R-program-name "--no-readline"
                              inferior-R-args))
  (ess-remote (process-name (get-buffer-process (current-buffer))) "R")
  (setq comint-process-echoes t))

;;. Tramp
(setq tramp-verbose 6)
(setq tramp-debug-buffer t)
(setq tramp-debug-to-file t)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
