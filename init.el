;; -*- lexical-binding: t; -*-
;;. Add init lisp scripts folder to load path  
(add-to-list 
  'load-path
  (expand-file-name "package_inits" user-emacs-directory))

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
     :target (file+head "${slug}.org.gpg"
                        "#+title: ${title}\n")
     :unnarrowed t)))
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

;; Set calendar start of the week to Monday
(setq calendar-week-start-day 1)

; (add-hook 'org-mode-hook #'mixed-pitch-mode)
; (add-hook 'org-mode-hook #'solaire-mode)

; (add-hook 'mixed-pitch-mode-hook #'solaire-mode-reset)

(setq org-html-head-extra "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://gongzhitaao.org/orgcss/org.css\"/>")
(setq org-html-head-include-default-style nil)

;;.. Export via pandoc
(use-package ox-pandoc
  :after org
  :ensure t)

;;. Outlining

;;.. outli
(use-package outli
  :straight (:host github :repo "jdtsmith/outli")
  ;:after lispy ; uncomment only if you use lispy; it also sets speed keys on headers!
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

(use-package evil-collection
  :after (evil)
  :demand
  :config
  (evil-collection-init))

(use-package evil-lion
  :ensure t
  :config
  (evil-lion-mode))

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

(use-package nerd-icons)

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))


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
    "bd"  'kill-current-buffer))

;; Which key, shows what keys do
(use-package which-key
  :demand
  :init
  (setq which-key-idle-delay 0.5) ; Open after .5s instead of 1s
  :config
  (which-key-mode))


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
;;. Minibuffer
;;.. Marginalia
(use-package marginalia
  :general
  (:keymaps 'minibuffer-local-map
   "M-A" 'marginalia-cycle)
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
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
                                vertico-unobtrusive
                                ))
  :general
  (:keymaps '(normal insert visual motion)
   "M-." #'vertico-repeat
   )
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
   "C-l" #'kb/vertico-multiform-flat-toggle
   )
  :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy) ; Clean up file path when typing
         (minibuffer-setup . vertico-repeat-save) ; Make sure vertico state is saved
         )
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
     (t reverse)
     ))
  (vertico-multiform-commands
   '(("flyspell-correct-*" grid reverse)
     (org-refile grid reverse indexed)
     (consult-yank-pop indexed)
     (consult-flycheck)
     (consult-lsp-diagnostics)
     ))
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
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
)

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
;;. Smartparens
(defun my-enable-evil-move-beyond-eol ()
  (setq-local evil-move-beyond-eol t))

(defun my-disable-evil-move-beyond-eol ()
  (setq-local evil-move-beyond-eol nil))

(use-package smartparens
  :ensure t
  :defer t
  :init
  :bind (:map smartparens-mode-map
              ("C-M-f" . sp-forward-sexp)
              ("C-M-b" . sp-backward-sexp)
              ("C-M-u" . sp-backward-up-sexp)
              ("C-M-d" . sp-down-sexp)
              ("C-M-a" . sp-backward-down-sexp)
              ("C-M-e" . sp-up-sexp)
              ("C-M-s" . sp-splice-sexp)
              ("M-(" . sp-wrap-round)
              ("M-[" . sp-wrap-square)
              ("M-{" . sp-wrap-curly)
              ("<M-delete>" . sp-unwrap-sexp)
              ("<M-backspace>" . sp-backward-unwrap-sexp)
              ("s-<right>" . sp-forward-slurp-sexp)
              ("s-<left>" . sp-forward-barf-sexp)
              ("M-<left>" . sp-backward-slurp-sexp)
              ("M-<right>" . sp-backward-barf-sexp))
  :config
  (add-hook 'smartparens-enabled-hook #'my-enable-evil-move-beyond-eol)
  (add-hook 'smartparens-disabled-hook #'my-disable-evil-move-beyond-eol))

;;. Shell (vterm)
(use-package vterm
  :ensure t)


;;. LSP and languages
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (rustic-mode . lsp)
         (ess-r-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode)
(add-to-list 'load-path (expand-file-name "lib/lsp-mode" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lib/lsp-mode/clients" user-emacs-directory))

;;.. Python
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred


;;.. Sly
(use-package sly
  :ensure t)

(setq sly-lisp-implementations
      '((sbcl ("sbcl" "--dynamic-space-size 4096"))))


;;.. ESS
(use-package ess
  :ensure t
  :init (require 'ess-site))


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


;;. Remote R session
;; From https://stackoverflow.com/a/22703777
;; Allow emacs to connect to persistent R sessions running remotely
;; 1: Separately to emacs, ssh onto the farm
;; 2: Launch a shell using dtach:
;;    dtach -A .dtach-(session-name) $SHELL
;; 3: Launch an interactive R jobs from this shell:
;;    bsubmem MMMM -Is R (--no-readline) [optionally now use C-\ to detach]
;; 4: In emacs, run M-x R-remote to connect to the session
(defvar R-remote-host "farm5")
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
