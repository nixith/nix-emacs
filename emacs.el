;;; emacs.el  my emacs init file -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
;;; from DOOM emacs
(setq gc-cons-threshold most-positive-fixnum) ;v large GC buffer, gcmh-mode cleans it up later
(setq load-prefer-newer noninteractive)		;I believer nix takes care of this as files loaded are always compiled and static
(setq-default bidi-display-reordering 'left-to-right ;I don't use bidirectional text (hebrew, arabic, etc), so diabling it helps performance
              bidi-paragraph-direction 'left-to-right
	      bidi-inhibit-bpa t)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)
;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)
;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
(setq idle-update-delay 1.0)  ; default is 0.5
;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but do it anyway, just in case. This increases memory
;; usage, however!
(setq inhibit-compacting-font-caches t)
;; PGTK builds only: this timeout adds latency to frame operations, like
;; `make-frame-invisible', which are frequently called without a guard because
;; it's inexpensive in non-PGTK builds. Lowering the timeout from the default
;; 0.1 should make childframes and packages that manipulate them (like `lsp-ui',
;; `company-box', and `posframe') feel much snappier. See emacs-lsp/lsp-ui#613.
  (setq pgtk-wait-for-event-timeout 0.001)
;; Increase how much is read from processes in a single chunk (default is 4kb).
;; This is further increased elsewhere, where needed (like our LSP module).
(setq read-process-output-max (* 64 1024))  ; 64kb
(setq redisplay-skip-fontification-on-input t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar
;; Set up the visible bell
(setq visible-bell t)
;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;;line numbers
(column-number-mode)
(global-display-line-numbers-mode t)
(prettify-symbols-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))



;; requirements for leaf
(eval-and-compile
  (customize-set-variable
   'package-archives '(("melpa" . "https://melpa.org/packages/")
		       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure nil)
    (leaf el-get :ensure nil)
    (leaf blackout :ensure nil)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))
;; end requirements for leaf

;; performance stuff
(use-package gcmh 			;for some reason leaf causes the error, this doesnt
 :custom ((gcmh-idle-delay . 'auto)
	  (gcmh-auto-idle-delay-factor .  10)
	  (gcmh-high-cons-threshold . (* 16 1024 1024))) ;16mb
 :hook (after-init-hook . (gcmh-mode 1))
 )
(leaf hyperbole)
(leaf evil
  :custom (
	   (evil-shift-width . 4)
	   (evil-undo-system . 'undo-redo)
	   (evil-want-c-u-scroll . t)
  ;; 	   (evil-want-keybinding . nil) ; needed for evil-collection
  ;; 	   (evil-want-integration . t)
     )
  :init
  (evil-mode 1))
;; (leaf evil-collection ;;TODO: figure out why this breaks lispy
;;   :after evil
;;   :custom (electric-pair-mode . t)
	  
;;   :config
;;   (evil-collection-init))


(leaf org
  :after evil
  :leaf-defer nil)
(leaf org-modern
  :hook (
	 (org-mode-hook . org-modern-mode)
	 (org-agenda-finalize-hook . org-modern-agenda)))


(leaf rainbow-mode
  :leaf-defer nil
  :hook org-mode
  emacs-lisp-mode
  web-mode
  typescript-mode
  js2-mode)

(leaf rainbow-delimiters
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(leaf which-key
  :after evil
  :config (which-key-mode))

(leaf catppuccin-theme
  :init (load-theme 'catppuccin :no-confirm))

;; Enable vertico
(leaf vertico
  :bind (auth-source-do-warn)
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )
(leaf vertico-posframe
  :after vertico
  :custom (vertico-posframe-mode . 1))

;; A few more useful configurations...
(leaf emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
		  (replace-regexp-in-string
		   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
		   crm-separator)
		  (car args))
	  (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
	'(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
	#'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete-tag))

(leaf doom-modeline
  :init (doom-modeline-mode)
  :custom (
	   (doom-modeline-height . 25)
	   (doom-modeline-support-imenu . t)
	   (doom-modeline-hud . t) ;;TODO: see if I like this setting
	   (doom-modeline-icon . t)))
(leaf solaire-mode
  :init (solaire-global-mode))
(leaf doom-themes)

;; Optionally use the `orderless' completion style.
(leaf orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       Orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless partial-completion basic)
	completion-category-defaults nil
	completion-category-overrides nil))

;; Example configuration for Consult
(leaf consult
  ;; Replace bindings. Lazily loaded due by `leaf'.
  :bind ;; C-c bindings in `mode-specific-map'
  (("C-c M-x" . consult-mode-command)
   ("C-c h" . consult-history)
   ("C-c k" . consult-kmacro)
   ("C-c m" . consult-man)
   ("C-c i" . consult-info)
   ([remap Info-search] . consult-info)
   ;; C-x bindings in `ctl-x-map'
   ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
   ("C-x b" . consult-buffer)		 ;; orig. switch-to-buffer
   ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
   ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
   ("C-x t b" . consult-buffer-other-tab) ;; orig. switch-to-buffer-other-tab
   ("C-x r b" . consult-bookmark)	  ;; orig. bookmark-jump
   ("C-x p b" . consult-project-buffer) ;; orig. project-switch-to-buffer
   ;; Custom M-# bindings for fast register access
   ("M-#" . consult-register-load)
   ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
   ("C-M-#" . consult-register)
   ;; Other custom bindings
   ("M-y" . consult-yank-pop) ;; orig. yank-pop
   ;; M-g bindings in `goto-map'
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flycheck)	 ;; Alternative: consult-flycheck
   ("M-g g" . consult-goto-line) ;; orig. goto-line
   ("M-g M-g" . consult-goto-line) ;; orig. goto-line
   ("M-g o" . consult-outline)	   ;; Alternative: consult-org-heading
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   ;; M-s bindings in `search-map'
   ("M-s d" . consult-find) ;; Alternative: consult-fd
   ("M-s c" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)
   ;; Isearch integration
   ("M-s e" . consult-isearch-history)
   (:isearch-mode-map
    ("M-e" . consult-isearch-history) ;; orig. isearch-edit-string
    ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
    ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
    ("M-s L" . consult-line-multi)) ;; needed by consult-line to detect isearch
   ;; Minibuffer history
   (:minibuffer-local-map
    ("M-s" . consult-history) ;; orig. next-matching-history-element
    ("M-r" . consult-history))) ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
	register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  ;; (consult-customize ;;NOTE: Disabled due to weird leaf errors
  ;;  consult-theme :preview-key '(:debounce 0.2 any)
  ;;  consult-ripgrep consult-git-grep consult-grep
  ;;  consult-bookmark consult-recent-file consult-xref
  ;;  consult--source-bookmark consult--source-file-register
  ;;  consult--source-recent-file consult--source-project-recent-file
  ;;  ;; :preview-key "M-."
  ;;  :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
;;;; 4. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )
(leaf consult-flycheck)

;; cape
;; Enable Corfu completion UI
;; See the Corfu README for more configuration tips.
;; disabled in favor of lsp-bridge. As nice as it would be
;; to have everything standardized, emacs' current model
;; just isn't set up well for LSP.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (leaf corfu										        ;;
;; 											        ;;
;;   :after orderless									        ;;
;;   :custom (										        ;;
;; 	   (corfu-cycle . t)								        ;;
;; 	   (corfu-auto . t)	   ;; Enable auto completion				        ;;
;; 	   (corfu-separator . ?\s) ;; Orderless field separator			        ;;
;; 	   (corfu-auto-prefix . 2)							        ;;
;; 	   (corfu-auto-delay . 0.0)							        ;;
;; 	   (corfu-popupinfo-delay . 1)						        ;;
;; 	   ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary	        ;;
;; 	   ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match        ;;
;; 	   ;; (corfu-preview-current nil)    ;; Disable current candidate preview	        ;;
;; 	   (corfu-preselect 'prompt) ;; Preselect the prompt				        ;;
;; 	   ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches	        ;;
;; 	   ;; (corfu-scroll-margin 5)        ;; Use scroll margin			        ;;
;; 	   )										        ;;
;;   ;; Enable Corfu only for certain modes.						        ;;
;;   ;; :hook ((prog-mode . corfu-mode)							        ;;
;;   ;;        (shell-mode . corfu-mode)						        ;;
;;   ;;        (eshell-mode . corfu-mode))						        ;;
;; 											        ;;
;;   ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can	        ;;
;;   ;; be usesed globally (M-/).  See also the customization variable			        ;;
;;   ;; `globally-corfu-modes' to exclude certain modes.				        ;;
;;   :bind										        ;;
;;   (:corfu-map (									        ;;
;; 	       ("TAB" . corfu-next)							        ;;
;; 	       ([tab] . corfu-next)							        ;;
;; 	       ("S-TAB" . corfu-previous)						        ;;
;; 	       ([backtab] . corfu-previous)))					        ;;
;;   :init										        ;;
;;   (global-corfu-mode)								        ;;
;;   (corfu-history-mode)								        ;;
;;   (corfu-popupinfo-mode))								        ;;
;; 											        ;;
;; 											        ;;
;; (leaf corfu-prescient ;; use prescient to filter corfu				        ;;
;;   :after corfu									        ;;
;;   :init (corfu-prescient-mode 1))							        ;;
;; (leaf corfu-terminal									        ;;
;;   :config										        ;;
;;   (unless (display-graphic-p)							        ;;
;;     (corfu-terminal-mode 1)))							        ;;
;;   											        ;;
;;   (leaf nerd-icons-corfu								        ;;
;;     :after corfu									        ;;
;;     :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))	        ;;
;; 											        ;;
;; 											        ;;
;; ;; Use Dabbrev with Corfu!								        ;;
;; (leaf dabbrev									        ;;
;;   ;; Swap M-/ and C-M-/								        ;;
;;   :bind (("M-/" . dabbrev-completion)						        ;;
;; 	 ("C-M-/" . dabbrev-expand))							        ;;
;;   ;; Other useful Dabbrev configurations.						        ;;
;;   :custom										        ;;
;;   (dabbrev-ignored-buffer-regexps . '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))		        ;;
;; 											        ;;
;; 											        ;;
;; 											        ;;
;; (leaf cape										        ;;
;;   ;; Bind dedicated completion commands						        ;;
;;   ;; Alternative prefix keys: C-c p, M-p, M-+, ...					        ;;
;; 											        ;;
;;   :bind (("C-c p p" . completion-at-point) ;; capf					        ;;
;; 	 ("C-c p t" . complete-tag)	   ;; etags					        ;;
;; 	 ("C-c p d" . cape-dabbrev)	   ;; or dabbrev-completion			        ;;
;; 	 ("C-c p h" . cape-history)							        ;;
;; 	 ("C-c p f" . cape-file)							        ;;
;; 	 ("C-c p k" . cape-keyword)							        ;;
;; 	 ("C-c p s" . cape-elisp-symbol)						        ;;
;; 	 ("C-c p e" . cape-elisp-block)						        ;;
;; 	 ("C-c p a" . cape-abbrev)							        ;;
;; 	 ("C-c p l" . cape-line)							        ;;
;; 	 ("C-c p w" . cape-dict)							        ;;
;; 	 ("C-c p :" . cape-emoji)							        ;;
;; 	 ("C-c p \\" . cape-tex)							        ;;
;; 	 ("C-c p _" . cape-tex)							        ;;
;; 	 ("C-c p ^" . cape-tex)							        ;;
;; 	 ("C-c p &" . cape-sgml)							        ;;
;; 	 ("C-c p r" . cape-rfc1345))							        ;;
;;   :init										        ;;
;;   ;; Add to the global default value of `completion-at-point-functions' which is	        ;;
;;   ;; used by `completion-at-point'.  The order of the functions matters, the		        ;;
;;   ;; first function returning a result wins.  Note that the list of buffer-local	        ;;
;;   ;; completion functions takes precedence over the global list.			        ;;
;;   (add-to-list 'completion-at-point-functions #'cape-keyword)			        ;;
;;   (add-to-list 'completion-at-point-functions #'cape-dabbrev)			        ;;
;;   (add-to-list 'completion-at-point-functions #'cape-file)				        ;;
;;   (add-to-list 'completion-at-point-functions #'cape-elisp-block)			        ;;
;;   ;;(add-to-list 'completion-at-point-functions #'cape-history)			        ;;
;;   ;;(add-to-list 'completion-at-point-functions #'cape-sgml)				        ;;
;;   ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)			        ;;
;;   ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)			        ;;
;;   ;;(add-to-list 'completion-at-point-functions #'cape-dict)				        ;;
;;   (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)			        ;;
;;   ;;(add-to-list 'completion-at-point-functions #'cape-line)				        ;;
;;   (add-to-list 'completion-at-point-functions #'cape-tex)				        ;;
;;   (add-to-list 'completion-at-point-functions #'cape-emoji)				        ;;
;;   (defun my/setup-elisp ()								        ;;
;;     (setq-local completion-at-point-functions					        ;;
;; 		`(,(cape-super-capf							        ;;
;; 		    #'elisp-completion-at-point						        ;;
;; 		    #'cape-dabbrev)							        ;;
;; 		  cape-file)								        ;;
;; 		cape-dabbrev-min-length 5))						        ;;
;;   (add-hook 'emacs-lisp-mode-hook #'my/setup-elisp))					        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(leaf marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind ((:minibuffer-local-map
	  ("M-A" . marginalia-cycle))
	 (:completion-list-mode-map
	  ("M-A" . marginalia-cycle)))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of leaf such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))
(leaf embark
  :bind (("C-'" . embark-act) ;; pick some comfortable binding
	 ("C-;" . embark-dwim) ;; good alternative: M-.
	 ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none)))))
(leaf embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
;; projectile
(leaf projectile
  :ensure t
  :init
  (projectile-mode 1)
  :bind ((:projectile-mode-map
	  ;;("s-p" . projectile-command-map)
	  ("C-c p" . projectile-command-map))))


;; lisp editing tools
(leaf lispy
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
  (defun conditionally-enable-lispy ()
    (when (eq this-command 'eval-expression)
      (lispy-mode 1)))
  (add-hook 'minibuffer-setup-hook 'conditionally-enable-lispy))

(leaf lispyville
  :hook ((emacs-lisp-mode-hook . lispyville-mode))
  :config
  (lispyville-set-key-theme '(operators c-w additional)))

(leaf flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode))
;; (Leaf lsp-mode
;;   :custom
;;   (lsp-completion-provider :none) ;; we use Corfu!
;;   :init
;;   (setq lsp-keymap-prefix "C-c l")
;;   (defun my/lsp-mode-setup-completion ()
;;     (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
;;           '(flex))) ;; Configure flex
;;   :hook (
;; 	 (lsp-completion-mode . my/lsp-mode-setup-completion)
;; 	 (lsp-mode . lsp-enable-which-key-integration)
;; 	 ;;follow this pattern for everything you want an lsp for
;; 	 (nix-ts-mode . lsp)
;; 	 (rust-ts-mode . lsp))
;;   :commands lsp)
;;   (use-package lsp-ui
;;     :config
;;     (setq lsp-ui-doc-position 'at-point
;; 	  lsp-ui-doc-show-with-cursor t)
;;     :commands lsp-ui-mode)


;; disabled for similar reasons as above - using lsp-bridge
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defvar +lsp-defer-shutdown 3								        ;;
;;   "If non-nil, defer shutdown of LSP servers for this many seconds after last		        ;;
;; workspace buffer is closed.									        ;;
;; 												        ;;
;; This delay prevents premature server shutdown when a user still intends on			        ;;
;; working on that project after closing the last buffer, or when programmatically		        ;;
;; killing and opening many LSP/eglot-powered buffers.")					        ;;
;; 												        ;;
;; 												        ;;
;; ;;												        ;;
;; ;;; Common											        ;;
;; 												        ;;
;; (defvar +lsp--default-read-process-output-max nil)						        ;;
;; (defvar +lsp--default-gcmh-high-cons-threshold nil)						        ;;
;; (defvar +lsp--optimization-init-p nil)							        ;;
;; 												        ;;
;; (define-minor-mode +lsp-optimization-mode							        ;;
;;   "Deploys universal GC and IPC optimizations for `lsp-mode' and `eglot'."			        ;;
;;   :global t											        ;;
;;   :init-value nil										        ;;
;;   (if (not +lsp-optimization-mode)								        ;;
;;       (setq-default read-process-output-max +lsp--default-read-process-output-max		        ;;
;;                     gcmh-high-cons-threshold +lsp--default-gcmh-high-cons-threshold		        ;;
;;                     +lsp--optimization-init-p nil)						        ;;
;;     ;; Only apply these settings once!							        ;;
;;     (unless +lsp--optimization-init-p							        ;;
;;       (setq +lsp--default-read-process-output-max (default-value 'read-process-output-max)	        ;;
;;             +lsp--default-gcmh-high-cons-threshold (default-value 'gcmh-high-cons-threshold))        ;;
;;       (setq-default read-process-output-max (* 1024 1024))					        ;;
;;       ;; REVIEW LSP causes a lot of allocations, with or without the native JSON		        ;;
;;       ;;        library, so we up the GC threshold to stave off GC-induced			        ;;
;;       ;;        slowdowns/freezes. Doom (where this code was obtained from)			        ;;
;;       ;;        uses `gcmh' to enforce its GC strategy,					        ;;
;;       ;;        so we modify its variables rather than `gc-cons-threshold'			        ;;
;;       ;;        directly.									        ;;
;;       (setq-default gcmh-high-cons-threshold (* 2 +lsp--default-gcmh-high-cons-threshold))	        ;;
;;       (gcmh-set-high-threshold)								        ;;
;;       (setq +lsp--optimization-init-p t))))							        ;;
;; 												        ;;
;; (leaf eglot											        ;;
;;   :hook ((eglot-managed-mode . +lsp-optimization-mode)					        ;;
;; 	 (nix-ts-mode . eglot-ensure)							        ;;
;; 	 (rust-ts-mode . eglot-ensure))							        ;;
;;   :config (											        ;;
;; 	   (add-to-list 'eglot-server-programs '(nix-ts-mode . ("nil"))))			        ;;
;;   :init											        ;;
;;   (setq completion-category-overrides '((eglot (styles orderless))))				        ;;
;;   (setq eglot-sync-connect 1									        ;;
;; 	eglot-autoshutdown t									        ;;
;; 	eglot-send-changes-idle-time 0.5							        ;;
;; 	;; NOTE This setting disable the eglot-events-buffer enabling more			        ;;
;; 	;;      consistent performance on long running emacs instance.			        ;;
;; 	;;      Default is 2000000 lines. After each new event the whole buffer		        ;;
;; 	;;      is pretty printed which causes steady performance decrease over time.	        ;;
;; 	;;      CPU is spent on pretty priting and Emacs GC is put under high pressure.	        ;;
;; 	eglot-events-buffer-size 0								        ;;
;; 	;; NOTE We disable eglot-auto-display-help-buffer because :select t in		        ;;
;; 	;;      its popup rule causes eglot to steal focus too often.			        ;;
;; 	eglot-auto-display-help-buffer nil)							        ;;
;;   												        ;;
;;   (defun my/eglot-capf ()									        ;;
;;     (setq-local completion-at-point-functions						        ;;
;; 		(list (cape-super-capf								        ;;
;; 		       #'eglot-completion-at-point						        ;;
;; 		       #'tempel-expand								        ;;
;; 		       #'cape-file))))								        ;;
;; 												        ;;
;;   (add-hook 'eglot-managed-mode-hook #'my/eglot-capf))					        ;;
;; (leaf consult-eglot)										        ;;
;; (leaf flycheck-eglot										        ;;
;;   :after (flycheck eglot)									        ;;
;;   :custom (flycheck-eglot-exclusive . nil)							        ;;
;;   :config											        ;;
;;   (global-flycheck-eglot-mode 1))								        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-bridge
  :after yasnippet
  :custom ((lsp-bridge-nix-lsp-server . "nil") ;nil the lsp - not the value
	   )
  :init (global-lsp-bridge-mode))
(leaf yasnippet
  :init (yas-global-mode 1)) 			;needed for lsp-bridge, can still template in tempel
(leaf yasnippet-snippets)
(leaf markdown-mode)

(leaf tempel
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
	 ("M-*" . tempel-insert))

  :init

  ;; commented due to useing lsp-bridge
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ;; Setup completion at point						   ;;
  ;; (defun tempel-setup-capf ()						   ;;
  ;;   ;; Add the Tempel Capf to `completion-at-point-functions'.		   ;;
  ;;   ;; `tempel-expand' only triggers on exact matches. Alternatively use	   ;;
  ;;   ;; `tempel-complete' if you want to see all matches, but then you	   ;;
  ;;   ;; should also configure `tempel-trigger-prefix', such that Tempel	   ;;
  ;;   ;; does not trigger too often when you don't expect it. NOTE: We add	   ;;
  ;;   ;; `tempel-expand' *before* the main programming mode Capf, such		   ;;
  ;;   ;; that it will be tried first.						   ;;
  ;;   (setq-local completion-at-point-functions				   ;;
  ;; 		(cons #'tempel-expand						   ;;
  ;; 		      completion-at-point-functions)))				   ;;
  ;; 										   ;;
  ;; (add-hook 'conf-mode-hook 'tempel-setup-capf)				   ;;
  ;; (add-hook 'prog-mode-hook 'tempel-setup-capf)				   ;;
  ;; (add-hook 'text-mode-hook 'tempel-setup-capf)				   ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  )

;; Optional: Add tempel-collection.
;; The package is young and doesn't have comprehensive coverage.
(leaf tempel-collection)

;; spellcheck
(leaf jinx
  :hook (
	 (emacs-startup . global-jinx-mode))
  :bind (("M-$" . jinx-correct)
	 ("C-M-$" . jinx-languages)))

;; tree-sitter NOTE: might want to check this as the ecosystem uses native emacs

;; ligatures

(leaf ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(leaf all-the-icons
  :if (display-graphic-p))

(leaf spacious-padding
  :custom (spacious-padding-widths)
  :init (spacious-padding-mode))

(leaf helpful
  :bind (("C-h f" . helpful-callable)
	 ("C-h v" . helpful-variable)
	 ("C-h k" . helpful-key)
	 ("C-h x" . helpful-command)
	 ("C-c C-d" . helpful-at-point)
	 ("C-c F" . helpful-function)
	 ))

(leaf centaur-tabs
  :custom ((centaur-tabs-mode . t)
	   (centaur-tabs-style . "rounded"))
  ;; (centaur-tabs-close-button .  "X") ;; disable close button
  :bind (
	 ("C-<prior>" . centaur-tabs-backward)
	 ("C-<next>" . centaur-tabs-forward)))

(leaf highlight-indent-guides
  :hook
  (prog-mode-hook . highlight-indent-guides-mode))

(leaf magit) 				;"But I will always use magit"
(leaf vterm)
(leaf eradio
  :custom (eradio-player . 'mpv))

(leaf hl-todo
  :hook ()
  :init (global-hl-todo-mode)
  )
(use-package flycheck-hl-todo
  :defer 5 				;wait for the other checkers
  :config (flycheck-hl-todo-setup)
  )
(use-package consult-todo)
;; languages
;;; nix-mode
(leaf nix-mode
  :mode "\\.nix\\'")
(leaf nix-ts-mode
  :mode "\\.nix\\'")

;;; rust

(provide 'emacs)

;;; emacs.el ends here
