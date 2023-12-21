;; from emacs from scratch #1
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
;; end requirements for lead

(leaf evil
  :ensure t
  :init
  (setq evil-want-c-u-scroll t)
  (evil-mode 1))

(leaf org
  :after evil
  :leaf-defer nil)

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
  :init (which-key-mode))

(leaf catppuccin-theme
  :init (load-theme 'catppuccin :no-confirm))

;; Enable vertico
(leaf vertico
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

;; Persist history over Emacs restarts. Vertico sorts by history position.
(leaf savehist
  :init
  (savehist-mode))

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
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; Optionally use the `orderless' completion style.
(leaf orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       Orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

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
          ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
          ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
          ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
          ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
          ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
          ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
          ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
          ;; Custom M-# bindings for fast register access
          ("M-#" . consult-register-load)
          ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
          ("C-M-#" . consult-register)
          ;; Other custom bindings
          ("M-y" . consult-yank-pop)                ;; orig. yank-pop
          ;; M-g bindings in `goto-map'
          ("M-g e" . consult-compile-error)
          ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
          ("M-g g" . consult-goto-line)             ;; orig. goto-line
          ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
          ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
          ("M-g m" . consult-mark)
          ("M-g k" . consult-global-mark)
          ("M-g i" . consult-imenu)
          ("M-g I" . consult-imenu-multi)
          ;; M-s bindings in `search-map'
          ("M-s d" . consult-find)                  ;; Alternative: consult-fd
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
          ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
          ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
          ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
          ("M-s L" . consult-line-multi))            ;; needed by consult-line to detect isearch
          ;; Minibuffer history
          (:minibuffer-local-map
          ("M-s" . consult-history)                 ;; orig. next-matching-history-element
          ("M-r" . consult-history)))                ;; orig. previous-matching-history-element

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

;; cape
;; Enable Corfu completion UI
;; See the Corfu README for more configuration tips.
(leaf corfu
  :init
  (global-corfu-mode)
(setq corfu-auto        t
            corfu-auto-delay  0.5 ;; TOO SMALL - NOT RECOMMENDED
            corfu-auto-prefix 1 ;; TOO SMALL - NOT RECOMMENDED
            completion-styles '(basic)))
;; Add extensions
; use prescient to filter corfu
(leaf corfu-prescient
  :after corfu
  :init (corfu-prescient-mode 1))
(leaf nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(leaf cape
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
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-emoji))


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
;; projectile
(leaf projectile
  :ensure t
  :init
  (projectile-mode +1)
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
