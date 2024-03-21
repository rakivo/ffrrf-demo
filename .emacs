(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(pixel-scroll-precision-mode 1)

(setq x-super-keysym 'meta)
(setq x-alt-keysym 'capslock)

;; (use-package evil-surround
;;   :ensure t
;;   :config
;;   (global-evil-surround-mode 1))

;; (require 'evil)
;; (evil-mode 1)

(add-hook 'emacs-lisp-mode-hook #'paredit-mode)

(use-package surround :ensure t)
    
(global-display-line-numbers-mode 1)
(setq display-line-numbers 'relative)

(setq haskell-interactive-popup-errors nil)

(defun set-rectangle-cursor ()
  (setq cursor-type 'box))

(global-set-key (kbd "TAB") (lambda () (interactive) (insert "    ")))

(set-default 'cursor-type 'box)
;; (setq evil-insert-state-cursor 'box)

;; (add-hook 'evil-normal-state-entry-hook 'set-rectangle-cursor)
;; (add-hook 'evil-insert-state-entry-hook 'set-rectangle-cursor)
;; (add-hook 'evil-visual-state-entry-hook 'set-rectangle-cursor)

;; (with-eval-after-load 'evil
;;   (defalias #'forward-evil-word #'forward-evil-symbol)
;;   (setq-default evil-symbol-word-search t))

;; (define-key evil-insert-state-map (kbd "C-h") 'left-char)
;; (define-key evil-insert-state-map (kbd "C-j") 'next-line)
;; (define-key evil-insert-state-map (kbd "C-k") 'previous-line)
;; (define-key evil-insert-state-map (kbd "C-l") 'right-char)
;; (define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
;; (define-key evil-insert-state-map (kbd "C-a") 'move-beginning-of-line)
;; (define-key evil-insert-state-map (kbd "C-y") 'yank)
;; (define-key evil-insert-state-map (kbd "C-d") 'delete-char)
;; (define-key evil-insert-state-map (kbd "C-w") 'kill-region)
;; (define-key evil-insert-state-map (kbd "C-f") 'forward-word)
;; (define-key evil-insert-state-map (kbd "C-b") 'backward-word)

;; (define-key global-map (kbd "C-h") #'evil-window-left)
;; (define-key global-map (kbd "C-j") #'evil-window-down)
;; (define-key global-map (kbd "C-k") #'evil-window-up)
;; (define-key global-map (kbd "C-l") #'evil-window-right)

(define-key global-map (kbd "C-c b") #'windmove-left)
(define-key global-map (kbd "C-c n") #'windmove-down)
(define-key global-map (kbd "C-c p") #'windmove-up)
(define-key global-map (kbd "C-c f") #'windmove-right)

(define-key global-map (kbd "C-?") #'comment-or-uncomment-region)

(require 'move-text)
(global-set-key (kbd "M-n") 'move-text-down)
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-j") 'move-text-down)
(global-set-key (kbd "M-k") 'move-text-up)

(global-set-key (kbd "M-e") 'grep-find)

;; (evil-define-key 'visual evil-normal-state-map (kbd "C-/") 'comment-uncomment-region)

;; (evil-define-key 'normal 'compilation-mode-map
;;   (kbd "C-h") 'evil-window-left
;;   (kbd "C-j") 'evil-window-down
;;   (kbd "C-k") 'evil-window-up
;;   (kbd "C-l") 'evil-window-right)

(global-set-key (kbd "M-r") 'recompile)

(require 'windswap)

;; (global-set-key (kbd "C-S-h") 'windswap-left)
;; (global-set-key (kbd "C-S-j") 'windswap-down)
;; (global-set-key (kbd "C-S-k") 'windswap-up)
;; (global-set-key (kbd "C-S-l") 'windswap-right)

(global-set-key (kbd "C-S-b") 'windswap-left)
(global-set-key (kbd "C-S-n") 'windswap-down)
(global-set-key (kbd "C-S-p") 'windswap-up)
(global-set-key (kbd "C-S-f") 'windswap-right)

;; (global-set-key (kbd "C-x C-h") 'previous-buffer)
;; (global-set-key (kbd "C-x C-l") 'next-buffer)

(global-set-key (kbd "C-x C-p") 'previous-buffer)
(global-set-key (kbd "C-x C-n") 'next-buffer)

(global-set-key (kbd "M-i") 'mark-sexp)

(global-set-key (kbd "C-0") 'shrink-window-horizontally)
(global-set-key (kbd "C--") 'enlarge-window-horizontally)
(global-set-key (kbd "C-=") 'enlarge-window)

(global-set-key (kbd "M-a") 'async-shell-command)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

(add-to-list 'load-path "~/.emacs.local/")
(load "~/.emacs.rc/rc.el")

;;; Appearance
(defun rc/get-default-font ()
  (cond
   ((eq system-type 'windows-nt) "Consolas-13")
   ((eq system-type 'gnu/linux) "Iosevka-20")))

(add-to-list 'default-frame-alist `(font . ,(rc/get-default-font)))

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(show-paren-mode 1)

(rc/require-theme 'gruber-darker)
;; (rc/require-theme 'zenburn)
;; (load-theme 'adwaita t)

(eval-after-load 'zenburn
  (set-face-attribute 'line-number nil :inherit 'default))

;;; ido
(rc/require 'smex 'ido-completing-read+)

(require 'ido-completing-read+)

(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)

(global-set-key (kbd "M-x") 'smex)
; (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(add-hook 'c-mode-hook (lambda ()
                         (interactive)
                         (c-toggle-comment-style -1)))

(rc/require 'haskell-mode)

(setq haskell-process-type 'cabal-new-repl)
(setq haskell-process-log t)

(add-hook 'haskell-mode-hook 'haskell-indent-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
;; (add-hook 'haskell-mode-hook 'haskell-doc-mode)
;; (add-hook 'haskell-mode-hook 'hindent-mode)

(require 'fasm-mode)
(add-to-list 'auto-mode-alist '("\\.asm\\'" . fasm-mode))

(require 'porth-mode)

(require 'noq-mode)

(require 'zig-mode)

(require 'jai-mode)

;; (rc/require 'cl-lib)
(rc/require 'magit)

(setq magit-auto-revert-mode nil)

(global-set-key (kbd "C-c m s") 'magit-status)
(global-set-key (kbd "C-c m l") 'magit-log)

(rc/require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
(global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)

(require 'dired-x)
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$"))
(setq-default dired-dwim-target t)
(setq dired-listing-switches "-alh")

(rc/require 'yasnippet)

(require 'yasnippet)

(setq yas/triggers-in-field nil)
(setq yas-snippet-dirs '("~/.emacs.snippets/"))

(yas-global-mode 1)

(setq mouse-wheel-mode nil)

(defun rc/enable-word-wrap ()
  (interactive)
  (toggle-word-wrap 1))

(add-hook 'markdown-mode-hook 'rc/enable-word-wrap)

(defun rc/turn-on-eldoc-mode ()
  (interactive)
  (eldoc-mode 1))

(add-hook 'emacs-lisp-mode-hook 'rc/turn-on-eldoc-mode)

;;; Company
(rc/require 'company)
(require 'company)

(global-company-mode)

(add-hook 'tuareg-mode-hook
          (lambda ()
            (interactive)
            (company-mode 0)))

;; (evil-define-key 'normal c-mode-map (kbd "C-]") 'lsp-find-definition)

(require 'lsp-mode)                                                 
(require 'lsp-ui)                                                   
;     UNCOMMENT IF WANT TO TURN OFF AUTOCOMPLETION HINTS 
(setq lsp-completion-enable nil)
; (setq lsp-completion-enable 1)

(setq lsp-ui-doc-enable nil
      lsp-ui-sideline-enable nil)

(setq lsp-eldoc-render-all nil)
(setq lsp-enable-symbol-highlighting nil)
(setq lsp-eldoc-render-all nil)
(setq lsp-signature-render-documentation nil)

(setq lsp-ui-doc-show-with-cursor nil)
(setq lsp-lens-enable nil)
(setq lsp-ui-sideline-show-code-actions nil)
(setq lsp-ui-sideline-enable nil)
(setq lsp-ui-sideline-show-hover nil)
(setq lsp-modeline-code-actions-enable nil)
(setq lsp-diagnostics-provider :none)
(setq lsp-ui-sideline-enable nil)
(setq lsp-modeline-diagnostics-enable nil)
(setq lsp-eldoc-enable-hover nil)
(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-ui-sideline-enable nil)

(setq lsp-ui-sideline-show-hover nil)
(setq lsp-ui-sideline-show-code-actions nil) 
(setq lsp-ui-imenu-enable nil) 
(setq lsp-ui-flycheck-enable nil) 
(setq lsp-ui-peek-enable nil) 
(setq lsp-ui-scratch-enable nil) 
(setq lsp-signature-auto-activate nil)

;(add-hook 'rust-mode-hook 'eglot-ensure)                                    
(global-eldoc-mode -1)

(add-hook 'rust-mode-hook 'lsp)
(add-hook 'c-mode-hook 'lsp)
(add-to-list 'auto-mode-alist '("\\.c\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-mode))
(add-hook 'c++-mode-hook 'lsp)
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
                                                                    
(require 'company)                                                  
(add-hook 'rust-mode-hook 'company-mode)                            
;; (define-key evil-normal-state-map (kbd "C-.") 'my-company-select-previous)
(electric-pair-mode 1)

(setq electric-pair-pairs '(
                            (?\" . ?\")
                            (?\{ . ?\})
                            (?\( . ?\))
                            ))

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
;; (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)

;; (define-key evil-normal-state-map (kbd "C-.") 'evil-repeat)

(add-to-list 'auto-mode-alist '("\\.c\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-mode))

(add-hook 'c-mode-hook
          (lambda ()
            (when (or (string-suffix-p ".c" buffer-file-name)
                      (string-suffix-p ".h" buffer-file-name))
              (c-set-style "stroustrup"))))

(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "stroustrup")))
(add-hook 'c-mode-common-hook 'company-mode)
(add-hook 'c-mode-common-hook 'ggtags-mode)
(add-hook 'c-mode-common-hook 'flycheck-mode)

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode 'rust-mode)
              (ggtags-mode 1))))

(rc/require
 'scala-mode
 'd-mode
 'yaml-mode
 'glsl-mode
 'tuareg
 'lua-mode
 'less-css-mode
 'graphviz-dot-mode
 'clojure-mode
 'cmake-mode
 'rust-mode
 'csharp-mode
 'nim-mode
 'jinja2-mode
 'markdown-mode
 'purescript-mode
 'nix-mode
 'dockerfile-mode
 'toml-mode
 'nginx-mode
 'kotlin-mode
 'go-mode
 'php-mode
 'racket-mode
 'qml-mode
 'ag
 'hindent
 'elpy
 'typescript-mode
 'rfc-mode
 'sml-mode
 )

(load "~/.emacs.shadow/shadow-rc.el" t)

(defun astyle-buffer (&optional justify)
  (interactive)
  (let ((saved-line-number (line-number-at-pos)))
    (shell-command-on-region
     (point-min)
     (point-max)
     "astyle --style=kr"
     nil
     t)
    (goto-line saved-line-number)))

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1))

(global-set-key (kbd "M-s") 'shell-command)

(use-package ivy
  :ensure t
  :config
  (ivy-mode +1))

;; (evil-set-undo-system 'undo-redo)

(require 'compile)

compilation-error-regexp-alist-alist

(add-to-list 'compilation-error-regexp-alist
             '("\\([a-zA-Z0-9\\.]+\\)(\\([0-9]+\\)\\(,\\([0-9]+\\)\\)?) \\(Warning:\\)?"
               1 2 (4) (5)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(paredit-everywhere surround evil-surround wrap-region linum-relative column-enforce-mode zig-mode zenburn-theme yaml-mode xterm-color windswap vterm typescript-mode tuareg toml-mode tide sml-mode smex smartparens scala-mode ryo-modal rust-mode rfc-mode rainbow-mode racket-mode qml-mode purescript-mode proof-general projectile powershell php-mode parinfer-rust-mode paredit org-cliplink nix-mode nim-mode nginx-mode nasm-mode multiple-cursors move-text magit-gitflow lua-mode lsp-ui kotlin-mode js2-mode jinja2-mode ido-completing-read+ hindent helm hc-zenburn-theme haskell-mode gruber-darker-theme graphviz-dot-mode go-mode glsl-mode ggtags evil emms elpy editorconfig dumb-jump dream-theme dockerfile-mode dash-functional d-mode counsel-etags cmake-mode clojure-mode anti-zenburn-theme ag)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
