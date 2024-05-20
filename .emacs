(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(pixel-scroll-precision-mode 1)

(setq x-super-keysym 'meta)
(setq x-alt-keysym 'capslock)

;; (require 'corfu)
;; (setq corfu-auto t
;;       corfu-quit-no-match 'separator)
;; (use-package corfu
;;   :custom
;;   (corfu-cycle t)          
;;   (corfu-preselect 'prompt)
;;   :bind
;;   (:map corfu-map
;;         ("C-n" . corfu-next)
;;         ("C-p" . corfu-previous)
;;         ([tab] . corfu-complete)
;;         ([ret] . corfu-complete))
;;   :init
;;   (global-corfu-mode))

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;; (setq company-idle-delay
;;       (lambda () (if (company-in-string-or-comment) nil 0.1)))
(setq company-tooltip-minimum-width 0)
(setq completion-show-inline 1)
(setq company-selection-wrap-around t)

(setq company-idle-delay nil)
(setq completion-auto-help -1)
(setq ac-auto-show-menu nil)

;; (global-set-key (kbd "<tab>") #'company-indent-or-complete-common)
(global-set-key (kbd "TAB") 'tab-to-tab-stop)
(global-set-key (kbd "M-<tab>") 'company-complete-selection)

(use-package surround :ensure t)

(defun duplicate-line()
  "Dup curr. line"    
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  (message "duped!"))

(defun copy-line ()
  "Copy the current line."
  (interactive)
  (kill-ring-save (line-beginning-position) (line-end-position))
  (message "copied"))

(defun insert-line-above-and-jump ()
  (interactive)
  (move-beginning-of-line nil)
  (newline)
  (previous-line))

(defun select-line ()
  "select."
  (interactive)
  (move-beginning-of-line nil)
  (push-mark nil t t)
  (move-end-of-line nil))

(defun insert-line-below-and-jump ()
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(global-set-key (kbd "C-M-o") 'insert-line-above-and-jump)
;; (global-set-key (kbd "M-<return>") 'insert-line-above-and-jump)
(global-set-key (kbd "C-<return>") 'insert-line-below-and-jump)
(global-set-key (kbd "C-c C-o") 'select-line)
(global-set-key (kbd "C-c C-m") 'duplicate-line)

(global-set-key (kbd "M-2") 'other-window)
(global-set-key (kbd "M-q") 'find-file)
(global-set-key (kbd "M-`") 'ivy-switch-buffer)
(global-set-key (kbd "M-s") 'shell-command)
(global-set-key (kbd "M-e") 'grep-find)
(global-set-key (kbd "M-r") 'recompile)
(global-set-key (kbd "M-i") 'mark-sexp)
(global-set-key (kbd "M-a") 'async-shell-command)
(global-set-key (kbd "M-o") 'insert-line-above-and-jump)

(delete-selection-mode 1)

(global-set-key (kbd "C-c C-k") 'kill-whole-line)
(global-set-key (kbd "C-c C-<backspace>") 'kill-whole-line)

;; (global-whitespace-mode 1)
;; (setq-default show-trailing-whitespace nil)

(add-hook 'prog-mode-hook (lambda () (setq display-line-numbers 'relative)))

(setq haskell-interactive-popup-errors nil)

(defun set-rectangle-cursor ()
  (setq cursor-type 'box))

(set-default 'cursor-type 'box)
(define-key global-map (kbd "M-h") #'windmove-left)
(define-key global-map (kbd "M-j") #'windmove-down)
;; (define-key global-map (kbd "M-k") #'windmove-up)
(define-key global-map (kbd "M-l") #'windmove-right)

(define-key global-map (kbd "C-?") #'comment-or-uncomment-region)

(require 'move-text)
(global-set-key (kbd "M-n") 'move-text-down)
(global-set-key (kbd "M-p") 'move-text-up)

(require 'windswap)
(global-set-key (kbd "C-S-b") 'windswap-left)
(global-set-key (kbd "C-S-n") 'windswap-down)
(global-set-key (kbd "C-S-p") 'windswap-up)
(global-set-key (kbd "C-S-f") 'windswap-right)

(global-set-key (kbd "C-c p") 'previous-buffer)
(global-set-key (kbd "M-1")   'previous-buffer)
(global-set-key (kbd "M-3")   'next-buffer)
(global-set-key (kbd "C-c n") 'next-buffer)

(global-set-key (kbd "C-0") 'shrink-window-horizontally)
(global-set-key (kbd "C--") 'enlarge-window-horizontally)
(global-set-key (kbd "C-=") 'enlarge-window)
(global-set-key (kbd "M-.") 'lsp-find-definition)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

(add-to-list 'load-path "~/.emacs.local/")
(load "~/.emacs.rc/rc.el")

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

(setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)

;; (set-frame-font "Ubuntu Mono-20" nil t)

(rc/require-theme 'gruber-darker)
;; (custom-set-faces
;;   '(font-lock-type-face ((t (:foreground "#FFDD33" :weight bold)))))

;; (rc/require-theme 'zenburn)
;; (custom-set-faces
;;  '(font-lock-constant-face ((t (:foreground "#96A6C8"))))
;;  '(font-lock-function-name-face ((t (:foreground "#94BFF3"))))
;;  '(font-lock-keyword-face ((t (:foreground "#F0DFAF" :weight bold))))
;;  '(font-lock-reference-face ((t (:foreground (\, "#DCDCCC")))))
;;  '(font-lock-type-face ((t (:foreground "#F0DFAF" :weight bold))))
;;  '(font-lock-variable-name-face ((t (:foreground "#DCDCCC")))))
;; (defun rust-unsafe ()
;;   (font-lock-add-keywords nil
;;     '(("\\<\\(unsafe\\)\\>"
;;        1 '(:foreground "ff4f58") t))))
;; (add-hook 'rust-mode-hook 'rust-unsafe)

(rc/require 'smex 'ido-completing-read+)

(require 'ido-completing-read+)

(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)

(global-set-key (kbd "M-x") 'smex)
(add-hook 'c-mode-hook (lambda ()
                       (interactive)
                       (setq c-basic-offset 4)
                       (setq tab-width 4)
                       (c-set-style "linux")
                       (c-toggle-comment-style -1)))

(rc/require 'haskell-mode)
(setq haskell-process-type 'cabal-new-repl)
(setq haskell-process-log t)

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(setq haskell-indentation-mode nil)
(setq-default haskell-indent-offset 2)

(require 'fasm-mode)
(add-to-list 'auto-mode-alist '("\\.asm\\'" . fasm-mode))
(require 'porth-mode)

(require 'zig-mode)

(add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode))
(add-to-list 'auto-mode-alist '("\\.zon\\'" . zig-mode))

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

;; (global-corfu-mode 1)
(global-company-mode 1)
(global-eldoc-mode -1)
 
(require 'lsp-mode)                                                 
(require 'lsp-ui)                                                   
;     UNCOMMENT IF WANT TO TURN OFF AUTOCOMPLETION HINTS 
; (setq lsp-completion-enable nil)
(setq lsp-completion-enable 1)

(setq lsp-ui-doc-enable nil)
(setq lsp-ui-sideline-enable nil)

(setq lsp-eldoc-render-all nil)
(setq lsp-eldoc-render-all nil)
(setq lsp-eldoc-enable-hover nil)
(setq lsp-enable-symbol-highlighting nil)
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
(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-ui-sideline-enable nil)

(setq lsp-ui-sideline-show-hover nil)
(setq lsp-ui-sideline-show-code-actions nil) 
(setq lsp-ui-imenu-enable nil) 
(setq lsp-ui-flycheck-enable nil) 
(setq lsp-ui-peek-enable nil) 
(setq lsp-ui-scratch-enable nil) 
(setq lsp-signature-auto-activate nil)

(add-hook 'c-mode-hook 'lsp)
(add-hook 'rust-mode-hook 'lsp)
(add-to-list 'auto-mode-alist '("\\.c\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-mode))
(add-hook 'c++-mode-hook 'lsp)
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
                                                                    
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

(add-to-list 'auto-mode-alist '("\\.c\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-mode))
(add-hook 'c-mode-hook
          (setq c-basic-offset 4
                tab-width 4
                indent-tabs-mode nil)
          (lambda ()
            (when (or (string-suffix-p ".c" buffer-file-name)
                      (string-suffix-p ".h" buffer-file-name))
              (c-set-style "stroustrup"))))

(with-eval-after-load 'lsp-mode
(setq lsp-clients-clangd-args '("--clang-tidy"
                                "--completion-style=detailed"
                                "--header-insertion=iwyu"
                                "--header-insertion-decorators"
                                "--fallback-style=llvm"
                                "--offset-encoding=utf-8"
                                "--pch-storage=memory")))

(add-hook 'c-mode-common-hook (lambda () (c-set-style "stroustrup")))
(add-hook 'c-mode-common-hook 'flycheck-mode)
(add-hook 'c-mode-common-hook (lambda () (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode 'rust-mode) (lsp-mode 1))))

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

(use-package ivy
  :ensure t
  :config
  (ivy-mode +1))

(require 'compile)

(load "~/.emacs.rc/mojo.el")
(add-to-list 'auto-mode-alist '("\\.mojo\\'" . mojo-mode))

(add-to-list 'compilation-error-regexp-alist
             '("\\([a-zA-Z0-9\\.]+\\)(\\([0-9]+\\)\\(,\\([0-9]+\\)\\)?) \\(Warning:\\)?"
               1 2 (4) (5)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
    '("e27c9668d7eddf75373fa6b07475ae2d6892185f07ebed037eedf783318761d7" "d19f00fe59f122656f096abbc97f5ba70d489ff731d9fa9437bac2622aaa8b89" "f366d4bc6d14dcac2963d45df51956b2409a15b770ec2f6d730e73ce0ca5c8a7" default))
 '(package-selected-packages
    '(company forge magit-gh-pulls mojo-mode erosiond-theme edit-server surround evil-surround wrap-region column-enforce-mode zenburn-theme yaml-mode xterm-color windswap vterm typescript-mode tuareg toml-mode tide sml-mode smex smartparens scala-mode ryo-modal rust-mode rfc-mode rainbow-mode racket-mode qml-mode purescript-mode proof-general projectile powershell php-mode parinfer-rust-mode org-cliplink nix-mode nim-mode nginx-mode nasm-mode multiple-cursors move-text magit-gitflow lua-mode lsp-ui kotlin-mode js2-mode jinja2-mode ido-completing-read+ hindent helm hc-zenburn-theme haskell-mode gruber-darker-theme graphviz-dot-mode go-mode glsl-mode evil emms editorconfig dumb-jump dream-theme dockerfile-mode dash-functional d-mode counsel-etags cmake-mode clojure-mode anti-zenburn-theme ag)))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
