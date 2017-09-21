;;; mjp2.el --- Mikes init
;;
;;; Commentary:
;;
;;; Code:

;; Some init stuff taken from: https://github.com/andschwa/.emacs.d/blob/master/init.el
;; Most from prelude

(require 'prelude-ido) ;; Super charges Emacs completion for C-x C-f and more
;(require 'prelude-helm) ;; Interface for narrowing and search
;(require 'prelude-helm-everywhere) ;; Enable Helm everywhere
(require 'prelude-company)
(require 'prelude-emacs-lisp)
(require 'prelude-js)
(require 'prelude-shell)
(require 'prelude-yaml)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-verbose t
      use-package-always-ensure t)

(eval-when-compile
  (require 'use-package))

;; 100 MB
(setq large-file-warning-threshold (* 100 1000 1000))

(set-face-font 'default "Hack-11")

;; set auto revert of buffers if file is changed externally
(global-auto-revert-mode)

;; recent files
(setq recentf-max-saved-items 256
      recentf-max-menu-items 16)
(recentf-mode)

;; turn off the beep
(setq visible-bell t)

;; packages used in init
(use-package dash)
(use-package f)

;; ;; load desktop configuration
;; (use-package init-desktop
;;   :load-path "lisp/")

;;; system specific packages
;; load Linux configuration
(use-package linux
  :ensure nil
  :load-path "lisp/"
  :if (eq system-type 'gnu/linux))

;; load OS X configurations
(use-package osx
  :ensure nil
  :load-path "lisp/"
  :if (eq system-type 'darwin))

;; load Windows configurations
(use-package windows
  :ensure nil
  :load-path "lisp/"
  :if (eq system-type 'windows-nt))

;; ido
(prelude-require-package 'ido-vertical-mode)
(ido-vertical-mode t)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

;; ;; js/json
;; (setq-default js-indent-level 2
;;               json-reformat:indent-width 2
;;               jsons-path-printer 'jsons-print-path-jq)
;; ;; sh
;; (setq-default sh-basic-offset 2
;;               sh-indentation 2)

(set-face-font 'default "Hack-11")
(setq whitespace-line-column 120) ;; limit line length (override prelude default 80)

;; set some gui stuff
(if (display-graphic-p)
  (message "> graphic mode")
  (message "> term mode"))
(setq prelude-theme (if (display-graphic-p) 'solarized-light 'solarized-dark))
(when (display-graphic-p)
  (message "setting default alist")
  (add-to-list 'default-frame-alist (cons 'height 60))
  (add-to-list 'default-frame-alist (cons 'width 120)))

;; server
(use-package server)
(unless (server-running-p) (server-start))

;; term
(add-hook 'term-mode-hook
          (lambda ()
            (prelude-off)
            ;(define-key term-raw-map (kbd "C-'") 'term-line-mode)
            ;(define-key term-mode-map (kbd "C-'") 'term-char-mode)
            (define-key term-raw-map (kbd "C-y") 'term-paste)))

;; Github flavored markdown
(use-package markdown-mode+)

(setq dired-listing-switches "-laX")

(setq ping-program-options '("-c" "10"))

(setq diff-switches "-u")

(setq sort-fold-case t)

;; I like clocks
(display-time-mode 1)
(setq display-time-default-load-average nil)


;; paredit for parinfer
(unless (package-installed-p 'paredit)
  (package-refresh-contents)
  (package-install 'paredit))

(unless (package-installed-p 'parinfer)
  (package-refresh-contents)
  (package-install 'parinfer))
;; parinfer
(use-package parinfer
  :ensure t
  :bind
  (("C-," . parinfer-toggle-mode))
  :init
  (progn
    (setq parinfer-extensions
          '(defaults       ; should be included.
             pretty-parens  ; different paren styles for different modes.
             paredit        ; Introduce some paredit commands.
             smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
             smart-yank))   ; Yank behavior depend on mode.
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)))

;; ;; Install SHM & Intero - instead of prelude-haskell - http://commercialhaskell.github.io/intero/

;; (add-to-list 'load-path (concat prelude-personal-dir "/../../src/structured-haskell-mode/elisp"))
;; (unless (package-installed-p 'shm)
;;   (package-refresh-contents)
;;   (package-install 'shm))
;; (use-package shm)

;; ;; (setq haskell-process-type 'ghci
;; ;;       haskell-process-path-stack "/Users/mikep/play/reflex-platform/try-reflex"
;; ;;       haskell-process-args-stack-ghci '("--command" "ghci"))

;; ;; https://github.com/travisbhartwell/nix-emacs
;; ;; let ghc find stuff in the nix sandbox
;; (add-to-list 'load-path (concat prelude-personal-dir "/../../src/nix-emacs"))
;; (require 'nix-sandbox)
;; ;; (setq haskell-process-wrapper-function
;; ;;       (lambda (args) (apply 'nix-shell-command (nix-current-sandbox) args)))

;; (add-to-list 'load-path (concat prelude-personal-dir "/../../src/dante"))
;; (use-package dante
;;   :ensure t
;;   :after haskell-mode
;;   :commands 'dante-mode)
;; (add-hook 'dante-mode-hook
;;             '(lambda () (flycheck-add-next-checker 'haskell-dante
;;                                                    '(warning . haskell-hlint))))


;; (setq haskell-process-type 'ghci
;;       haskell-process-path-stack (concat prelude-personal-dir "/../../play/reflex-platform/nix-shell")
;;       haskell-process-args-stack-ghci '("--command" "ghci"))
;; (setq haskell-process-type 'ghci
;;       haskell-process-path-ghci (concat prelude-personal-dir "/../../play/reflex-platform/nix-shell")
;;       haskell-process-args-ghci '("--command" "ghci"))


;;https://news.ycombinator.com/item?id=7905511
;; (setq haskell-process-type 'ghci
;;       haskell-process-path-ghci "/home/ben/.nix-profile/bin/nix-shell"
;;       haskell-process-args-ghci '("-I" "." "shell.nix" "--pure" "--command" "cabal configure; cabal repl"))


;; (unless (package-installed-p 'intero)
;;   (package-refresh-contents)
;;   (package-install 'intero))
;; (use-package intero
;;   :init
;;   (progn
;;     (add-hook 'haskell-mode-hook 'intero-mode)
;;     (add-hook 'haskell-mode-hook 'structured-haskell-mode))
;;   :config
;;   (progn
;;     (subword-mode +1)
;;     (eldoc-mode +1)
;;     (haskell-indentation-mode nil)      ; for SHM
;;     (interactive-haskell-mode +1)))


(provide 'mjp2)
;;; mjp2 ends here

;; (when t ;window-system
;;   (require 'prelude-packages)
;;   (scroll-bar-mode -1)
;;   (prelude-require-package 'solarized-theme)
;;   (setq prelude-theme 'solarized-light))

;(prelude-require-packages '(solarized-theme))

;; Solarized
;; https://github.com/sellout/emacs-color-theme-solarized/pull/187
;; (setq color-themes '())
;; (use-package color-theme-sanityinc-solarized
;;   :config
;;   (load-theme 'solarized t)
;;   :init
;;   (add-hook 'after-make-frame-functions
;;       (lambda (frame)
;;         (let ((mode (if (display-graphic-p frame) 'light 'dark))
;;               (set-frame-parameter frame 'background-mode mode)
;;               (set-terminal-parameter frame 'background-mode mode))
;;           (enable-theme 'solarized)))))

;; Solarized
;; https://github.com/sellout/emacs-color-theme-solarized/pull/187
;; (setq color-themes '())
;; (use-package color-theme-solarized
;;   :config
;;   (load-theme 'solarized t)
;;   :init
;;   (add-hook 'after-make-frame-functions
;;       (lambda (frame)
;;         (let ((mode (if (display-graphic-p frame) 'light 'light))
;;               (set-frame-parameter frame 'background-mode mode)
;;               (set-terminal-parameter frame 'background-mode mode))
;;           (enable-theme 'solarized)))))
