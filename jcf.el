;; -*- mode: emacs-lisp -*-

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     ansible
     auto-completion
     better-defaults
     c-c++
     (clojure :variables
              cider-repl-history-file
              (expand-file-name ".cider-history" spacemacs-cache-directory)
              cider-repl-history-size 1000
              cider-repl-wrap-history t
              clojure-enable-fancify-symbols nil)
     dash
     django
     dockerfile
     elixir
     emacs-lisp
     erc
     erlang
     evernote
     extra-langs
     (git :variables
          git-magit-status-fullscreen t
          magit-repository-directories '("~/Code/"))
     github
     go
     gtags
     haskell
     html
     java
     javascript
     latex
     markdown
     ocaml
     (org :variables
          org-enable-github-support t
          org-enable-pandoc-support t)
     (osx :variables
          osx-use-option-as-meta nil)
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     pandoc
     puppet
     python
     racket
     restclient
     ruby
     ruby-on-rails
     rust
     scala
     shell-scripts
     sql
     typescript
     syntax-checking
     vagrant
     version-control
     )
   dotspacemacs-additional-packages '(
                                      lfe-mode
                                      slamhound
                                      ob-lfe
                                      )
   dotspacemacs-excluded-packages '()
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  (setq-default
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '(recents projects)
   dotspacemacs-themes '(zenburn leuven)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-command-key ":"
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-use-ido nil
   dotspacemacs-enable-paste-micro-state nil
   dotspacemacs-guide-key-delay 0.6
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-smartparens-strict-mode t
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("pt" "ag" "ack" "grep")
   dotspacemacs-default-package-repository nil
   ))

(defun dotspacemacs/config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."
  (setq
   powerline-default-separator nil
   user-full-name "James Conroy-Finn"
   user-mail-address "james@logi.cl"
   vc-follow-symlinks t)

  (evil-leader/set-key ":" 'helm-M-x)

  ;; Set Pandoc export options when org-mode fires up
  (when org-enable-pandoc-support
    (add-hook
     'org-mode-hook
     (lambda ()
       (push '(email-obfuscation . "references") org-pandoc-options))))

  ;; Make evil-mode up/down operate in screen lines instead of logical lines
  (define-key evil-motion-state-map "j" 'evil-next-visual-line)
  (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
  (define-key evil-visual-state-map "j" 'evil-next-visual-line)
  (define-key evil-visual-state-map "k" 'evil-previous-visual-line)

  ;; Quit cider-test-mode when we press `q'
  (evil-declare-key 'normal cider-test-report-mode-map
    "q" 'cider-popup-buffer-quit-function)

  ;; Start an interactive Elisp REPL
  (evil-leader/set-key "aI" 'ielm)

  ;; Delete other windows giving focus to the current
  (evil-leader/set-key "wf" 'delete-other-windows)

  (when (system-is-mac)
    ;; Take back the option key so we can type #
    (setq mac-option-key-is-meta nil)
    (setq mac-command-key-is-meta nil)
    (setq mac-command-modifier 'super)
    (setq mac-option-modifier nil))

  ;; Enable Babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((awk . t)
     (clojure . t)
     (ditaa . t)
     (dot . t)
     (gnuplot . t)
     (haskell . t)
     (js . t)
     (latex . t)
     (lisp . t)
     (ruby . t)
     (python . t)
     (R . t)
     (sass . t)
     (scala . t)
     (sh . t)))

  ;; Use cider to evaluate Clojure
  (require 'ob-clojure)
  (setq org-babel-clojure-backend 'cider)

  ;; Use escape to quit, and not as a meta-key.
  (defun jcf-minibuffer-keyboard-quit ()
    "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))

  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'jcf-minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'jcf-minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'jcf-minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'jcf-minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'jcf-minibuffer-keyboard-quit)

  ;; Add magic requires that modify the namespace if a require is missing when
  ;; it's alias is used.
  (add-hook
   'clj-refactor-mode-hook
   (lambda ()
     (dolist (pair '(("component" . "com.stuartsierra.component")
                     ("medley" . "medley.core")
                     ("s" . "schema.core")))
       (add-to-list 'cljr-magic-require-namespaces pair)))))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-case-fold-search nil)
 '(ahs-default-range (quote ahs-range-whole-buffer))
 '(ahs-idle-interval 0.25)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil)
 '(paradox-github-token t)
 '(ring-bell-function (quote ignore) t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foreground "#DCDCCC" :background "#3F3F3F"))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(evil-search-highlight-persist-highlight-face ((t (:background "Black")))))
