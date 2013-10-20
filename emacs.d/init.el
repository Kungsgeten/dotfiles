;; Install packages
;;;;;;;;;;;;;;;;;;;

(require 'package)
(package-initialize)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(defvar my-packages '(ace-jump-mode
                      anzu
                      auctex
                      auto-complete
                      bookmark+
                      deft
                      dired+ dired-details+
                      diminish
                      expand-region
                      flycheck
                      helm helm-spotify helm-descbinds helm-orgcard
                      key-chord
		      lua-mode
                      magit
                      multiple-cursors
                      smartparens
                      slime rainbow-delimiters
                      undo-tree
                      volatile-highlights
                      yascroll
                      yasnippet
                      zenburn-theme
                      zencoding-mode rainbow-mode)
  "List of packages to install at launch")

(if system-type 'gnu/linux
  (setq my-packages (append my-packages '(ac-nrepl nrepl clojure-mode clojure-cheatsheet))))

(defun my-missing-packages ()
  (let (missing-packages)
    (dolist (package my-packages (reverse missing-packages))
      (or (package-installed-p package)
(push package missing-packages)))))

(defun ensure-my-packages ()
  (let ((missing (my-missing-packages)))
    (when missing
      ;; Check for new packages (package versions)
      (package-refresh-contents)
      ;; Install the missing packages
      (mapc (lambda (package)
(when (not (package-installed-p package))
(package-install package)))
missing)
      ;; Close the compilation log.
      (let ((compile-window (get-buffer-window "*Compile-Log*")))
(if compile-window
(delete-window compile-window))))))

(ensure-my-packages)


;; Emacs initialization
;;;;;;;;;;;;;;;;;;;;;;;

(setq frame-title-format '("emacs"))
(if system-type 'gnu/linux
  (set-frame-font "Terminus-10"))

(let ((default-directory "~/.emacs.d/plugins/"))
  (normal-top-level-add-to-load-path '("" "ledger")))

(load "ledger-mode")

(package-initialize)
(load-theme 'zenburn t)

(setq make-backup-files t)
(setq version-control t
      backup-by-copying t
      delete-old-versions t
      kept-old-versions 6
      kept-new-versions 9)
(setq backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))

;; Personal info
;;;;;;;;;;;;;;;;

(setq user-mail-address "sjostrand.erik@gmail.com")
(setq user-full-name "Erik Sjöstrand")

;; General configuration
;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'after-init-hook 'yas-global-mode)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq-default indent-tabs-mode nil)
(tool-bar-mode -1)
(menu-bar-mode -1)
(column-number-mode t)
(blink-cursor-mode 0)
(show-paren-mode t)
(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen 1)
(fset 'yes-or-no-p 'y-or-n-p)
(global-hl-line-mode +1)
(pending-delete-mode 1)

(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)

(scroll-bar-mode -1)
(require 'yascroll)
(global-yascroll-bar-mode 1)
;; make the fringe (gutter) smaller
;; the argument is a width in pixels (the default is 8)
(if (fboundp 'fringe-mode)
    (fringe-mode 4))

(setq
 scroll-margin 5
 scroll-conservatively 100000
 scroll-preserve-screen-position 1)

;; diminish keeps the modeline tidy
(require 'diminish)

(require 'anzu)
(global-anzu-mode +1)
(diminish 'anzu-mode)

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(global-set-key [remap goto-line] 'goto-line-with-feedback)

(helm-mode t)
(helm-descbinds-mode t)
(diminish 'helm-mode)

;; saveplace remembers your location in a file when saving files
(require 'saveplace)
(setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
;; activate it for all buffers
(setq-default save-place t)

;; savehist keeps track of some history
(require 'savehist)
(setq savehist-additional-variables
      ;; search entries
      '(search ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (expand-file-name "savehist" user-emacs-directory))
(savehist-mode +1)

;; save recent files
(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" user-emacs-directory)
      recentf-max-saved-items 500
      recentf-max-menu-items 15)
(recentf-mode +1)

(global-auto-revert-mode t)

;; sensible undo
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

;; enable winner-mode to manage window configurations
(winner-mode +1)

;; Text editiing
;;;;;;;;;;;;;;;;

; enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; enabled change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; = smart open and line, join line and line beginning

(defun smart-open-line ()
  "Insert an empty line after the current line.
   Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(global-set-key [(shift return)] 'smart-open-line)
(global-set-key "\C-o" 'smart-open-line)

(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

(defun smart-line-beginning ()
  "Move point to the beginning of text
  on the current line; if that is already
  the current position of point, then move
  it to the beginning of the line."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line-text)
    (when (eq pt (point))
      (beginning-of-line))))

(global-set-key "\C-a" 'smart-line-beginning)
;; ==

(require 'expand-region)
(global-set-key (kbd "C-0") 'er/expand-region)
(global-set-key (kbd "C-9") 'mc/mark-next-like-this)
(global-set-key (kbd "C-8") 'mc/mark-all-like-this)

;; = Increment/decrement integer

(require 'thingatpt)

(defun thing-at-point-goto-end-of-integer ()
  "Go to end of integer at point."
  (let ((inhibit-changing-match-data t))
    ;; Skip over optional sign
    (when (looking-at "[+-]")
      (forward-char 1))
    ;; Skip over digits
    (skip-chars-forward "[[:digit:]]")
    ;; Check for at least one digit
    (unless (looking-back "[[:digit:]]")
      (error "No integer here"))))
(put 'integer 'beginning-op 'thing-at-point-goto-end-of-integer)

(defun thing-at-point-goto-beginning-of-integer ()
  "Go to end of integer at point."
  (let ((inhibit-changing-match-data t))
    ;; Skip backward over digits
    (skip-chars-backward "[[:digit:]]")
    ;; Check for digits and optional sign
    (unless (looking-at "[+-]?[[:digit:]]")
      (error "No integer here"))
    ;; Skip backward over optional sign
    (when (looking-back "[+-]")
      (backward-char 1))))
(put 'integer 'beginning-op 'thing-at-point-goto-beginning-of-integer)

(defun thing-at-point-bounds-of-integer-at-point ()
  "Get boundaries of integer at point."
  (save-excursion
    (let (beg end)
      (thing-at-point-goto-beginning-of-integer)
      (setq beg (point))
      (thing-at-point-goto-end-of-integer)
      (setq end (point))
      (cons beg end))))
(put 'integer 'bounds-of-thing-at-point 'thing-at-point-bounds-of-integer-at-point)

(defun thing-at-point-integer-at-point ()
  "Get integer at point."
  (let ((bounds (bounds-of-thing-at-point 'integer)))
    (string-to-number (buffer-substring (car bounds) (cdr bounds)))))
(put 'integer 'thing-at-point 'thing-at-point-integer-at-point)

(defun increment-integer-at-point (&optional inc)
  "Increment integer at point by one.
     
     With numeric prefix arg INC, increment the integer by INC amount."
  (interactive "p")
  (let ((inc (or inc 1))
        (n (thing-at-point 'integer))
        (bounds (bounds-of-thing-at-point 'integer)))
    (delete-region (car bounds) (cdr bounds))
    (insert (int-to-string (+ n inc)))))

(defun decrement-integer-at-point (&optional dec)
  "Decrement integer at point by one.
     
     With numeric prefix arg DEC, decrement the integer by DEC amount."
  (interactive "p")
  (increment-integer-at-point (- (or dec 1))))

(global-set-key (kbd "C-c +") 'increment-integer-at-point)
(global-set-key (kbd "C-c -") 'decrement-integer-at-point)
;; ===

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))
(global-set-key (kbd "C-c e") 'eval-and-replace)

(global-set-key(kbd"RET")'newline-and-indent)

(defun kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (point) (mark))
    (backward-kill-word 1)))
(define-key global-map "\C-w" 'kill-region-or-backward-word)

;; Org-mode
;;;;;;;;;;;

;; = Setup

(require 'org-install)
(require 'org-habit)
(require 'org-protocol)

(setq org-entities-user '(("space" "\\ " nil " " " " " " " ")))

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-log-done t)
(setq org-completion-use-ido t)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

(setq org-directory "~/org/")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-use-fast-todo-selection t)
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-inbox-for-pull "~/org/mobileorg.org")
(setq org-mobile-files '("~/org/gtd.org"
                         "~/org/someday.org"))
(setq org-mobile-force-id-on-agenda-items nil)

(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(setq org-agenda-files '("~/org/gtd.org"))
(setq org-refile-targets '(("gtd.org" :maxlevel . 1)))

(find-file "~/org/gtd.org")

;; = Shortcuts

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cb" 'org-iswitchb)

;; = Capture

(setq org-capture-templates
      '(
        ("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
         "* TODO %? %^g \n  %i\n  %a \n Added: %u\n\n")
        ("s" "Someday" entry (file+headline "~/org/someday.org" "Inbox")
         "* TODO %? %^g \n  %i\n  %a \n Added: %u\n\n")
        ("i" "Inbox" entry (file+headline "~/org/gtd.org" "Inbox")
         "* %?\n %a \n Added: %u\n\n  %i")
        ("b" "Bookmark" entry (file "~/org/bookmarks.org")
         "* [[%:link][%^{Title|%:description}]]   %^g \n  %?\n  Added: %u\n\n  %i"
         :empty-lines 1)
        ))

;; = TODO tags

(setq org-agenda-custom-commands
      '(("h" "Todo lists"
         ((agenda)
          (tags-todo "SKÖVDE")
          (tags-todo "HIS")
          (tags-todo "TOWN")
          (tags-todo "COMPUTER")
          (tags-todo "WINDOWS")
          (tags-todo "LINUX")
          (tags-todo "READING")
          (tags-todo "BORÅS")
          (tags-todo "PHONE")
          (tags-todo "GÖTEBORG")))))

;; Deft
;;;;;;;

(setq deft-extension "txt")
(setq deft-text-mode 'org-mode)
(global-set-key [f9] 'deft)
(setq deft-directory "~/org/deft")
(setq deft-use-filename-as-title t)

;; LaTeX
;;;;;;;;

(setq TeX-auto-save t) 
(setq TeX-parse-self t) 
(setq TeX-save-query nil)
(setq TeX-PDF-mode t)

(setq-default TeX-master nil)
(setq-default TeX-master "master")

;; TODO: Change for Windows
(setq TeX-view-program-list '(("zathura" "zathura %o")))
(setq TeX-view-program-selection '((output-pdf "zathura")))

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'orgtbl-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-buffer)
(setq reftex-plug-into-AUCTeX t)

;; Programming
;;;;;;;;;;;;;;

(require 'smartparens-config)

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

;; saner regex syntax
(require 're-builder)
(setq reb-re-syntax 'string)


(defun prelude-font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):\\)"
          1 font-lock-warning-face t))))

(defun prelude-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t))

(defun prelude-prog-mode-defaults ()
  "Default coding hook, useful with any programming language."
  (smartparens-mode +1)
  (prelude-local-comment-auto-fill)
  (prelude-font-lock-comment-annotations))

(setq prelude-prog-mode-hook 'prelude-prog-mode-defaults)

(require 'which-func)
(add-to-list 'which-func-modes 'ruby-mode)
(which-function-mode 1)

(add-hook 'prog-mode-hook (lambda ()
                            (run-hooks 'prelude-prog-mode-hook)))

;; AutoHotKey
;;;;;;;;;;;;;

(autoload 'xahk-mode "xahk-mode" "Load xahk-mode for editing AutoHotkey scripts." t)
(add-to-list 'auto-mode-alist '("\\.ahk\\'" . xahk-mode))
(defalias 'ahk-mode 'xahk-mode) ; make it easier to remember.

;; CSS
;;;;;;

(add-hook 'css-mode-hook (lambda () (rainbow-mode +1)))
(eval-after-load 'css-mode (setq css-indent-offset 2))

;; LISP
;;;;;;;

;; Lisp configuration
(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)

;; a great lisp coding hook
(defun prelude-lisp-coding-defaults ()
  (smartparens-mode t)
  (rainbow-delimiters-mode +1))

(setq prelude-lisp-coding-hook 'prelude-lisp-coding-defaults)

(defun prelude-interactive-lisp-coding-defaults ()
  (smartparens-mode t)
  (rainbow-delimiters-mode +1))

(setq prelude-interactive-lisp-coding-hook 'prelude-interactive-lisp-coding-defaults)


(setq slime-lisp-implementations
      '((ccl ("ccl"))
        (clisp ("clisp" "-q"))
        (cmucl ("cmucl" "-quiet"))
        (sbcl ("sbcl" "--noinform") :coding-system utf-8-unix)))

(setq slime-default-lisp 'clisp)

(add-hook 'lisp-mode-hook (lambda () (run-hooks 'prelude-lisp-coding-hook)))
(add-hook 'emacs-lisp-mode-hook (lambda () (run-hooks 'prelude-lisp-coding-hook)))
(add-hook 'slime-repl-mode-hook (lambda () (run-hooks 'prelude-interactive-lisp-coding-hook)))

(defun prelude-start-slime ()
  "Start SLIME unless it's already running."
  (unless (slime-connected-p)
    (save-excursion (slime))))

;; start slime automatically when we open a lisp file
(add-hook 'slime-mode-hook 'prelude-start-slime)

(eval-after-load "slime"
  '(progn
     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol
           slime-fuzzy-completion-in-place t
           slime-enable-evaluate-in-emacs t
           slime-autodoc-use-multiline-p t)

     (define-key slime-mode-map (kbd "TAB") 'slime-indent-and-complete-symbol)
     (define-key slime-mode-map (kbd "C-c i") 'slime-inspect)
     (define-key slime-mode-map (kbd "C-c C-s") 'slime-selector)))

;; Dired and bookmarks
;;;;;;;;;;;;;;;;;;;;;;

(setq bmkp-prompt-for-tags-flag 1)
(bookmark-bmenu-list)
(switch-to-buffer "*Bookmark List*")

;; dired - reuse current buffer by pressing 'a'
(put 'dired-find-alternate-file 'disabled nil)

;; always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

(require 'dired+)

;; dired, better searching (filenames)
(setq dired-isearch-filenames t)

(put 'dired-find-alternate-file 'disabled nil)

(require 'dired-x)
(setq-default dired-omit-files-p t) ;; this is buffer-local variable
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$")) ;; dot-files are omitted

(setq dired-dwim-target t)

(require 'dired-details)
(setq-default dired-details-hidden-string nil)
(dired-details-install)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)


(defun open-in-external-app ()
  "Open the current file or dired marked files in external app."
  (interactive)
  (let ( doIt
         (myFileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           (t (list (buffer-file-name))) ) ) )
    
    (setq doIt (if (<= (length myFileList) 5)
                   t
                 (y-or-n-p "Open more than 5 files?") ) )
    
    (when doIt
      (cond
       ((string-equal system-type "windows-nt")
        (mapc (lambda (fPath) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t)) ) myFileList)
        )
       ((string-equal system-type "darwin")
        (mapc (lambda (fPath) (shell-command (format "open \"%s\"" fPath)) )  myFileList) )
       ((string-equal system-type "gnu/linux")
        (mapc (lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fPath)) ) myFileList) ) ) ) ) )

(global-set-key (kbd "<C-return>") 'open-in-external-app)


;; Buffer manipulation
;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

(defun djcb-find-file-as-root ()
  "Like `ido-find-file, but automatically edit the file with
    root-privileges (using tramp/sudo), if the file is not writable by
    user."
  (interactive)
  (let ((file (ido-read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo:root@localhost:" file)))
    (find-file file)))
;; or some other keybinding...
(global-set-key (kbd "C-x F") 'djcb-find-file-as-root)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(setq windmove-wrap-around t)

(defun open-with ()
  "Simple function that allows us to open the underlying
    file of a buffer in an external program."
  (interactive)
  (when buffer-file-name
    (shell-command (concat
                    (if (eq system-type 'darwin)
                        "open"
                      (read-shell-command "Open current file with: "))
                    " "
                    buffer-file-name))))

(global-set-key (kbd "C-c o") 'open-with)


(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)))))))

(global-set-key (kbd "C-c r")  'rename-file-and-buffer)

(global-set-key (kbd "<C-S-iso-lefttab>") 'previous-buffer)
(global-set-key (kbd "<C-tab>") 'next-buffer)

;; == Toggle buffers

(defun next-user-buffer ()
  "Switch to the next user buffer.
    User buffers are those whose name does not start with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setq i (1+ i)) (next-buffer) )))

(defun previous-user-buffer ()
  "Switch to the previous user buffer.
    User buffers are those whose name does not start with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setq i (1+ i)) (previous-buffer) )))

(defun next-emacs-buffer ()
  "Switch to the next emacs buffer.
    Emacs buffers are those whose name starts with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (not (string-match "^*" (buffer-name))) (< i 50))
      (setq i (1+ i)) (next-buffer) )))

(defun previous-emacs-buffer ()
  "Switch to the previous emacs buffer.
    Emacs buffers are those whose name starts with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (not (string-match "^*" (buffer-name))) (< i 50))
      (setq i (1+ i)) (previous-buffer) )))

(global-set-key "\M-n" 'next-user-buffer)
(global-set-key "\M-p" 'previous-user-buffer)
(global-set-key (kbd "<C-prior>") 'previous-emacs-buffer) ; Ctrl+Shift+PageUp
(global-set-key (kbd "<C-next>") 'next-emacs-buffer) ; Ctrl+Shift+PageDown

;; Key chords
;;;;;;;;;;;;;

(require 'key-chord)

(key-chord-define-global "jj" 'ace-jump-word-mode)
(key-chord-define-global "jl" 'ace-jump-line-mode)
(key-chord-define-global "jp" 'ace-jump-char-mode)
(key-chord-define-global "uu" 'undo-tree-visualize)

(key-chord-mode +1)

;; Spelling
;;;;;;;;;;;

(setq-default ispell-program-name "aspell")
(setq ispell-list-command "list")

;; The WWW
;;;;;;;;;;

(if (string-equal system-type "gnu/linux") 
    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "/home/ess/bin/conkeror.sh"))

(global-set-key (kbd "C-c u") 'browse-url)

;; Misc
;;;;;;;

(require 'steam)
