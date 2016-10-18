;;; init.el --- Neil Woods's GNU Emacs init file.
;;
;; Filename: init.el
;; Description: GNU Emacs initialisation.
;; Author: Neil Woods
;; Created: Fri Oct 14 19:58:40 2016 (+0100)
;; Version: 20161015
;; Package-Requires: ()
;; Last-Updated: Tue Oct 18 03:21:26 2016 (+0100)
;;           By: Neil Woods
;;     Update #: 33
;; URL: https://github.com/netlexer/dotfiles.git
;; Keywords: initialization, startup.
;; Compatibility: GNU Emacs >= 24.3
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Commentary: Written originally for GNU Emacs (ver 19.x), with many
;; ideas from usenet, emacswiki, etc. This is a work-in-progress.
;; Renamed from ~/.emacs.
;; (c) Neil Woods, 1992-2016.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Startup stuff & initialize variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Announce start of file loading...
(message "Loading Emacs personal init file...")
(let ((nw_dir (expand-file-name "~neil/.emacs.d/lisp")))
  (if (file-exists-p nw_dir)
      (progn (setq load-path (append (list nw_dir) load-path)))))

;; Initialise variable for, and load emacs-uptime. (M-x emacs-uptime)
(defvar *emacs-start-time* (current-time) "blink-blink yawn")
(require 'emacs-uptime)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
;; default plus add melpa for latest elisp packages
(add-to-list 'package-archives
  '("melpa" . "https://melpa.org/packages/") t)

(unless package-archive-contents    ;; Refresh the packages descriptions
  (package-refresh-contents))
(setq package-load-list '(all))     ;; List of packages to load
(add-hook 'package-menu-mode-hook 'highline-mode)
(package-initialize)

;;; load customizations.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; load a cool theme & mode-line
(load-theme 'cyberpunk t)
(setq sml/name-width 30
      sml/mode-width 'full
      sml/theme 'dark)
(sml/setup)
(load-theme 'smart-mode-line-powerline t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set a few variables
(setq-default Info-directory-list '("/usr/local/share/info/" "/usr/share/info/")
              backup-by-copying nil
              backup-by-copying-when-linked t
              backup-directory-alist '(("." . "~/.backups"))
              column-number-mode t
              custom-file "~/.emacs.d/custom.el"
              default-major-mode 'text-mode
              delete-old-versions t
              diary-file "~/.diary"
              font-lock-maximum-decoration t
              frame-title-format '("emacs@" system-name " [%b]" )
              global-font-lock-mode t
              global-paren-face-mode t
              hippie-expand-verbose t
              icon-title-format '("[%b]")
              inhibit-startup-screen t
              initial-scratch-message nil
              kept-new-versions 4
              kept-old-versions 2
              line-number-mode t
              make-backup-files t
              max-mini-window-height 1
              mouse-autoselect-window -0.5
              my-emacs-dir "~/.emacs.d/"
              next-line-add-newlines nil
              nw-identifier "Neil Woods <neil@netlexer.uk>"
              revert-without-query (cons "TAGS" revert-without-query)
              save-place t
              show-paren-mode t
              show-paren-style 'expression
              show-trailing-whitespace t
              system-time-locale "Europe/London"
              tab-width 4
              tab-stop-list (number-sequence 4 120 4)
              indent-tabs-mode nil
              transient-mark-mode t
              use-dialog-box nil
              use-file-dialog nil       ;; lines are set to t.
              vc-follow-symlinks t      ;; avoid 'Symbolic link to Git-controlled
              version-control t         ;; source file' messages, just do it.
              x-use-old-gtk-file-dialog t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Convenience functions etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; disable backups for files in /tmp or in my Mail or News directories.
(defun nw-backup-enable-predicate (filename)
  (and (not (string= "/tmp/" (substring filename 0 5)))
       (not (string-match "~/Mail/" filename))
       (not (string-match "~/News/" filename))))
(setq backup-enable-predicate 'nw-backup-enable-predicate)

;; Insert date/time string, bound to Meta-f9
(defun nw-insert-time ()
  (interactive)
  ;; parameters are: FORMAT (like date(1)), TIME (ie now), and an optional
  ;; third argument, UNIVERSAL. Example: Mon, 12 Nov 2001 17:25 +0000
  ;; Example similar to "date -R" cmd. & Debian changelog time format.
  (insert (format-time-string "%a, %-d %b %Y %T %z " (current-time) t)))
(define-key global-map [(meta f9)] 'nw-insert-time)

;; Now define a function to insert the time + personal identity (eg email).
;; neil <neil@nova.lan> -- 11/12/01 17:51:26 : modified format,
;; that is, it doesn't use nw-insert-time anymore.
(defun nw-insert-ident ()
  (interactive)
  (save-excursion)
  (beginning-of-line)
  (if (stringp comment-start)
      (if (= 1 (string-width comment-start))
	  (insert (concat comment-start comment-start " "))
	(insert comment-start)))
  (insert (concat nw-identifier " -- "
		  (format-time-string "%x %X " (current-time) t)
		  ": ")))
;; Bind it to M-F10
(define-key global-map [(meta f10)] 'nw-insert-ident)

;; Count words in buffer
(defun count-words-buffer ()
  "Count the number of words in current the buffer;
 print a message in the minibuffer with the result."
  (interactive)
  (save-excursion
    (let ((count 0))
      (goto-char (point-min))
      (while (< (point) (point-max))
	(forward-word 1)
	(setq count (1+ count)))
      (message "buffer contains %d words." count))))

;; Query replace on region

(defun query-replace-from-region (&optional to)
  (interactive "sQuery replace region with: ")
  (let ((from (buffer-substring (region-beginning)
                                (region-end))))
    (save-excursion
      (goto-char (region-beginning))
      (query-replace from to))))


;; for some reason Emacs lacks delete-line, implementing it with the
;; source from kill-line is, however, trivial
(defun delete-line (&optional arg)
  "Delete the rest of the current line; if no nonblanks there, delete
thru newline. With prefix argument, delete that many lines from point.
Negative arguments delete lines backward.

When calling from a program, nil means \"no arg\", a number counts as
a prefix arg.

To delete a whole line, when point is not at the beginning, type \
\\[beginning-of-line] \\[delete-line] \\[delete-line].

If `kill-whole-line' is non-nil, then this command deletes the whole line
including its terminating newline, when used at the beginning of a line
with no argument.  As a consequence, you can always delete a whole line
by typing \\[beginning-of-line] \\[delete-line]."
  (interactive "P")
  (delete-region (point)
	       ;; It is better to move point to the other end of the
	       ;; delete before deleting. That way, in a read-only
	       ;; buffer, point moves across the text that is to be
	       ;; delete. The choice has no effect on undo now that
	       ;; undo records the value of point from before the
	       ;; command was run.
	       (progn
		 (if arg
		     (forward-visible-line (prefix-numeric-value arg))
		   (if (eobp)
		       (signal 'end-of-buffer nil))
		   (if (or (looking-at "[ \t]*$") (and kill-whole-line (bolp)))
		       (forward-visible-line 1)
		     (end-of-visible-line)))
		 (point))))

(global-set-key (kbd "C-c d") 'delete-line)


;; press HOME key to either move to bol or beginning of indentation.
(defun back-to-indentation-or-beginning ()
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))

(global-set-key [home] 'back-to-indentation-or-beginning)


;; from "rgb" <rbielaws@i1.net> in
;; <1112372995.606713.126040@g14g2000cwa.googlegroups.com>
(defun insert-sequence-key (key)
  "Inserts a keystroke suitable for use in fcns like global-set-key"
  (interactive "kInsert key chord: ")
  (insert (format "(kbd \"%s\")" (key-description key))))

;;; literal characters
(defun insert-literal-char (arg)
  "Insert a character into a buffer by specifying its ascii code"
  (interactive "nEnter decimal value of chracter to insert: ")
  (insert (format "%c" arg)) )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Global Key assignments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set kp-enter  to `newline-and-indent' - globally; use the vector
;; definition -- this is also bound to C-j.
(define-key global-map [kp-enter] 'newline-and-indent)
;; Bind the Function keys for useful shortcuts
;; Make F1 invoke help : f1 = help; shift-f1 = "man"; ctrl-f1 = info.
;; TODO: Make possibly better/useful bindings here...
(global-set-key [f1]    'help-command)
(global-set-key [S-f1]  'man)
(global-set-key [ (control f1) ] 'info)
(global-set-key [f2]    'start-kbd-macro)	; Also C-X(
(global-set-key [f3]    'end-kbd-macro)		; Also C-X)
(global-set-key [f4]    'call-last-kbd-macro)	; Also C-Xe
(global-set-key [S-f4]  'name-last-kbd-macro)	; So we can save it?
(global-set-key [f5]    'dictionary-lookup-definition)
(global-set-key [f6]    'first-error)
(global-set-key [f7]    'previous-error)
(global-set-key [f8]    'next-error)
(global-set-key [f10]   'ispell-buffer)         ; normally F10 = menu
(global-set-key [S-f10] 'delete-other-windows)
(global-set-key [f11]   'undo)
(global-set-key [f12]   'other-window)
(global-set-key [S-f12] 'delete-window)

(if (string= (getenv "TERM") "screen")
    (progn
       (global-set-key "\e[V"  'scroll-down)
       (global-set-key "\e[U"  'scroll-up)))

;; Re-define the normal `save-buffers-kill-emacs' function when in X to
;; delete the current frame. Will NOT delete the LAST frame.
(global-unset-key "\C-x\C-c")
(global-set-key "\C-x\C-c" 'delete-frame)
(global-set-key [(alt ?i)] 'iconify-frame)   ; Alt-i

(define-key global-map [M-S-down-mouse-3] 'imenu)
(global-set-key [(hyper ?b)] 'ibuffer)

;; smilies. Use F9 then a, b, c etc (more to add later...)
;; a = U+1F603 SMILING FACE WITH OPEN MOUTH
;; b = U+1F609 WINKING FACE
;; c = U+1F606 SMILING FACE WITH OPEN MOUTH AND TIGHTLY-CLOSED EYES
;;
;; (see http://ergoemacs.org/emacs/emacs_n_unicode.html)

(global-set-key (kbd "<f9> a") (lambda () (interactive) (insert "😉")))
(global-set-key (kbd "<f9> b") (lambda () (interactive) (insert "😉")))
(global-set-key (kbd "<f9> c") (lambda () (interactive) (insert "😆")))

(global-set-key (kbd "C-x g") 'magit-status)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dired / Ibuffer / Web
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'dired-x)
(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")))

(add-hook 'dired-mode-hook
          (lambda ()
            (setq dired-omit-files "^\\.[a-z|A-Z]+\\|^\\.?#\\|^\\.$")
	    ;; could add dired-omit-extentions here, also.
	    (setq dired-omit-files-p nil)   ;; disabled by default
	    (highline-mode)
	    (define-key dired-mode-map [delete] 'dired-flag-file-deletion)
	    (define-key dired-mode-map [return] 'dired-find-file-other-window)
	    (define-key dired-mode-map [C-down-mouse-1]
	      'dired-mouse-find-file-other-window)))

(setq ibuffer-formats '((mark modified read-only " " (name 16 16) " "
			      (size 6 -1 :right) " " (mode 16 16 :center)
			      " " (process 8 -1) " " filename)
                        (mark " " (name 16 -1) " " filename))
      ibuffer-elide-long-columns t
      ibuffer-eliding-string "&")

(add-hook 'ibuffer-mode-hook 'highline-mode)

;; WWW -- Read URL's with specified browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "/usr/bin/light")

(global-set-key "\C-xm" 'browse-url-at-point)

;; super-click-button-1 (Windows key + click btn 1 )
(global-set-key [s-mouse-1] 'browse-url-at-mouse)

;; turn on ffap (emacs-goodies) (best loaded after browse-url or w3)(drazi)
(ffap-bindings)
(setq ffap-url-regexp nil)           ; disable URL features in ffap


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Spell check & dictionary lookup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Standard location of personal dictionary
(setq ispell-personal-dictionary "~/.flydict")

(if (file-exists-p "/usr/bin/hunspell")
    (progn
      ;; Add english-hunspell as a dictionary
      (setq-default ispell-program-name "hunspell"
                    ispell-dictionary "en_US"))
  (progn (setq-default ispell-program-name "aspell")
         (setq ispell-extra-args '("--sug-mode=normal" "--ignore=3"))))

(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))

;; flyspell
(require 'flyspell)
(define-key flyspell-mode-map (kbd "M-n") 'flyspell-goto-next-error)
(define-key flyspell-mode-map (kbd "M-.") 'ispell-word)

(load "dictionary-init")
(global-set-key "\C-cs" 'dictionary-search)
(global-set-key "\C-cm" 'dictionary-match-words)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion modes: dabbrev, hippie-expand, complete, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Hippie expand enables completion of filenames/dirs in buffers
(require 'hippie-exp)
(global-set-key [(hyper return)] 'hippie-expand)
(global-set-key [(control tab)] 'hippie-expand)

;; This binds word completions to Shift-Tab.
(global-set-key [S-iso-lefttab] 'dabbrev-completion)

;; Enables completion of recently used words (bound to M-RET & C-RET)
(require 'completion)
(dynamic-completion-mode)
(initialize-completions)

;; Icomplete-mode hooks - contrain minibuffer height...
(add-hook 'icomplete-minibuffer-setup-hook
	  (function
	   (lambda ()
	     (make-local-variable 'resize-minibuffer-window-max-height)
	     (setq resize-minibuffer-window-max-height 3))))

(add-hook 'message-setup-hook 'mail-abbrevs-setup)

;(require 'minibuf-electric-gnuemacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parens, paredit & related, eldoc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define function to match a parenthesis otherwise insert a % (like vi !;-P )
(global-set-key "%" 'match-paren)
(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

(require 'eldoc) ; if not already loaded
(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)

(defvar electrify-return-match
  "[\]}\)\"]"
  "If this regexp matches the text after the cursor, do an \"electric\"
  return.")

(defun electrify-return-if-match (arg)
  "If the text after the cursor matches `electrify-return-match' then
  open and indent an empty line between the cursor and the text.  Move the
  cursor to the new line."
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at electrify-return-match)
	(save-excursion (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))

;; Using local-set-key in a mode-hook is a better idea.
(global-set-key (kbd "RET") 'electrify-return-if-match)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  MODES & File HOOKS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; really useful: M-. -> jump to definition of identifier at point
;;                M-, -> jump back.
(require 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))

;; Keep a list of recently opened files (kept across sessions, too!)
(require 'recentf)
(recentf-mode 1)
(savehist-mode 1)
(require 'highline)           ;; used by (at least) mldonkey, gnus.

;; EXtra modes for editing various generic UNIX specific files:
(require 'generic-x)

(autoload 'xrdb-mode "xrdb-mode" "Mode for editing X resource files" t)
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

(setq auto-mode-alist
      (append '(("\\.Xdefaults$"    . xrdb-mode)
		("\\.Xenvironment$" . xrdb-mode)
		("\\.Xresources$"   . xrdb-mode)
		("*.\\.ad$"	    . xrdb-mode)
                ("\.lua$"           . lua-mode)
                ("/rfc[0-9]+\\.txt\\'" . rfcview-mode)
                )
	      auto-mode-alist))

(define-generic-mode 'xmodmap-mode
  '(?!)
  '("add" "clear" "keycode" "keysym" "pointer" "remove")
  nil
  '("[xX]modmap\\(rc\\)?\\'")
  nil
  "Simple mode for xmodmap files.")

(add-hook 'find-file-hooks 'auto-insert)

;; to add to auto-insert-alist, there's already a defun... needs work?
(define-auto-insert 'sh-mode "Shell-script.inc" t)  ; in ~/emacs/insert/

;; Automatic opening of zipped files.
(auto-compression-mode 1)

;; Add Time-stamp <> or Time-stamp " " anywhere in the top 8 lines of a
;; file to insert save date and time and user:
(add-hook 'write-file-hooks 'time-stamp)

;; Calendar, diary, and todo-modes...
(autoload 'todo-mode "todo-mode"
  "Major mode for editing TODO lists." t)
(autoload 'todo-show "todo-mode"
  "Show TODO items." t)
(autoload 'todo-insert-item "todo-mode"
  "Add TODO item." t)

(global-set-key "\C-ct" 'todo-show) ;; switch to TODO buffer
(global-set-key "\C-ci" 'todo-insert-item) ;; insert new item

;; Mode for viewing FAQ's...
(autoload 'faq-mode "faq-mode"
  "Major mode for reading faq files." t)
(autoload 'rfcview-mode "rfcview" nil t)

;; TeX support
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

;; Support .md files, as used on github and elsewhere
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))


;; A few variables which affect the *shell* (emacs terminal) in a window
;; (also includes the general 'comint' = COMMand INTerpreter functions).
(setq shell-prompt-pattern "^[^#$%>\n]*[#$%>] *")

;; Shell-mode hooks:

(add-hook 'comint-output-filter-functions
	  'comint-strip-ctrl-m)

(add-hook 'comint-output-filter-functions
	  'comint-truncate-buffer)

(add-hook 'shell-mode-hook
	  'ansi-color-for-comint-mode-on)

(setq comint-scroll-to-bottom-on-input 't
      comint-scroll-show-maximum-output 't
      comint-scroll-to-bottom-on-output 'all
      comint-input-ignoredups 't)

(define-key comint-mode-map [up] 'comint-previous-input)
(define-key comint-mode-map [down] 'comint-next-input)

;; Support Arch Linux PKGBUILD
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode)) auto-mode-alist))

;; header creation & update (autoloaded via package-initialise)
(autoload 'auto-update-file-header "header2")
(add-hook 'write-file-hooks 'auto-update-file-header)
(autoload 'auto-make-header "header2")
(add-hook 'emacs-lisp-mode-hook 'auto-make-header)
(add-hook 'c-mode-common-hook   'auto-make-header)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Programming: Haskell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Make Emacs look in Cabal directory for binaries
(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))

(setq haskell-process-auto-import-loaded-modules t
      haskell-process-log t
      haskell-process-suggest-remove-import-lines t
      haskell-process-type (quote cabal-repl)
      haskell-tags-on-save t)

; Choose indentation mode
;; Use haskell-mode indentation
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;; Use hi2
;(require 'hi2)
;(add-hook 'haskell-mode-hook 'turn-on-hi2)
;; Use structured-haskell-mode
;(add-hook 'haskell-mode-hook 'structured-haskell-mode)

; Add F8 key combination for going to imports block
(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))

; Add key combinations for interactive haskell-mode
(eval-after-load 'haskell-mode
  '(progn
     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
     (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
     (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
     (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
     (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)))
(eval-after-load 'haskell-cabal
  '(progn
     (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
     (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
     (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
     (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map (kbd "C-c C-o") 'haskell-compile))
(eval-after-load 'haskell-cabal
  '(define-key haskell-cabal-mode-map (kbd "C-c C-o") 'haskell-compile))

;; GHC-MOD
;; -------

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Programming: C/C++ modes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use Gnu Coding Standards.
;; RET now works as C-j to re-indent & indent in C/C++ and related modes.
(require 'cc-mode)
(add-hook 'c-mode-common-hook
          (lambda ()
	    (c-set-style "bsd")
            (define-key c-mode-base-map
	      "\C-m" 'c-context-line-break)))

;; could add a (local-set-key [return] 'reindent-then-newline-and-indent) too
(setq c-default-style "bsd")
(setq cperl-hairy t)

(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
	    (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
	      (ggtags-mode 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rs-info: Enhancements to info (esp. with Gnus) by Reiner Steib
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'rs-info-insert-current-node "rs-info"
  "Insert reference to current Info node using STYPE in buffer." t nil)
(autoload 'rs-info-boxquote "rs-info"
  "Yank text (from an info node), box it and use current info node as title."
  t nil)
(autoload 'rs-info-reload "rs-info" "Reload current info node." t nil)
(autoload 'rs-info-insert-node-for-variable "rs-info"
  "Insert a custom style info node for the top level form at point." t nil)
(defalias 'boxquote-info 'rs-info-boxquote)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  GENERAL: Menu Interface/Mouse related
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disable menubar and toolbar on the console, enable menu under X.
(menu-bar-mode 1)
(setq window-system-default-frame-alist
      '((x (menu-bar-lines . 1) (tool-bar-lines . 0))
        (nil (menu-bar-lines . 0) (tool-bar-lines . 0))))

(scroll-bar-mode nil)

;; for use in xterm
(require 'xt-mouse)
(unless window-system (xterm-mouse-mode 1))

;; Replace yes/no+enter prompts with y/n prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; Emacs 22+ - revert space completing filenames in minibuffer (see FAQ)
(define-key minibuffer-local-filename-completion-map (kbd "SPC")
  'minibuffer-complete-word)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mule and UNICODE Support :: not needed, generally. (See earlier revs).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; but this is useful to define...
;; Next ln equiv: ISO 2022 based 8-bit encoding for Latin-1 (MIME:ISO-8859-1)
(set-language-environment "English")
(setq system-time-locale "Europe/London")
(prefer-coding-system 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
