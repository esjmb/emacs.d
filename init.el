;; init.el --- 
;;      
;; Filename: init.el
;; Description: My config for emacs.
;; Author: Stephen Barrett
;; Created: Thu Jul 14 19:00:18 2016 (+0100)
;; Version: 1
;; Package-Requires: ()
;; Last-Updated: Sat Sep  2 21:50:35 2017 (+0100)
;;           By: Stephen Barrett
;;     Update #: 1355
;; Keywords: emacs config 
;; Compatibility: GNU Emacs: 25.x
;;  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: This is my basic emacs configuration. Feel free to
;; grab anything useful. init.el lives in ~/.emacs.d
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code: 

;; lets just abort a load with warning is we are running emacs < 23


(if (version< emacs-version "23.3")
    (error "Init file only supports > 23.3 Aborting load. Try installing a newer version of emacs"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Kill startup message etc.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq inhibit-startup-message t)      ; hide welcome screne
(setq ring-bell-function 'ignore)     ; inhibit bell - it's annoying
(setq echo-keystrokes 0.01)           ; immediate echo
(global-hl-line-mode t)               ; highlight the current line
(blink-cursor-mode 0)                 ; stop blinking cursor
(setq custom-safe-themes t) 
(load-theme 'tango-dark)

(setq-default cursor-type '(bar . 4)) ; set cursor to bar
(tool-bar-mode -1)              ; no toolbar 
(menu-bar-mode -1)             ; no menu

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add MELPA to package archives.  Also set up use-package so that I
;; can use it to manage subsequent package loads.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents ; fetch the list of packages available 
  (package-refresh-contents))
(unless (package-installed-p 'use-package) ; integrate use-package
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

(use-package diminish
  :ensure t     
  :functions rename-modeline
  :config (progn
            (defmacro rename-modeline (package-name mode new-name)
              `(eval-after-load ,package-name
                 '(defadvice ,mode (after rename-modeline activate)
                    (setq mode-name ,new-name))))

            (rename-modeline "js2-mode" js2-mode "JS2")
            (rename-modeline "clojure-mode" clojure-mode "Clj")
            (rename-modeline "emacs-lisp-mode" emacs-lisp-mode "ELISP")))

(use-package bind-key :ensure t)
(use-package package-utils
  :ensure t
  ;;    :functions package-speak-upgrades
  :config (progn
            (defun package-upgrade-all () ; easier to remember than M-x -upg
              (interactive)
              (package-utils-upgrade-all))
            ))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some basic gui settings
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; force tooltip content to echo area
(tooltip-mode -1)

;; for neotree mode
(use-package all-the-icons :ensure t)
(use-package all-the-icons-dired :ensure t)

(use-package neotree
  :ensure t
  :config (progn
	    (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
            (setq neo-smart-open t))
  :bind ("A-i" . neotree-toggle))

;; no fringes on windows
(set-fringe-mode 0)

;; Don't display warnings below error level
(setq warning-minimum-level :error)

(set-default 'truncate-lines t) ; don't let lines wrap

(mac-auto-operator-composition-mode) ;; use ligatures for haskell hasklig display etc. 

(use-package mode-icons ; Show icons instead of mode names 
  :ensure t
  :config (mode-icons-mode))

;; set frame title
(setq frame-title-format
      '("" invocation-name ": "(:eval (if (buffer-file-name)
					  (abbreviate-file-name (buffer-file-name))
					"%b"))))

(setq column-number-mode t)           ; display cursor position in mode-line
(set-face-background 'scroll-bar "black")

;; increase the size of the minibuffer
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup)
(defun my-minibuffer-setup ()
  (set (make-local-variable 'face-remapping-alist)
       '((default :height 1.5))))


(global-set-key (kbd "<M-left>")  'windmove-left)
(global-set-key (kbd "<M-right>") 'windmove-right)
(global-set-key (kbd "<M-up>")    'windmove-up)
(global-set-key (kbd "<M-down>")  'windmove-down)

(use-package auto-dim-other-buffers
  :ensure t
  :config (progn
	    (add-hook 'after-init-hook (lambda ()
				       (when (fboundp 'auto-dim-other-buffers-mode)
					 (auto-dim-other-buffers-mode t))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode line
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package smart-mode-line
  :ensure t
  :config (progn
            (setq sml/theme 'dark)
            (sml/setup)))

(use-package spaceline       ;; nice mode line
  :ensure t
  :config (progn
	    (require 'spaceline-config)
	    (spaceline-emacs-theme)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global key bindings
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(when (window-system)
  (progn   ; unbind C-i from tab for later remapping
    (define-key input-decode-map (kbd "C-i") (kbd "H-i"))))  

;; Allow hash to be entered  as M-3
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

;; key bindings for window manipulation
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "A-(") 'delete-other-windows)
(global-set-key (kbd "M-(") 'delete-other-windows) ; for progrmmers keyboard
(global-set-key (kbd "M-2") 'split-window-below)
(global-set-key (kbd "A-2") 'split-window-below)
(global-set-key (kbd "A-)") 'split-window-below)
(global-set-key (kbd "M-)") 'split-window-below) ; for progrmmers keyboard
(global-set-key (kbd "A-3") 'split-window-right)
;;(global-set-key (kbd "M-3") 'split-window-right)
(global-set-key (kbd "A-}") 'split-window-right) 
(global-set-key (kbd "M-}") 'split-window-right) ; for progrmmers keyboard
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "A-o") 'other-window)
(global-set-key (kbd "A-d") 'delete-window)
;; buffers
(global-set-key (kbd "A-p") 'previous-buffer)
(global-set-key (kbd "A-n") 'next-buffer)
;; widen and shrink buffer
(global-set-key (kbd "A-]") 'enlarge-window-horizontally)
(global-set-key (kbd "A-[") 'shrink-window-horizontally)
;; set M-n and M-p to next and previous lines
(global-set-key (kbd "RET") 'reindent-then-newline-and-indent) ; indent present line then to next

;; change text size
(global-set-key (kbd "A-=") 'text-scale-increase)
(global-set-key (kbd "A--") 'text-scale-decrease)

;; recentre to current line
(global-set-key (kbd "A-l") 'recenter)
;; mac specific key bindings
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'alt)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  (global-set-key (kbd "A-z") 'undo)
  (global-set-key (kbd "A-a") 'mark-whole-buffer)
  (if (display-graphic-p)
      (progn
        (global-set-key (kbd "A-x") 'clipboard-kill-region)   ; mimic standard mac cut'n'paste
        (global-set-key (kbd "A-c") 'clipboard-kill-ring-save)        
        (global-set-key (kbd "A-v") 'clipboard-yank))
    (use-package osx-clipboard
      :ensure t
      :config (progn
                (osx-clipboard-mode +1)
                (global-set-key (kbd "A-c") 'osx-clipboard-cut-function)
                (global-set-key (kbd "A-v") 'osx-clipboard-paste-function)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customer set variable
;; reordering to here because smart-mode-line needs it set fuirst
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(company-quickhelp-color-background "dark olive green")
 '(company-quickhelp-color-foreground "white")
 '(company-tooltip-margin 3)
 '(custom-safe-themes
   (quote
    ("e654ce0507ae5b2d7feeaef2c07354206781527941e7feb178c0a94be4a98e90" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "0bec8e74ce41664f0e3ce76c0d2cc82804089df70164419af313483651b16bd1" "6ae174add87509daef7a844174f4f985592d70ea05c3d82377ad0a38a380ae80" "10e231624707d46f7b2059cc9280c332f7c7a530ebc17dba7e506df34c5332c4" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(git-gutter:hide-gutter t)
 '(line-number-mode nil)
 '(package-selected-packages
   (quote
    (haskell-snippets flyspell-prog-mode auto-dim-other-buffers shakespeare-mode git-gutter diff-hl diff-hl-mode aggressive-indent highlight-symbol all-the-icons-dired neotree gruvbox-theme popwin hightlight-symbol light-symbol-mode light-symbol fic-mode use-package-chords mode-icons spaceline spaceline-config markdown-mode idomenu ace-jump-mode multiple-cursors restclient restclient-mode company-dabbrev-code company-ghc commpany-ghc package-utils async php-ext php-mode company-quickhelp dash git-commit intero with-editor yaml-mode osx-clipboard ghc-imported-from fill-column-indicator centered-cursor-mode magit-popup rich-minority use-package line-number-mode iy-go-to-char expand-region magit dash-at-point highlight-parentheses exec-path-from-shell reveal-in-osx-finder company cus-face color-theme smart-mode-line smex header2 haskell-complete-module auto-compile haskell-mode)))
 '(safe-local-variable-values
   (quote
    ((haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#454e51"))))
 '(company-scrollbar-fg ((t (:background "#394143"))))
 '(company-tooltip ((t (:inherit default :background "#454e51"))))
 '(company-tooltip-annotation ((t (:inherit font-lock-comment-face))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
 '(font-lock-comment-face ((t (:italic t))))
 '(font-lock-comment-warning-face ((t (:background "grey10" :foreground nil))))
 '(font-lock-function-name-face ((t (:italic t))))
 '(font-lock-warning-face ((t (:background "grey10" :foreground nil))))
 '(highlight ((t (:background "grey10" :foreground nil)))))



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flyspell-prog-mode for comments spell checking 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flyspell-prog-mode :ensure t
	     :config 
	     (add-hook 'prog-mode-hook 'flyspell-prog-mode))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnippets
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yasnippet :ensure t
  :config (yas-global-mode 1))

(use-package haskell-snippets :ensure t)

(setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight-symbol
;; This mode highlights all occurences of the presently highlighted symbol
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package highlight-symbol
  :ensure t
  :config (progn
	    (setq highlight-symbol-idle-delay 0.3)
            (set-face-attribute 'highlight-symbol-face nil
                                :background "firebrick4"))
  :init (add-hook 'prog-mode-hook 'highlight-symbol-mode))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Last Command
;; The following code displays the last run command on the mode-line,
;; along with the keystrokes.
;; Basic approach is lifted from gnus-notify.el, which adds email
;; message notifications to the mode line.
;;
;; TODO: improve formatting of output so that pointless outputs like
;; self-insert-command for normal keystrokes are not presented.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package cl :ensure t)

;; (defvar display-new-message-sb "")
;; (defun notify-modeline-form-sb ()
;;   display-new-message-sb)

;; (if (featurep 'xemacs)
;;     (unless (member 'display-new-messages global-mode-string)
;;       (if (null global-mode-string)
;;           (setq global-mode-string '("" display-new-messages))
;;         (setq global-mode-string
;;               (append global-mode-string
;;                       '(display-new-messages)))))
;;   (unless (member '(:eval (notify-modeline-form-sb)) global-mode-string)
;;     (setq global-mode-string
;;           (append global-mode-string
;;                   (list '(:eval (notify-modeline-form-sb)))))))

;; (defun execute-extended-command-shorter-sb-1 (name length end-time)
;;        (cond
;;      ;((not (time-less-p (current-time) end-time)) nil)
;;      ((zerop length) (list ""))
;;      ((equal name "") nil)
;;      (t
;;       (nconc (mapcar (lambda (s) (concat (substring name 0 1) s))
;;                      (execute-extended-command-shorter-sb-1
;;                       (substring name 1) (1- length) end-time))
;;              (when (string-match "\\`\\(-\\)?[^-]*" name)
;;                (execute-extended-command-shorter-sb-1
;;                 (substring name (match-end 0)) length end-time))))))

;; (defun execute-extended-command-shorter-sb (name)
;;   (let ((candidates '())
;;         (max (length name))
;;         (len 1)
;;         binding
;;         (end-time (time-add (current-time) (seconds-to-time 1))))
;;     (while (and (not binding)
;;                 (time-less-p (current-time) end-time) ; timeout fail
;;                 (progn
;;                   (unless candidates
;;                     (setq len (1+ len))
;;                     (setq candidates (execute-extended-command-shorter-sb-1
;;                                       name len end-time)))
;;                   ;; Don't show the help message if the binding isn't
;;                   ;; significantly shorter than the M-x command the user typed.
;;                   (< len (- max 5))))
;;       (let ((candidate (pop candidates)))
;;         (when (equal name
;;                        (car-safe (completion-try-completion
;;                                   candidate obarray 'commandp len)))
;;           (setq binding candidate))))
;;     binding))

;; (defvar extended-command-hash (make-hash-table :test 'equal))
;; (defun generate-extended-command--shorter-sb (name)
;;   "Simple caching function to prevent recalculation of shortened command form."
;;   (let ((val (gethash name extended-command-hash)))
;;     (if (not val) 
;;         (let ((val (execute-extended-command-shorter-sb name)))
;; 	  (puthash name val extended-command-hash)
;; 	  val)		
;;       val)))

;; (defvar display-new-message-hash (make-hash-table :test 'equal))

;; (defadvice call-interactively (after show-last-command activate)
;;   "Shows the interactive command that was just run in the message area."
;;   (unless (or (eq major-mode 'minibuffer-inactive-mode)
;; 	      (not (symbolp real-this-command)))

;;     (let* ((tc (symbol-name real-this-command))	 ; don't use 'this-command' !
;; 	   (val (gethash tc display-new-message-hash))) ; cache previous string generations
;;       (if (bound-and-true-p val)
;; 	  (setq display-new-message-sb val)
;; 	;; TODO: clean up this by creating a variable that end user can add search strings to
;; 	(unless (or (string= "isearch-printing-char" tc)
;; 		    (cl-search "mouse-" tc)	    ; do not change bar
;; 		    (cl-search "wheel-" tc) 
;; 		    (cl-search "isearch-repeat" tc)				   
;; 		    (cl-search "company-ignore" tc)
;; 		    (cl-search "company-select" tc)
;; 		    (cl-search "company-complete" tc)
;; 		    (cl-search "ignore" tc))
	  
;; 	  (if (or (<= (length tc) 4)
;; 		  (string= "self-insert-command" real-this-command))  ; clear if non command pressed
;; 	      (setq display-new-message-sb "")
;; 	    (when (not (string= real-this-command "nil"))
;; 	      (let* ((kd (key-description (this-command-keys)))
;; 		     (seq (lambda (x)
;; 			    (or (string= x kd)
;; 				(cl-search "<me" x)
;; 				(cl-search "wheel-" x))))
		     
;; 		     (kda (mapcar 'key-description
;; 				  (where-is-internal real-this-command overriding-local-map nil)))
;; 		     (mem (member kd kda)) ;  
;; 		     (kdas (mapconcat 'identity (cl-remove-if seq kda) ", "))
;; 		     (str (format "%s%s" kdas	; append an M-x shortened version of command		 
;; 				  (if (not mem) 
;; 				      (let ((s (generate-extended-command--shorter-sb tc)))
;; 					(if (or (string= "" s)(string= "nil" s)) ""
;; 					  (format ", M-x %s" s)))
;; 				    "")))
;; 		     (dstr  (format "%s%s%s  "
;; 				    (if mem (format "%s : " kd) "")
;; 				    real-this-command 
;; 				    (if (string= "" str) ""
;; 				      (format ": %s" str)))))
;; 		(progn
;; 		  (puthash tc dstr display-new-message-hash)
;; 		  (setq display-new-message-sb dstr ))))))))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ispell
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ispell
  :ensure t
  :config (progn
	    (setq ispell-dictionary "english")))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Directories stuff
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq default-directory "~/github/"		 ; default directory
      backup-directory-alist		 ; backups to single directory
	     '(("." . "~/MyEmacsBackups"))
      delete-by-moving-to-trash t	 ; Move to trash when deleting stuff
      trash-directory "~/.Trash/emacs"
      insert-directory-program		 ; use gls for dired 
      (executable-find "gls"))

(when (memq window-system '(mac ns)) ; enable finder open on mac
  (use-package reveal-in-osx-finder  
    :ensure t
    :bind (("C-c C-f" . reveal-in-osx-finder))))

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prompt and message stuff stuff
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fset 'yes-or-no-p 'y-or-n-p) ; y or n prompt only
(setq confirm-nonexistent-file-or-buffer nil)  ; if it ain't there, create it
(setq kill-buffer-query-functions              ; me no care about live processes
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(eval-after-load "startup"                                  ; don't offer me help
  '(fset 'display-startup-echo-area-message (lambda ())))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell stuff
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :ensure t
    :config (progn	      
	      (setq exec-path-from-shell-check-startup-files nil)
	      (exec-path-from-shell-initialize))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smex - M-x enhancement
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package smex
  :ensure t
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands)
	 ("C-c x x" . execute-extended-command)
	 ("C-c s u" . smex-show-unbound-commands))) ; old M-X

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General text editing stuff
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(add-hook 'doc-view-mode-hook 'auto-revert-mode)

(delete-selection-mode 1) ; overwrite selected text
(global-set-key (kbd "M-k") '(lambda () (interactive) (kill-line 0)) ) ;M-k kills to the left

(use-package saveplace  ;; Save point position between sessions
  :ensure t 
  :config (progn
            (save-place-mode 1)
            (setq save-place-file (expand-file-name ".places" user-emacs-directory))))

;; (use-package linum			       ; line numbering globally
;; 	     :ensure t
;; 	     :config
;; 	     (progn 
;; 	       (setq linum-format "%3d\u2502") ; pretify format
;; 	       (global-linum-mode 1)))

(use-package highlight-parentheses   ; Parenthesis highlighting globally
  :ensure t
  :diminish highlight-parentheses-mode
  :config
  (progn 
    (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold)
    (global-highlight-parentheses-mode)
    ; make paren highlight update after stuff like paredit changes
    (add-to-list
     'after-change-functions
     '(lambda (&rest x) (hl-paren-highlight)))))

(setq comment-auto-fill-only-comments t	 ; auto fill comment lines
      indent-tabs-mode nil)		 ; use spaces, not <tab>

(setq-default auto-fill-function 'do-auto-fill)
(setq-default fill-column 140)		  ; we're not in the 70s
					
(use-package iy-go-to-char		 ; see emacs rocks episode 4
  :ensure t				 ; https://github.com/doitian/iy-go-to-char
  :bind (("M-]" . iy-go-to-char)
	 ("M-[" . iy-go-to-char-backward)))

;; indent yanked text
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
	   (and (not current-prefix-arg)
		(member major-mode '(emacs-lisp-mode lisp-mode
				     clojure-mode    scheme-mode
				     haskell-mode    ruby-mode
				     rspec-mode	     python-mode
				     c-mode	     c++-mode
				     objc-mode	     latex-mode
				     plain-tex-mode))
		(let ((mark-even-if-inactive transient-mark-mode))
		  (indent-region (region-beginning) (region-end) nil))))))

;; kill and join lines to avoid indented spaces
(defun kill-and-join-forward (&optional arg)
  "If at end of line, join with following; otherwise kill line and indent.
   Deletes whitespace at join."
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (delete-indentation t)
    (progn
      (kill-line arg))))

(global-set-key (kbd "C-k") 'kill-and-join-forward)

;; modify A-k kills either forward whitespace or next word
(defun kill-whitespace-or-word ()   
  (interactive)
  (if (looking-at "[ \t\n]")
      (let ((p (point)))
	(re-search-forward "[^ \t\n]" nil :no-error)
	(backward-char)
	(kill-region p (point)))
    (kill-word 1)))
(global-set-key (kbd "A-k") 'kill-whitespace-or-word)

;; backward kill line, leaving point at correct indentation
(global-set-key (kbd "A-<backspace>") (lambda ()
					(interactive)
					(kill-line 0)
					(indent-according-to-mode)))

(defun my-select-current-line ()
  (interactive)
  (move-beginning-of-line nil)
  (set-mark-command nil)
  (move-end-of-line nil)
  (setq deactivate-mark nil))

(defun sb/select-current-region (start end)
  (interactive)
  (goto-char start)
  (set-mark-command nil)
  (goto-char end)
  (setq deactivate-mark nil))

(defun comment-eclipse ()               
  (interactive)                         
  (let ((start (line-beginning-position)) 
        (end (line-end-position)))      
    (when (region-active-p)      
          (setq start (save-excursion   
                        (goto-char (region-beginning)) 
                        (beginning-of-line)
                        (point))
                end (save-excursion
                      (goto-char (region-end))
                      (end-of-line)
                      (point))))
    (comment-or-uncomment-region start end)))


(global-set-key (kbd "M-;") 'comment-eclipse)
(global-set-key (kbd "A-;") 'comment-eclipse)
(global-set-key (kbd "C-;") 'comment-dwim)

;; move around faster
(defun sb/forward-line ()
  (interactive)
  (ignore-errors (forward-line 5)))
  
(global-set-key (kbd "C-M-n") 'sb/forward-line)
(global-set-key (kbd "<A-down>") 'sb/forward-line)

(defun sb/backward-line()
  (interactive)
  (ignore-errors (forward-line -5)))

(global-set-key (kbd "C-M-p") 'sb/backward-line)
(global-set-key (kbd "<A-up>") 'sb/backward-line)

(defun sb/forward-fast ()
  (interactive)
  (ignore-errors (forward-char 10)))

(global-set-key (kbd "C-M-f") 'sb/forward-fast)
(global-set-key (kbd "<A-right>") 'sb/forward-fast)

(defun sb/backward-fast()
  (interactive)
  (ignore-errors (backward-char 10)))

(global-set-key (kbd "C-M-b") 'sb/backward-fast)
(global-set-key (kbd "<A-left>") 'sb/backward-fast)

;; scrolling
(when (display-graphic-p)
  ;; Disable pixel-by-pixel scrolling, since it's extremely choppy.
  (setq mac-mouse-wheel-smooth-scroll nil))
;; Keyboard smooth scrolling: Prevent the awkward "snap to re-center" when
;; the text cursor moves off-screen. Instead, only scroll the minimum amount
;; necessary to show the new line. (A number of 101+ disables re-centering.)
(setq scroll-conservatively 101)

;; Optimize mouse wheel scrolling for smooth-scrolling trackpad use.
;; Trackpads send a lot more scroll events than regular mouse wheels,
;; so the scroll amount and acceleration must be tuned to smooth it out.
(setq
 ;; If the frame contains multiple windows, scroll the one under the cursor
 ;; instead of the one that currently has keyboard focus.
 mouse-wheel-follow-mouse 't
 ;; Completely disable mouse wheel acceleration to avoid speeding away.
 mouse-wheel-progressive-speed nil
 ;; The most important setting of all! Make each scroll-event move 2 lines at
 ;; a time (instead of 5 at default). Simply hold down shift to move twice as
 ;; fast, or hold down control to move 3x as fast. Perfect for trackpads.
 mouse-wheel-scroll-amount '(2 ((shift) . 4) ((control) . 6)))

;; create line above or below
(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer-and-window)
    (comint-delchar-or-maybe-eof arg)))

(defvar  shell-mode-map)
(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map
              (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer manipulation
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil) 
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(setq help-window-select t) ;; move cursor to help window so 'q' closes

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; font selection
;; ordered for clarity of symbols for haskell programming. the list of
;; font-preferences is reduced to those fonts installed, and then
;; the first available font is selected, if any are available.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package cl-lib :ensure t)
(defun font-existsp (font)
  "Check to see if the named FONT is available."
  (if (null (x-list-fonts font))
      nil t))
(defun font-avail (fonts)
  "Finds the available fonts."
  (cl-remove-if-not 'font-existsp fonts))

(defvar font-preferences
  '(
     "Hasklig"
     ;;"Inconsolata"
     "Source Code Pro"
     "Fira Code"
     "PragmataPro"
     "DejaVu Sans Mono"
     "Bitstream Vera Sans Mono"
     "Anonymous Pro"
     "Menlo"     
     "Consolas"))

(unless (eq window-system nil)
  (let ((fonts (font-avail font-preferences)))
    (unless (null fonts)
      (progn
	(set-face-attribute 'default nil :font (car fonts))
	(set-face-attribute 'default nil :weight 'light)

        ;; If we have Hasklig font as default, which supports ligatures for haskell symbols,
        ;; then don't haskel-font-lock-symbols as Hasklig does a better job visually.
        (if (string-match "Hasklig" (car fonts))
            (progn
              (setq haskell-font-lock-symbols nil)
              (setq mac-auto-operator-composition-mode t)) ;; enable ligatures	  
          (setq haskell-font-lock-symbols 'unicode))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell Stuff
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; no delay on flycheck error
;; (setq flycheck-display-errors-delay 0)
;; (setq next-error-recenter 35)
;;(setq compilation-auto-jump-to-first-error t)

;; next error should scroll round to the top again
(defun my-next-error-wrapped (&optional arg reset)
  "Jumps to previous error if at first error jump to last error instead.
Prefix argument ARG says how many error messages to move forwards (or
backwards, if negative). With just C-u as prefix moves to first error"
  (interactive "P")
  (condition-case nil
      (call-interactively 'next-error)
    ('user-error (progn (next-error 1 t)))))

(defun my-jump-to-last-error (buffer)
  "Jump to last error in the BUFFER, this assumes that
the error is at last but third line"
  (save-selected-window
    (select-window (get-buffer-window buffer))
    (goto-char (point-max))
    (forward-line -3)
    (call-interactively 'compile-goto-error)
    (recenter)))

(defun my-previous-error-wrapped (&optional arg)
  "Jumps to previous error if at first error jump to last error instead.
Prefix argument ARG says how many error messages to move backwards (or
forwards, if negative)." 
  (interactive "P")
  (condition-case nil
      (if (compilation-buffer-p (current-buffer))
          (compilation-previous-error 1)
        (call-interactively 'previous-error))
    ('user-error (progn
                   (let ((error-buffer (next-error-find-buffer)))
                     ;; If the buffer has an associated error buffer use it to
                     ;; to move to last error
                     (if (and (not (eq (current-buffer) error-buffer))
                              (compilation-buffer-p error-buffer))
                         (my-jump-to-last-error error-buffer)
                       ;; Otherwise move to last point and invoke previous error
                       (goto-char (point-max))
                       (call-interactively 'previous-error)))))))
  
;; put a fringe indicator on the current error
(define-fringe-bitmap 'custom-right-arrow [128 192 96 48 24 48 96 192 128] 9 8 'center)
(put 'overlay-arrow-position 'overlay-arrow-bitmap 'custom-right-arrow)
(defface right-triangle-face
  '((t (:background "red" :foreground "green")))
  "Face for `ight-triangle-face`."
  :group 'basic-faces)

(set-fringe-bitmap-face 'right-triangle 'right-triangle-face)

(defun bar () 
  (with-current-buffer next-error-last-buffer
    (unless (eq 'filled-rectangle (cdr (assq 'overlay-arrow fringe-indicator-alist)))
      (setq fringe-indicator-alist
            (cons '(overlay-arrow . filled-rectangle) fringe-indicator-alist)))))
(add-hook 'next-error-hook 'bar)


;; code for nice nested indent
(defun haskell-move-right ()  
  (interactive)
  (haskell-move-nested 1))	
(defun haskell-move-left ()
  (interactive)
  (haskell-move-nested -1))

(use-package haskell-mode
  :config (progn
	    (setq haskell-stylish-on-save t)
	    (setq haskell-compile-cabal-build-command "stack build")))

;; load up intero
(use-package intero
  :ensure t
  :bind* ( :map haskell-mode-map
                ("C-r" . haskell-move-right)	 ; nested right indent
                ("C-l" . haskell-move-left)	 ; nested left indent
                ("C-c C-b" . haskell-compile)
                ("C-`" . my-next-error-wrapped)
                ("M-`" . my-previous-error-wrapped))
  :config
  (progn 
    (add-hook 'haskell-mode-hook 'intero-mode)))
    
;; (use-package flycheck
;;   :ensure t
;;   :config (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))

;; (use-package flycheck-popup-tip
;;   :config (progn
;;             (with-eval-after-load 'flycheck
;;               '(add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode))))

(use-package company
  :ensure t
  :functions company-quickhelp-manual-begin
  :config
  (progn 
    (add-hook 'after-init-hook 'global-company-mode)
    ;; (add-to-list 'company-backends 'company-yasnippet)
    ;;(add-to-list 'company-backends '(company-ghc :with company-dabbrev-code))    
    (setq company-ghc-show-info t)
    (add-hook 'haskell-mode-hook 'company-mode)
    ;; (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 2)
    ;; set up color package to pretify company tooltips etc. 
    (use-package color
      :ensure t
      :functions color-lighten-name
      :config (progn
		(let ((bg (face-attribute 'default :background)))
		  (custom-set-faces
		   `(company-tooltip ((t (:inherit default :background, (color-lighten-name bg 10)))))
		   `(company-scrollbar-bg ((t (:background, (color-lighten-name bg 10)))))
		   `(company-scrollbar-fg ((t (:background, (color-lighten-name bg 5)))))
		   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face :background "darkgreen"))))
		   `(company-tooltip-annotation((t (:inherit font-lock-comment-face))))))))	   
    (eval-after-load 'company
      '(define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin))))

(use-package company-ghc :ensure t
  :config (progn
	    (add-hook 'haskell-mode-hook (lambda ()
					   ;;(ghc-init)
					   (add-to-list 'company-backends 'company-ghc)))
	    (autoload 'ghc-init "ghc" nil t)
	    (autoload 'ghc-debug "ghc" nil t)))
	    
(use-package company-quickhelp
  :ensure t
  :bind (("A-g" . company-ghc-complete-by-hoogle)
	 ("A-m" . company-ghc-complete-in-module))
  :config (progn 
	    (company-quickhelp-mode 1)))

;; display TODO: comments in red, do lambdas etc. 
(defun pretty-lambdas-haskell ()
  (font-lock-add-keywords
   nil `((,(concat "\\(" (regexp-quote "\\") "\\)")
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil)))
         ;; display liftIO or 'liftIO $' as φ
         (,(concat "\\(" (regexp-quote "liftIO") "\\)") ;; hide liftIO and liftIO $
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 246))
                    nil)))
         (,(concat "\\(" (regexp-quote "liftIO $") "\\)")
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 246))
                    nil)))))
  
  (font-lock-add-keywords nil '(("\\<\\(TODO\\):" 1 '(:foreground "red") t)))
  (font-lock-add-keywords nil '(("\\<\\(liftIO\\)" 1 '(:foreground "orange") t)))
  (font-lock-add-keywords nil '(("\\<\\(liftIO $\\)" 1 '(:foreground "orange") t))))

(add-hook 'haskell-mode-hook 'pretty-lambdas-haskell)

;; syntax highlighting and indentation for editing Shakespearean templates (hamlet, lucius, julius).
(use-package shakespeare-mode :ensure t)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Documentation Browsers 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dash-at-point
  :ensure t
  :config (progn
            (add-hook 'haskell-mode-hook
                      (lambda () (setq dash-at-point-docset "haskell")))
            (add-hook 'lisp-mode-hook
                      (lambda () (setq dash-at-point-docset "lisp"))))
  :bind (("C-c d" . dash-at-point)
         ("C-c e" . dash-at-point-with-docset)))

;; ghc-imported-from - front end for ghc-imported-from for finding haddock documentation
;; for symbols in
(use-package ghc-imported-from
  :ensure t
  :config (progn
            (eval-after-load 'haskell-mode
              `(define-key haskell-mode-map
                 (kbd "C-c h") #'ghc-imported-from-haddock-for-symbol-at-point))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ANSI colour etc. for compilation
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ansi-color
  :config (progn
	    (defun colorize-compilation-buffer ()
	      (let ((inhibit-read-only t))
		(ansi-color-apply-on-region (point-min) (point-max))))
	    (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)))

(setq compilation-scroll-output t)

;; Some code that will make it so the background color of the lines
;; that gcc found errors on, should be in another color.
 
(use-package custom
  :config (progn

	    (defvar all-overlays ())
	    
	    (defun delete-this-overlay(overlay is-after begin end &optional len)
	      (delete-overlay overlay)
	      )
	    (defvar current-point)
            (defvar init/beg)
            (defvar init/end)
            (defvar error-line-overlay)
	    (defun highlight-current-line()
	      (interactive)
	      (setq current-point (point))
	      (beginning-of-line)
	      (setq init/beg (point))
	      (forward-line 1)
	      (setq init/end (point))
	      ;; Create and place the overlay
	      (setq error-line-overlay (make-overlay 1 1))
	      
	      ;; Append to list of all overlays
	      (setq all-overlays (cons error-line-overlay all-overlays))
	      
	      (overlay-put error-line-overlay
			   'face '(background-color . "dark slate gray"))
	      (overlay-put error-line-overlay
			   'modification-hooks (list 'delete-this-overlay))
	      (move-overlay error-line-overlay init/beg init/end)
	      (goto-char current-point))
	    
	    (defun delete-all-overlays()
	      (while all-overlays
		(delete-overlay (car all-overlays))
		(setq all-overlays (cdr all-overlays))))
	    
	    (defun highlight-error-lines(compilation-buffer process-result)
	      (interactive)
	      (delete-all-overlays)
	      (condition-case nil
		  (while t
		    (next-error)
		    (highlight-current-line)
		    (save-excursion
		      (compilation-next-error-function 0)
		      (highlight-current-line))
		    )
		(error nil)))
            (setq compilation-finish-functions 'highlight-error-lines)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source Control, ie. Git
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit                 ; Magit for git integration
	     :ensure t
	     :bind (("C-c m o" . magit-clone)
		    ("C-c m s" . magit-status)
		    ("C-c m b" . magit-blame)
		    ("C-c m l" . magit-log-buffer-file)
		    ("C-c m p" . magit-pull))
	     :config
	     (progn
	       (setq magit-save-repository-buffers 'dontask)
               ;; full screen magit-status and magit-commit
               ;; This code makes magit-status run alone in the frame, and then restores the old window
               ;; configuration when you quit out of magit.
               (defadvice magit-status (around magit-fullscreen activate)
                 (window-configuration-to-register :magit-fullscreen)
                 ad-do-it
                 (delete-other-windows))
	       
               (defun magit-quit-session ()
                 "Restores the previous window configuration and kills the magit buffer"
                 (interactive)
                 (kill-buffer)
                 (jump-to-register :magit-fullscreen))

               (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)))

(use-package git-gutter
  :ensure t
  :config (progn
	    (global-git-gutter-mode +1)
            ;; hide git gutter if no changes
	    (custom-set-variables
	     '(git-gutter:hide-gutter t))))

(use-package git-commit
  :ensure nil
  :preface
  (defun me/git-commit-set-fill-column ()
    (setq-local comment-auto-fill-only-comments nil)
    (setq fill-column 72))
  :config
  (advice-add 'git-commit-turn-on-auto-fill :before #'me/git-commit-set-fill-column))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Restclient-model
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package restclient
  :ensure t
  :bind (("C-9" . idomenu)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multiple-cursors
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ace-jump-mode
;; A marvelous move for quickly jumping to locations in the present view.
;;  https://github.com/winterTTr/ace-jump-mode
;;
;; To use, type C-0, then type the first character of the word you want to jump to. Each
;; instance of this the key typed in the view  is then replaced with a unique letter. Type
;; that letter to jump the cursor to that position. The mode is cleared. Type "C-x C-SPC" to
;; pop the cursor back to where you started.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ace-jump-mode
  :ensure t
  :config (progn
            (setq ace-jump-mode-gray-background nil)
            (setq ace-jump-mode-case-fold t))
  :bind (("C-j" . ace-jump-mode)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; idomenu
;; A menu based symbol search, depending on the current mode.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package idomenu :ensure t)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lisp Stuff
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package auto-compile     ; automatically recompile compiled lisp 
  :ensure t                   ; files on save.
  :config (progn
            (setq load-prefer-newer t)
            (auto-compile-on-load-mode)
            (auto-compile-on-save-mode)))

(use-package lisp-mode :bind (("C-0" . eval-buffer)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; php stuff
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package php-mode)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yaml mode
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yaml-mode :ensure t)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown mode
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;;Handy MACROS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  insert current date into the buffer at point  
(defun insert-date()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))   

(global-set-key "\C-cd" 'insert-date)

(set-background-color "#202020")
;;; init.el ends here
