(let ((default-directory "/usr/share/emacs/site-lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))
(setq backup-directory-alist '((".*" . "~/.emacs.d/.temp")))

(with-eval-after-load "persp-mode-autoloads"
  (setq wg-morph-on nil)
  (add-hook 'after-init-hook #'(lambda ()
                                 (progn
                                   (persp-mode 1)
                                   (setq persp-add-buffer-on-after-change-major-mode t)))))


(package-initialize)
(setq package-enable-at-startup nil)

(unless package-archive-contents
  (package-refresh-contents))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(column-number-mode)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(show-paren-mode)
(fringe-mode 0)
(global-subword-mode 1)

(setq auto-save-default nil)
(setq-default truncate-lines t)
(set-display-table-slot standard-display-table 'truncation ? )
(setq comment-style 'multi-line)
(setq comment-style 'extra-line)

(add-to-list 'default-frame-alist '(font . "monospace-11"))
(add-to-list 'default-frame-alist '(fullscreen . fullboth))

(setq next-line-add-newlines nil)
(setq inhibit-startup-screen t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq display-time-default-load-average nil)
(setq display-time-24hr-format t)
(setq display-time-format nil)
(display-time-mode)
(setq scroll-step 1)
(setq hscroll-margin 1)
(setq hscroll-step 1)

(defun extract-patch (dir)
  (interactive "Ddir: ")
  (let ((id (notmuch-show-get-message-id t)))
    ;; https://github.com/aaptel/notmuch-extract-patch
    (shell-command (format "notmuch-extract-patch %s > %s"
                           (shell-quote-argument (notmuch-id-to-query id))
                           (shell-quote-argument (concat (expand-file-name dir) id ".patch"))))))

(defun apply-thread-patchset (repo branch)
  (interactive "Dgit repo: \nsnew branch name: ")
  (let ((tid (notmuch-search-find-thread-id))
    (tmp "/tmp/notmuch-patchset"))
    ;; https://github.com/aaptel/notmuch-extract-patch
    (shell-command (format "notmuch-extract-patch %s > %s && ( cd %s && git checkout -b %s && git am %s )"
                           (shell-quote-argument tid)
                           (shell-quote-argument tmp)
                           (shell-quote-argument (expand-file-name repo))
                           (shell-quote-argument branch)
                           (shell-quote-argument tmp)))))

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

(defun dos2unix ()
  "Convert EOLs from dos to unix"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (prin1 (eval (read (current-kill 0)))
	 (current-buffer)))

(defun insert-date ()
  "Insert date at point."
  (insert (format-time-string "%d/%m/%y-%R")))

(defun smart-tab ()
  ;; TODO: make this a list of functions
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
      (if mark-active
	  (indent-region (region-beginning)
			 (region-end))
	(if (and (bound-and-true-p go-mode)
		 (bound-and-true-p auto-complete-mode))
	    (ac-complete-go)
	  (if (looking-at "\\_>")
	      (dabbrev-expand nil)
	    (indent-for-tab-command))))))

(defun unix2dos ()
  "Convert EOLs from unix to dos"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n")))

;; key bindings
(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)
(global-set-key (kbd "C-.") 'mc/mark-next-like-this)
(global-set-key (kbd "C-,") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c mi") 'mc/insert-numbers)
(global-set-key (kbd "C-c rr") 'mc/reverse-regions)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (vector (list 'control mouse-wheel-down-event)) 'zoom-frm-in)
(global-set-key (vector (list 'control mouse-wheel-up-event))   'zoom-frm-out)
(global-set-key (kbd "C-c C-z") 'zoom-frm-unzoom)
(global-set-key (kbd "C-c C-e") 'eval-and-replace)
(global-set-key [(control h)] 'delete-backward-char)
(global-set-key [(super h)] 'help-command)
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "M-j")
		(lambda ()
		  (interactive)
		  (join-line -1)))
(global-set-key [(tab)] 'smart-tab)
(define-key notmuch-hello-mode-map (kbd "<C-tab>") nil)
(define-key notmuch-show-mode-map (kbd "<C-tab>") nil)
(global-set-key (kbd "<C-tab>") 'awesome-tab-forward)
(global-set-key (kbd "C-S-<iso-lefttab>") 'awesome-tab-backward)


;; ------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-dark)))
 '(read-file-name-completion-ignore-case t)
 '(vc-handled-backends nil)
 '(message-user-fqdn "linux.ibm.com")
 '(sendmail-program "/usr/bin/msmtp")
 '(send-mail-function 'sendmail-send-it)
 '(notmuch-fcc-dirs "Sent")
 '(notmuch-search-oldest-first nil)
 '(notmuch-mua-cite-function 'message-cite-original-without-signature)
 '(awesome-tab-common-group-name "notmuch")
 '(awesome-tab-cycle-scope 'tabs)
 '(awesome-tab-display-sticky-function-name nil)
 '(awesome-tab-buffer-groups-function
   '(lambda ()
      (list
	(cond
	 ((string-equal "*notmuch-hello*" (buffer-name)) "notmuch")
	 ((string-equal "*scratch*" (buffer-name)) "notmuch")
	 ((string-equal "*Messages*" (buffer-name)) "emacs")
	 ((string-equal "*Completions*" (buffer-name)) "emacs")
	 ((string-equal "*Help*" (buffer-name)) "emacs")
	 ((string-equal "*Apropos*" (buffer-name)) "emacs")
	 (t "notmuch")
	 )))
   )
 '(notmuch-saved-searches
   (quote
    ((:name "inbox" :query "to:farosas tag:unread")
     (:name "qemu-devel" :query "folder:qemu.qemu-devel")
     (:name "qemu-ppc" :query "folder:qemu.qemu-ppc")
     (:name "kvm-ppc" :query "folder:kvm-ppc")
     (:name "linuxppc-dev" :query "folder:linuxppc-dev")
     (:name "linuxppc-issues" :query "folder:github")
     (:name "kvm" :query "folder:kvm")
     (:name "power-port-tune" :query "folder:power-port-tune")
     (:name "slof" :query "folder:slof")
     (:name "virtiofs" :query "folder:virtiofs")
     (:name "skiboot" :query "folder:skiboot")
     (:name "linuxppc-uv" :query "folder:uv")
     (:name "virtio-dev" :query "folder:virtio-dev")
     (:name "libre-soc" :query "folder:libre-soc")
     (:name "openbios" :query "folder:openbios")
     (:name "sent" :query "folder:Sent")
     (:name "recv" :query "folder:INBOX")
)))
)
