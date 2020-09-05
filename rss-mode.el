;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq rss-archive-dir "~/rss/archive/")
(setq rss-trash-dir "~/rss/trash/")
(setq rss-dir "~/rss/INBOX")
(setq rss-entry-buffer-name "*RSS Entry*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes and keymaps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq rss-directory-mode-map (make-sparse-keymap))
(define-key rss-directory-mode-map (kbd "RET") 'rss-open-entry)
(define-key rss-directory-mode-map (kbd "r") 'rss-archive-current-entry)
(define-key rss-directory-mode-map (kbd "D") 'rss-delete-current-entry)

(define-derived-mode rss-directory-mode tabulated-list-mode "RSS Dir"
  "View list of RSS entries"
  (setq-local tabulated-list-format [("Source" 18 t)("Title" 18 t)])
  (setq-local tabulated-list-sort-key (cons "Title" nil))
  (setq-local tabulated-list-entries (rss-entries))
  (tabulated-list-init-header)
  (tabulated-list-print t))

(setq rss-mode-map (make-sparse-keymap))
(define-key rss-mode-map (kbd "q") 'kill-buffer-and-window)

(define-derived-mode rss-mode special-mode "RSS"
  "View one RSS entry"
  (read-only-mode)
  (let ((inhibit-read-only t)
        (entry (file-name-nondirectory buffer-file-name)))
    (erase-buffer)
    (rename-buffer (rss-entry-title entry))
    (insert (rss-label-string "Feed: ") (rss-entry-feed entry) "\n"
            (rss-label-string "Title: ") (rss-entry-title entry) "\n"
            (rss-label-string "Published at: ") (rss-entry-published-at entry) "\n"
            "\n")
    (let ((start (point)))
      (insert (rss-entry-content entry))
      (shr-render-region start (point-max)))
    (goto-char (point-min)))
  (read-only-mode)
  (not-modified))

(add-to-list 'auto-mode-alist '("\\.rss\\'" . rss-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XML complementary functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun xml-node-list-children (node)
  (seq-filter 'listp (xml-node-children node)))

(defun xml-find-by-tag (tag node)
  (if (eq tag (xml-node-name node))
      node
    (seq-find 'identity (mapcar (lambda (node) (xml-find-by-tag tag node)) (xml-node-list-children node)))))

(defun xml-nodes-content (node)
  (apply 'concat (xml-node-children node)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rss-entry-feed-filename (file)
  (concat default-directory "../.meta/" (car (split-string file "-")) ".rss"))

(defun rss-entry-feed (file)
  (xml-nodes-content (xml-find-by-tag 'title (car (xml-parse-file (rss-entry-feed-filename file))))))

(defun rss-entry-title (file)
  (xml-nodes-content (xml-find-by-tag 'title (car (xml-parse-file (concat default-directory file))))))

(defun rss-entry-published-at (file)
  (xml-nodes-content
   (or
    (xml-find-by-tag 'pubDate (car (xml-parse-file (concat default-directory file))))
    (xml-find-by-tag 'published (car (xml-parse-file (concat default-directory file)))))))

(defun rss-entry-content (file)
  (xml-nodes-content
   (or
    (xml-find-by-tag 'description (car (xml-parse-file (concat default-directory file))))
    (xml-find-by-tag 'content (car (xml-parse-file (concat default-directory file)))))))

(defun rss-entry (file)
  (list file (vector (rss-entry-feed file) (rss-entry-title file))))

(defun is-dots (file)
  (member file '("." "..")))

(defun rss-entries ()
  (mapcar 'rss-entry (seq-remove 'is-dots (directory-files default-directory))))

(defun rss-label-string (str)
  (propertize str 'font-lock-face '(:foreground "orange")))

(defun ensure-directory (path)
  (if (not (file-directory-p path)) (make-directory path)))

(defun rss-move-entry (file destination-dir)
  (ensure-directory destination-dir)
  (rename-file (concat default-directory file) (concat destination-dir file)))

(defun rss-delete-entry (file)
  "Delete the RSS entry file and create an empty file with the
same name in the trash directory to prevent creating it again
when refreshing RSS feeds, Use this if you don't want to keep the
RSS entry on your desk, otherwise Archiving it is the way to go"
  (ensure-directory rss-trash-dir)
  (write-region "" nil (concat rss-trash-dir file))
  (delete-file (concat default-directory file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rss-archive-current-entry ()
  "Moved the RSS entry to the rss-archive-dir"
  (interactive)
  (if (tabulated-list-get-id)
      (list
       (rss-move-entry (tabulated-list-get-id) rss-archive-dir)
       (tabulated-list-delete-entry))))

(defun rss-delete-current-entry ()
  (interactive)
  (if (tabulated-list-get-id)
      (progn
        (rss-delete-entry (tabulated-list-get-id))
        (tabulated-list-delete-entry))))

(defun rss-open-entry ()
  (interactive)
  (find-file-other-window (concat default-directory (tabulated-list-get-id)))
  (rss-mode))

(defun rss-entries-list ()
  (interactive)
  (find-file rss-dir)
  (rss-directory-mode))
