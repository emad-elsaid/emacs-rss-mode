(setq rss-mode-map (make-sparse-keymap))
(define-key rss-mode-map (kbd "RET") 'rss-open-entry)
(define-key rss-mode-map (kbd "r") 'rss-archive-current-entry)
(define-key rss-mode-map (kbd "D") 'rss-delete-current-entry)

(define-derived-mode rss-mode tabulated-list-mode "RSS"
  "View list of RSS entries"
  (setq-local tabulated-list-format [("Source" 18 t)("Title" 18 t)])
  (setq-local tabulated-list-sort-key (cons "Title" nil))
  (tabulated-list-init-header))

(setq rss-entry-mode-map (make-sparse-keymap))
(define-key rss-entry-mode-map (kbd "q") 'kill-buffer-and-window)

(define-derived-mode rss-entry-mode special-mode "RSS Entry"
  "View one RSS entry")

(defun xml-node-list-children (node)
  (seq-filter 'listp (xml-node-children node)))

(defun xml-find-by-tag (tag node)
  (if (eq tag (xml-node-name node))
      node
    (seq-find 'identity (mapcar (lambda (node) (xml-find-by-tag tag node)) (xml-node-list-children node)))))

(defun xml-nodes-content (node)
  (apply 'concat (xml-node-children node)))

(defun rss-entry-feed-filename (file)
  (concat rss-dir "../.meta/" (car (split-string file "-")) ".xml"))

(defun rss-entry-feed (file)
  (xml-nodes-content (xml-find-by-tag 'title (car (xml-parse-file (rss-entry-feed-filename file))))))

(defun rss-entry-title (file)
  (xml-nodes-content (xml-find-by-tag 'title (car (xml-parse-file (concat rss-dir file))))))

(defun rss-entry-published-at (file)
  (xml-nodes-content
   (or
    (xml-find-by-tag 'pubDate (car (xml-parse-file (concat rss-dir file))))
    (xml-find-by-tag 'published (car (xml-parse-file (concat rss-dir file))))
    )))

(defun rss-entry-content (file)
  (xml-nodes-content
   (or
    (xml-find-by-tag 'description (car (xml-parse-file (concat rss-dir file))))
    (xml-find-by-tag 'content (car (xml-parse-file (concat rss-dir file))))
    )))

(defun rss-entry (file)
  (list file (vector (rss-entry-feed file) (rss-entry-title file))))

(defun is-dots (file)
  (member file '("." "..")))

(defun rss-entries (path)
  (mapcar 'rss-entry
          (seq-remove 'is-dots (directory-files path))))

(defun rss-list-entry-file ()
     (concat rss-dir (tabulated-list-get-id)))

(defun rss-label-string (str)
  (propertize str 'font-lock-face '(:foreground "orange")))

(defun kill-buffer-if-exists (buffer)
  (if (get-buffer buffer) (kill-buffer buffer)))

(defun ensure-directory (path)
  (if (not (file-directory-p path)) (make-directory path)))

(defun rss-move-entry (file destination-dir)
  (ensure-directory destination-dir)
  (rename-file (concat rss-dir file) (concat destination-dir file)))

(defun rss-delete-entry (file)
  "Delete the RSS entry file and create an empty file with the
same name in the trash directory to prevent creating it again
when refreshing RSS feeds, Use this if you don't want to keep the
RSS entry on your desk, otherwise Archiving it is the way to go"
  (ensure-directory rss-trash-dir)
  (write-region "" nil (concat rss-trash-dir file))
  (delete-file (concat rss-dir file)))

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

(defun find-window-or-split-right (buffer)
  (let ((window (get-buffer-window buffer)))
    (if window
        (select-window window)
      (split-window-right-and-focus))))

(defun rss-open-entry ()
  (interactive)
  (let ((entry (tabulated-list-get-id)))
    (find-window-or-split-right "*RSS Entry*")
    (kill-buffer-if-exists "*RSS Entry*")
    (pop-to-buffer-same-window "*RSS Entry*")
    (insert (rss-label-string "Feed: ") (rss-entry-feed entry) "\n"
            (rss-label-string "Title: ") (rss-entry-title entry) "\n"
            (rss-label-string "Published at: ") (rss-entry-published-at entry) "\n"
            "\n")
    (let ((start (point)))
      (insert (rss-entry-content entry))
      (shr-render-region start (point-max)))
    (goto-char (point-min))
    (rss-entry-mode)))

(defun rss-entries-list ()
  (interactive)
  (switch-to-buffer "*RSS*")
  (setq rss-dir "~/rss/INBOX/")
  (setq rss-archive-dir "~/rss/archive/")
  (setq rss-trash-dir "~/rss/trash/")
  (setq tabulated-list-entries (rss-entries rss-dir))
  (rss-mode)
  (tabulated-list-print t))