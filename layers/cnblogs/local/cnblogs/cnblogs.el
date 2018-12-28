;;; cnblogs.el --- cnblogs client using xml-rpc.
;;
;; Copyright (c) 2018 WEN YANG & Contributors
;;
;; Author: WEN YANG <steven@WENdeMacBook-Pro.local>
;; URL: https://github.com/yangwen0228/.spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; cnblogs client using xml-rpc.
;; `blog-id' is equal to user-name by default.
;; `post-id' is the id of the blog/article.

;;; Code:

;; entry structure:
;; (id   title   postid      categories src-file state)
;; (int  string  int/string  list       string   string)

(require 'metaweblog)

(defgroup cnblogs nil
  "The cnblogs client using xml-rpc."
  :group 'org)

(defcustom cnblogs-store-root-dir "~/.cnblogs/"
  "The root directory of stored files."
  :group 'cnblogs
  :type 'string)

(defcustom cnblogs-blog-id nil
  "The blog id or address. It's the same to the `cnblogs-user-name' by default."
  :group 'cnblogs
  :type 'string)

(defcustom cnblogs-user-name nil
  "The login user name."
  :group 'cnblogs
  :type 'string)

(defcustom cnblogs-user-passwd nil
  "The login password."
  :group 'cnblogs
  :type 'string)

(defcustom cnblogs-media-object-suffix-list '("jpg" "jpeg" "png" "gif" "mp4")
  "The supported media types."
  :group 'cnblogs
  :type 'list)

(defcustom cnblogs-src-file-extension-list '("org" "html")
  "The supported file types."
  :group 'cnblogs
  :type 'list)

(defvar cnblogs-category-list-file
  (concat cnblogs-store-root-dir ".category-list")
  "The stored file of category list.")

(defvar cnblogs-entry-list-file
  (concat cnblogs-store-root-dir ".entry-list")
  "The stored file of entry list.")

;; (("title" . "Hello Cnblogs!")
;;  ("dateCreated" :datetime (20423 52590))
;;  ("categories"  "Emacs")
;;  ("description" . "The content of the blog."))
(defvar cnblogs-file-post-path
  (concat cnblogs-store-root-dir "posts/")
  "The blog files translated to `xml-rpc' style.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar cnblogs--server-url nil
  "The cnblogs url to server's `metaweblog.aspx'.")

(defvar cnblogs--category-list nil
  "The blogs category list in cache.")

(defvar cnblogs--entry-list nil
  "The blogs entry list in cache. Keep the post-id to .org file mapping.")

(defvar cnblogs--posts-in-category nil
  "The blogs in category.")

(defvar cnblogs--user-blogs nil
  "The blog information.")

(defvar cnblogs--next-id nil
  "The next inner id, start from 1 and increase 1 by 1.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;Menu;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar cnblogs-mode-map
  (make-sparse-keymap "Cnblogs")
  "The cnblogs minor mode keymap.")

(define-key cnblogs-mode-map [tags-getUsersBlogs]
  '(menu-item "User blogs" cnblogs/get-user-blogs
              :help "Get user's blogs."))

(define-key cnblogs-mode-map [tags-getRecentPosts]
  '(menu-item "Get recent posts" cnblogs/get-recent-posts
              :help "Get the recent sevaral blogs."))

(define-key cnblogs-mode-map [tags-getCategories]
  '(menu-item "Get(Update) categories" cnblogs/get-categories
              :help "Get/Update the categories."))

(define-key cnblogs-mode-map [tags-getPost]
  '(menu-item "Get post" cnblogs/get-post
              :help "Get/Update the local post by post-id."))

(define-key cnblogs-mode-map [separator-cnblogs-tags]
  '(menu-item "--"))

(define-key cnblogs-mode-map [tags-editPost]
  '(menu-item "Update post" cnblogs/update-post
              :help "Update the blog."))

(define-key cnblogs-mode-map [tags-deletePost]
  '(menu-item "Delete post" cnblogs/delete-post
              :help "Delete the blog."))

(define-key cnblogs-mode-map [tags-saveDraft]
  '(menu-item "Save draft" cnblogs/save-draft
              :help "Save the blog to server, but not published yet."))

(define-key cnblogs-mode-map [tags-newPost]
  '(menu-item "Publish post" cnblogs/new-post
              :help "Publish the blog."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;LoadData;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cnblogs//load-variables ()
  (cnblogs//load-entry-list)
  (cnblogs//load-category-list)
  (cnblogs//init-posts-in-category))

(defun cnblogs//load-entry-list ()
  "Load entry list from stored `cnblogs-entry-list-file'."
  (setq cnblogs--entry-list
        (condition-case ()
            (with-temp-buffer
              (insert-file-contents cnblogs-entry-list-file)
              (car (read-from-string (buffer-string))))
          (error nil))))

(defun cnblogs//load-category-list ()
  "Load category list from stored `cnblogs-entry-list-file'."
  (setq cnblogs--category-list
        (condition-case ()
            (with-temp-buffer
              (insert-file-contents cnblogs-category-list-file)
              (car (read-from-string (buffer-string))))
          (error nil))))

(defun cnblogs//init-posts-in-category ()
  (dolist (categorie cnblogs--category-list)
    (push (cons categorie nil) cnblogs--posts-in-category)
    (dolist (entry cnblogs--entry-list)
      (and (member categorie (nth 3 entry))
           (push entry (cdr (assoc categorie cnblogs--posts-in-category)))))))

(defun cnblogs//save-entry-list ()
  "Save the `cnblogs-entry-list' to `cnblogs-entry-list-file'."
  (condition-case ()
      (with-temp-file cnblogs-entry-list-file
        (print cnblogs--entry-list (current-buffer)))
    (error nil)))

(defun cnblogs//save-category-list ()
  "Save the `cnblogs-category-list' to `cnblogs-category-list-file'."
  (setq cnblogs--category-list
        (mapcar (lambda (category)
                  (cdr (assoc "description" category)))
                cnblogs--category-list))
  (with-temp-file cnblogs-category-list-file
    (print cnblogs--category-list (current-buffer))))

(defun cnblogs//get-post-state (src-file)
  (let ((legal-state 'NEW))
    (if (not (member (file-name-extension src-file)
                     cnblogs-src-file-extension-list))
        (setq legal-state 'UNSUPPORTED)
      (if (equal (cnblogs//get-src-file-state (buffer-file-name)) "PUBLISHED")
          (setq legal-state 'PUBLISHED)))
    legal-state))

(defun cnblogs//get-src-file-state (src-file)
  (nth 5 (find-if
          #'(lambda (entry) (equal src-file (nth 4 entry)))
          cnblogs--entry-list)))

(defun cnblogs//gen-id ()
  (setq cnblogs--next-id
        (if cnblogs--next-id
            (1+ cnblogs--next-id)
          (let ((id 1))
            (while (find-if #'(lambda (entry) (equal id (car entry))) cnblogs--entry-list)
              (setq id (1+ id)))
            id))))

(defun cnblogs//push-post-to-entry-list (post)
  (let ((title      (cdr (assoc "title"      post)))
        (postid     (cdr (assoc "postid"     post)))
        (categories (cdr (assoc "categories" post)))
        (done nil))
    (if (integerp postid) (setq postid (int-to-string postid)))
    (with-temp-file (concat cnblogs-file-post-path postid)
      (print post (current-buffer)))
    ;; Merge all the posts with same title.
    (setq done (cnblogs//merge-post-with-same-title postid title categories))
    ;;还没有插入则新建项
    (unless done
      (push
       (list (cnblogs//gen-id) ;id
             title             ;title
             postid            ;postid
             categories        ;categories
             nil               ;src-file
             "PUBLISHED"       ;state
             )
       cnblogs--entry-list))))

(defun cnblogs//merge-post-with-same-title (postid title categories)
  (let ((index 0)
        (done nil))
    (mapc
     (lambda (entry)
       (and (not done)
            (equal title (nth 1 entry))
            (y-or-n-p (format "merge the post %s with entry %S" postid entry))
            (progn
              (setq done t)
              (setcar (nthcdr index cnblogs--entry-list)
                      (list (nth 0 entry) ; id
                            title         ;title
                            postid        ;postid
                            categories    ;categories
                            (nth 4 entry) ;src-file
                            "PUBLISHED"   ;state
                            ))))
       (setq index (1+ index)))
     cnblogs--entry-list)
    done))

(defun cnblogs//push-src-file-to-entry-list (src-file)
  (if (cnblogs//file-existp-in-entry-list src-file)
      t
    (let ((title
           (with-temp-buffer
             (insert-file-contents src-file)
             (cnblogs//fetch-field "TITLE")))
          (done nil)
          (index 0))
      (setq done (cnblogs//merge-src-file-with-same-title src-file title))
      (unless done
        (push
         (list (cnblogs//gen-id) ;id
               title             ;title
               nil               ;postid
               nil               ;categories
               src-file          ;src-file
               "UNPUBLISHED"     ;state
               )
         cnblogs--entry-list)))))

(defun cnblogs//merge-src-file-with-same-title (src-file title)
  (let ((index 0)
        (done nil))
    (mapc
     (lambda (entry)
       (and (not done)
            title
            (equal title (nth 1 entry))
            (y-or-n-p (format "merge the file %s with entry %S" src-file entry))
            (progn
              (setq done t)
              (setcar (nthcdr 4 (nth index cnblogs--entry-list))
                      src-file)))
       (setq index (1+ index)))
     cnblogs--entry-list)
    done))

(defun cnblogs//assign-post-to-file (post src-file)
  (condition-case ()
      (progn
        (setq cnblogs--entry-list
              (mapcar
               (lambda (entry)
                 (if (equal src-file (nth 4 entry))
                     (list (nth 0 entry)                   ;id
                           (cdr (assoc "title" post))      ;title
                           (cdr (assoc "postid" post))     ;postid
                           (cdr (assoc "categories" post)) ;categories
                           src-file                        ;src-file
                           "PUBLISHED"                     ;state
                           )
                   entry))
               cnblogs--entry-list))
        t)
    (error nil)))

(defun cnblogs//delete-entry-from-entry-list (postid)
  (condition-case ()
      (progn
        (setq cnblogs--entry-list
              (remove-if (lambda (entry)
                           (equal postid
                                  (nth 2 entry)))
                         cnblogs--entry-list))
        (cnblogs//save-entry-list)
        (and (file-exists-p (concat cnblogs-file-post-path postid))
             (delete-file (concat cnblogs-file-post-path postid)))
        t)
    (error nil)))

(defun cnblogs//get-postid-by-title (title)
  (and (stringp title)
       (let ((postid nil))
         (mapc (lambda (entry)
                 (or postid
                     (and (equal title
                                 (nth 4 cnblogs--entry-list))
                          (setq postid
                                (nth 2 cnblogs--entry-list)))))
               cnblogs--entry-list)
         (and postid
              (integerp postid)
              (int-to-string postid))
         (or postid
             (setq postid "0"))))
  postid)

(defun cnblogs//get-postid-by-src-file-name (filename)
  (let ((postid nil))
    (mapc (lambda (entry)
            (if (equal filename (nth 4 entry))
                (setq postid (nth 2 entry))))
          cnblogs--entry-list)
    (or postid
        (setq postid "0"))
    postid))

(defun cnblogs//categories-string-to-list (categories-string)
  (if (or (eq categories-string nil)
          (eq categories-string ""))
      nil
    (let ((idx1
           (string-match "[^　 \t]+"    ;圆角半角空格
                         categories-string)))
      (if (not idx1)
          nil
        (setq categories-string         ;圆角半角空格
              (substring categories-string idx1))
        (let ((idx2
               (string-match "[　 \t]+"
                             categories-string)))
          (if idx2
              (cons (concat "[随笔分类]"
                            (substring categories-string
                                       0
                                       idx2))
                    (cnblogs//categories-string-to-list (substring categories-string
                                                                   idx2)))
            (cons (concat "[随笔分类]"
                          categories-string)
                  nil)))))))

(defun cnblogs//fetch-field (field)
  (let ((regexp (concat "^[ \t]*#\\+" field ":\\([^\n]*\\)$"))
        (content (buffer-substring-no-properties (point-min) (point-max))))
    (if (string-match regexp content)
        (s-trim (match-string 1 content))
      nil)))

(defun cnblogs//make-media-object-file-data (media-path)
  (and (file-exists-p media-path)
       (list
        ;;media-path name
        (cons "name"
              (file-name-nondirectory media-path))
        ;; bits
        (cons "bits"
              (base64-encode-string
               (with-temp-buffer
                 (insert-file-contents-literally media-path)
                 (buffer-string))))
        (cons "type" "image/jpg"))))

(defun cnblogs//org-mode-buffer-to-post ()
  (delq nil (list
             ;; title
             (cons "title"
                   (or (cnblogs//fetch-field "TITLE")
                       "新随笔"))
             ;; excerpt
             (cons "mt_excerpt"
                   (or (cnblogs//fetch-field "DESCRIPTION")
                       ""))
             ;; categories
             (cons "categories"
                   (let ((categories-list (cnblogs//categories-string-to-list
                                           (cnblogs//fetch-field "CATEGORIES"))))
                     (or categories-list
                         '("[随笔分类]未分类"))))
             ;; tags
             (cons "mt_keywords"
                   (or (cnblogs//fetch-field "KEYWORDS")
                       ""))

             ;; dateCreated
             (cons "dateCreated"
                   (list
                    :datetime
                    (condition-case ()
                        (date-to-time (cnblogs//fetch-field "DATE"))
                      (error (progn
                               (message "时间格式不支持，使用默认时间:1989-05-17 00:00")
                               (date-to-time "1989-05-17 00:00"))))))

             ;; description
             (cons "description"
                   (let ((org-export-show-temporary-export-buffer nil))
                     (with-current-buffer (org-html-export-as-html nil nil nil nil nil)
                       (let ((buf-str (cnblogs//replace-media-object-location
                                       (buffer-substring-no-properties
                                        (point-min) (point-max)))))
                         (kill-buffer)
                         buf-str)))))))

(defun cnblogs//replace-media-object-location (buf-str)
  (mapc (lambda (suffix)
          (let ((regexp
                 (concat "[file]*[:]?[/\\]*[a-z]?[:]?[^:*\"?<>|#]+."
                         suffix))
                (current 0))
            (while (string-match regexp
                                 buf-str
                                 current)
              (let* ((media-path (match-string 0
                                               buf-str))
                     (media-url
                      (save-match-data
                        (and (file-exists-p media-path)
                             (cnblogs-metaweblog-new-media-object
                              (cnblogs//make-media-object-file-data
                               media-path))))))

                (if media-url
                    (progn
                      (setq current
                            (+ (match-beginning 0)
                               (length media-url)))
                      (setq buf-str
                            (replace-match media-url
                                           t
                                           t
                                           buf-str)))
                  (setq current
                        (match-end 0)))))))
        cnblogs-media-object-suffix-list)
  buf-str)

(defun cnblogs//current-buffer-to-post ()
  (cond
   ((equal mode-name "Org") (cnblogs//org-mode-buffer-to-post))
   (t)))

(defun cnblogs//file-existp-in-entry-list (src-file)
  (let ((exist nil))
    (mapc (lambda (entry)
            (or exist
                (setq exist
                      (equal src-file (nth 4 entry)))))
          cnblogs--entry-list)
    exist))

(defun cnblogs//delete-post-from-entry-list (postid)
  (if (integerp postid) (setq postid (int-to-string postid)))
  (condition-case ()
      (progn
        (setq cnblogs--entry-list
              (mapcar (lambda (entry)
                        (if (equal postid
                                   (if (integerp (nth 2 entry))
                                       (int-to-string (nth 2 entry))
                                     (nth 2 entry)))
                            (progn
                              (setcar (nthcdr 2 entry) nil)
                              (setcar (nthcdr 3 entry) nil)
                              (setcar (nthcdr 5 entry) "UNPUBLISHED")))
                        entry)
                      cnblogs--entry-list))
        (cnblogs//save-entry-list)
        (and (file-exists-p (concat cnblogs-file-post-path postid))
             (delete-file (concat cnblogs-file-post-path postid)))
        t)
    (error nil)))

(defun cnblogs//import-directory (directory)
  (let ((files (directory-files directory t "^[^\.].*[^~]$" t)))
    (mapc (lambda (file)
            (cond ((file-directory-p file)
                   (cnblogs//import-directory file))
                  ((member (file-name-extension file) cnblogs-src-file-extension-list)
                   (cnblogs//push-src-file-to-entry-list file))))
          files)))

;; commands
(defun cnblogs/setup-blog ()
  (interactive)
  (setq cnblogs-user-name
        (read-string "Your username: " nil nil))
  (setq cnblogs-blog-id
        (read-string "Your blog id (default same with user name): " cnblogs-user-name nil))
  (setq cnblogs-user-passwd
        (read-passwd "Your password: " nil ))
  (setq cnblogs--server-url
        (concat "https://rpc.cnblogs.com/metaweblog/"
                cnblogs-blog-id))
  (setq cnblogs--category-list
        (cnblogs-metaweblog-get-categories))
  (setq cnblogs--user-blogs
        (cnblogs-metaweblog-get-user-blogs))
  (if cnblogs--user-blogs
      (progn
        (customize-save-variable 'cnblogs-blog-id  cnblogs-blog-id)
        (customize-save-variable 'cnblogs-user-name cnblogs-user-name)
        (customize-save-variable 'cnblogs-user-passwd cnblogs-user-passwd)
        (customize-save-variable 'cnblogs--server-url cnblogs--server-url)
        (or (file-directory-p cnblogs-store-root-dir)
            (make-directory cnblogs-store-root-dir t))
        (or (file-directory-p cnblogs-file-post-path)
            (make-directory cnblogs-file-post-path t))
        (cnblogs//save-category-list)
        (and (yes-or-no-p "Should I pull all your posts now, it may talk a long time?")
             (let ((posts (cnblogs-metaweblog-get-recent-posts 0)))
               (mapc (lambda (post)
                       (cnblogs//push-post-to-entry-list post))
                     posts))
             (cnblogs//save-entry-list))
        (message "Succeed to setup!"))
    (message "Failed to setup!")))

(defun cnblogs/new-or-update-post ()
  "New post if it doesn't post before, otherwise, update post!"
  (interactive)
  (unless (and (file-exists-p cnblogs-category-list-file)
               cnblogs--server-url)
    (error "You should setup first!"))
  (cnblogs//load-entry-list)
  (let ((legal-state (cnblogs//get-post-state (buffer-file-name))))
    (cond
     ((eq 'UNSUPPORTED legal-state)
      (message "Failed: UNSUPPORTED file!"))
     ((eq 'PUBLISHED legal-state)
      (cnblogs/new-category)
      (cnblogs/update-post))
     (t
      (cnblogs/new-category)
      (cnblogs/new-post)))))

(defun cnblogs/delete-post ()
  (interactive)
  (unless (and (file-exists-p cnblogs-category-list-file)
               cnblogs--server-url)
    (error "You should setup first!"))
  (if (cnblogs//get-post-state (buffer-file-name))
      (let ((postid
             (cnblogs//get-postid-by-src-file-name (buffer-file-name))))
        (if (and postid
                 (yes-or-no-p "Are you sure to delete?")
                 (cnblogs-metaweblog-delete-post postid t)
                 (cnblogs//delete-post-from-entry-list postid)
                 (cnblogs//save-entry-list))
            (message "The blog has been deleted!")
          (message "Failed!")))))

(defun cnblogs/save-draft ()
  (interactive)
  (let ((postid
         (cnblogs-metaweblog-new-post (cnblogs//current-buffer-to-post)
                                      nil)))
    (setq cnblogs--entry-list
          (cons
           (cnblogs-metaweblog-get-post postid)
           cnblogs--entry-list))
    (cnblogs//save-entry-list))
  (message "The draft has been saved cuccessfully!"))

(defun cnblogs/import-folder ()
  "Add all file under choosen folder to entry list recursively."
  (interactive)
  (let ((directory (read-directory-name "Import folder: ")))
    (cnblogs//import-directory directory)
    (cnblogs//save-entry-list)))

(defun cnblogs/new-post ()
  "New post!"
  (interactive)
  (if (yes-or-no-p "Do you want to post this blog to cnblogs?")
      (let* ((postid
              (cnblogs-metaweblog-new-post (cnblogs//current-buffer-to-post) t))
             (post (cnblogs-metaweblog-get-post postid)))
        (if (integerp postid) (setq postid (int-to-string postid)))
        (with-temp-file (concat cnblogs-file-post-path postid)
          (print post (current-buffer)))
        (if (cnblogs//file-existp-in-entry-list (buffer-file-name))
            (cnblogs//assign-post-to-file post (buffer-file-name))
          (push
           (list (cnblogs//gen-id)               ; id
                 (cdr (assoc "title" post))      ; title
                 postid                          ; postid
                 (cdr (assoc "categories" post)) ; categories
                 (buffer-file-name)
                 "PUBLISHED")
           cnblogs--entry-list))
        (cnblogs//save-entry-list)
        (message "The blog has been published cuccessfully!"))))

(defun cnblogs/update-post ()
  (interactive)
  (if (cnblogs//get-post-state (buffer-file-name))
      (let ((postid (cnblogs//get-postid-by-src-file-name (buffer-file-name))))
        (if (and postid
                 (yes-or-no-p "Already published! Do you want to update this blog to cnblogs?")
                 (cnblogs-metaweblog-edit-post postid
                                               (cnblogs//current-buffer-to-post)
                                               t)
                 (cnblogs//assign-post-to-file (cnblogs-metaweblog-get-post postid)
                                               (buffer-file-name))
                 (cnblogs//save-entry-list))
            (message "The blog has be updated cuccessfully!")
          (message "Failed!")))))

(defun cnblogs/get-post ()
  (interactive)
  (let* ((postid (read-string "Post ID: "))
         (post (and postid
                    (condition-case ()
                        (cnblogs-metaweblog-get-post postid)
                      (error nil)))))
    (if (and postid
             post
             (cnblogs//delete-post-from-entry-list postid)
             (setq cnblogs--entry-list
                   (cons post cnblogs--entry-list)))
        (message "Succeed to get post!")
      (message "Failed to get post!"))))

(defun cnblogs/new-category ()
  "Add a new category according to the CATEGORIES tag in article."
  (interactive)
  (let ((categories-list (cnblogs//categories-string-to-list
                          (cnblogs//fetch-field "CATEGORIES"))))
    (dolist (categories categories-list)
      (unless (member categories cnblogs--category-list)
        (print (cnblogs-metaweblog-new-category
                (list (cons "name" categories)
                      (cons "description" categories)
                      (cons "slug" "slug")
                      (cons "parent_id" 0))))))))

(defun cnblogs/get-categories ()
  (interactive)
  (setq cnblogs--category-list
        (condition-case ()
            (cnblogs-metaweblog-get-categories)
          (error nil)))
  (if cnblogs--category-list
      (progn
        (cnblogs//save-category-list)
        (message "Succeed to get categories!"))
    (message "Failed to get categories!")))

(defun cnblogs/get-recent-posts ()
  (interactive)
  (let* ((num (read-number "The number of posts: " 1))
         (posts (condition-case ()
                    (cnblogs-metaweblog-get-recent-posts num)
                  (error nil))))
    (if posts
        (progn
          (mapc (lambda (post)
                  (cnblogs//push-post-to-entry-list post))
                posts)
          (cnblogs//save-entry-list)
          (message "Succeed to get posts!"))
      (message "Failed to get posts!"))))

(defun cnblogs/get-user-blogs ()
  (interactive)
  (setq cnblogs--user-blogs
        (condition-case ()
            (prog1
                (cnblogs-metaweblog-get-user-blogs)
              (message "Succeed to get user's blogs."))
          (error cnblogs--user-blogs))))

(defun cnblogs/category-selection ()
  (interactive)
  (save-window-excursion
    (delete-other-windows)
    (split-window-vertically)
    (org-switch-to-buffer-other-window (get-buffer-create " *Cnblogs categories*"))
    (erase-buffer)
    (insert "Current:    \n")
    (insert "\n\n随笔分类:    \n")
    (let* ((idx ?0)
           (ctgr-list (remove-if-not (lambda (ctgr)
                                       (equal (substring ctgr 1 5) "随笔分类"))
                                     cnblogs--category-list))
           (maxlen (apply 'max (mapcar 'length ctgr-list))))
      (mapc (lambda (ctgr)
              (insert "[" idx "]" (format "%s  " (substring ctgr 6)))
              (setq idx (1+ idx)))
            ctgr-list))
    (insert "\n\n网站分类:    \n")
    (let* ((idx ?A)
           (ctgr-list (remove-if-not (lambda (ctgr)
                                       (equal (substring ctgr 1 5) "网站分类"))
                                     cnblogs--category-list))
           (maxlen (apply 'max (mapcar 'length ctgr-list))))
      (mapc (lambda (ctgr)
              (insert "[" idx "]" (format "%s  " (substring ctgr 6)))
              (setq idx (1+ idx)
                    ))
            ctgr-list))
    (insert "\n\n其他分类:    \n")
    ;; 列出其他分类
    (mapc (lambda (ctgr)
            (if (equal (substring ctgr 1 3) "发布")
                (insert (format "%s   " ctgr)))
            )
          cnblogs--category-list)
    (message "[0..9..a-z..]:Toggle [SPC]:clear [RET]:accept")
    (catch 'exit
      (while t
        (let ((c (read-char-exclusive)))
          (cond
           ((= c ?\r) (throw 'exit t))
           (t nil)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; minor mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key cnblogs-mode-map [menu-bar menu-bar-cnblogs-menu]
  (cons "Cnblogs" cnblogs-mode-map))

(define-minor-mode cnblogs-minor-mode
  "cnblogs-minor-mode"
  :init-value nil
  :lighter    " Cnblogs"
  :keymap     cnblogs-mode-map
  :group      Cnblogs)

(add-hook 'cnblogs-minor-mode-hook 'cnblogs//load-variables)

(provide 'cnblogs)

;;; cnblogs.el ends here
