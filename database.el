;; dbm - database manager

;; dbu - database utils
;;-----------------------------------------------------------------------------

(defun dbu/list-to-org-row (ls)
  (if (= 0 (length ls) )
      ""
    (concat "| " (cl-reduce (lambda (all x) (concat all " | " x)) ls) " |")))
;; -- Tests 
(assert (equal (dbu/list-to-org-row '("a" "b" "c"))
               "| a | b | c |"))
(assert (equal  (dbu/list-to-org-row '("a"))
                "| a |"))
;; TODO make 
(assert (equal (dbu/list-to-org-row '())
               ""))
;; --------------------------------
(defun make-region-read-only (start end)
  (interactive "*r")
  (let ((inhibit-read-only t))
    (put-text-property start end 'read-only t)))
;; --------------------------------
(defun make-region-read-write (start end)
  (interactive "*r")
  (let ((inhibit-read-only t))
    (put-text-property start end 'read-only nil)))

;; Database data structure 
;;----------------------------------------------------------------------------- 
(defun dbm/new-database (user pass db)
  `((:username . ,user) (:password . ,pass) (:database . ,db)))
;; -- Tests
(defvar test-database (dbm/new-database "root" "password" "obsidian-chat"))
(defvar test-database2 (dbm/new-database "root" "password" "lpsignals"))
(assert (equal (dbm/new-database "root" "password" "obsidian-chat")
               '((:username . "root")
                 (:password . "password")
                 (:database . "obsidian-chat"))))
(assert (equal test-database
               (dbm/new-database "root" "password" "obsidian-chat")))
;; -- Globals 
(defvar +database+
  (dbm/new-database "root" "password" "obsidian-chat"))
;; --------------------------- Functions ---------------------------------------
(defun dbm/db-exec (db command)
  (shell-command-to-string
   (concat
    "mysql "
    "-u" (alist-get :username db) " "
    "-p" (alist-get :password db) " "
    (alist-get :database db) " "
    "-e"
    "'" command "'")))
;; -- Tests 
(assert (s-match "user_id" (dbm/db-exec test-database "show columns from messages")))
(assert (s-match "channel_id" (dbm/db-exec test-database "show columns from messages")))
(assert (not (s-match "ass_id" (dbm/db-exec test-database "show columns from messages"))))
;; --------------------------------
(defun dbm/db-raw-show-columns (db table)
  (dbm/db-exec db (concat "show columns from " table)))
;; -- Tests
(assert (s-match "user_id" (dbm/db-raw-show-columns test-database "messages")))
(assert (s-match "channel_id" (dbm/db-raw-show-columns test-database "messages")))
(assert (not (s-match "ass_id" (dbm/db-raw-show-columns test-database "messages"))))
;; -- Hand Run Tests
;; (dbm/db-raw-show-columns test-database "messages")
;; --------------------------------
(defun dbm/db-get-columns (db table)
  (->>
   (split-string (dbm/db-raw-show-columns db table) "\n")
   rest
   rest
   (mapcar (lambda (x) (-> x split-string car)))
   (-drop-last 1)))
;; -- Tests
(assert (equal '("id" "user_id" "channel_id" "message" "created_at") (dbm/db-get-columns test-database "messages")))
;; --------------------------------
(cl-defun dbm/db-raw-select-all-from-table (db table &optional (limit-start 1) (limit 25))
  (dbm/db-exec db (format "select * from %s limit %s,%s" table limit-start limit)))
;; -- Hand Run Tests 
;; (dbm/db-raw-select-all-from-table test-database "messages" 25 25)
;; --------------------------------
(defun dbm/db-get-all-from-table (db table)
  (mapcar (lambda (x) (split-string x "\t"))
          (-> (dbm/db-raw-select-all-from-table db table)
              (split-string "\n")
              rest
              rest)))
;; -- Hand Run Tests
(dbm/db-get-all-from-table test-database "messages")
;; --------------------------------
(cl-defun dbm/db-get-all-page-from-table (db table &optional (page 0) (page-size 25))
  (mapcar (lambda (x) (split-string x "\t"))
          (-> (dbm/db-raw-select-all-from-table db table (* page-size page) page-size)
              (split-string "\n")
              rest
              rest)))
;; -- Hand Run Tests
;; (dbm/db-get-all-page-from-table test-database2 "race_summaries")
;; (dbm/db-get-all-from-table test-database "messages")
;; --------------------------------
(defun dbm/table-cols-text (db table)
  (concat
   "| " 
   (cl-reduce (lambda (all x) (concat all " | " x)) (dbm/db-get-columns db table))
   " |"))
;; Hand Run Tests
;; (dbm/table-cols-text test-database "messages")
;; (mapcar 'dbu/list-to-org-row (dbm/db-get-columns db table))
;; --------------------------------
(defun dbm/insert-table-body (db table)
  (interactive)
  (loop 
   for x in (mapcar 'dbu/list-to-org-row (dbm/db-get-all-from-table db table))
   do
   (insert x "\n")
   finally
   (insert x)))
;;( dbm/insert-table-body test-database "messages")
;; --------------------------------
(defun dbm/insert-table (db table)
  (interactive)
  (insert (dbm/table-cols-text db table))
  (insert "\n")
  (dbm/insert-table-body db table))
;; -- Hand Run Tests
;; (dbm/insert-table test-database "messages")
;; (dbm/insert-table test-database2 "race_summaries")
;; --------------------------------


