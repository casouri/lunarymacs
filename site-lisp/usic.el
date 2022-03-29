;;; usic.el --- Music player  -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:

;;; Code:

(defgroup usic nil
  "Music player."
  :group 'applications)

(defvar usic-columns 3
  "Number of columns of cover grid display")

(defvar usic-library-path "~/attic/music-test"
  "Path to the music library.")

(defvar usic-player #'usic-player-mpv
  "The player used to play music.
A function that takes (FILE ACTION) where FILE is the absolute
path of the music file and ACTION is one of 'play, 'pause,
'resume. FILE can be nil when ACTION is 'pause or 'resume.")

(defvar usic-database-path (expand-file-name
                            "musicdb" usic-library-path)
  "Path to the database.")

(defface usic-album-title '((t . (:inherit variable-pitch
                                           :height 120)))
  "Face for album title.")

(defface usic-album-artist '((t . (:inherit variable-pitch
                                            :height 120)))
  "face for album artist.")

(define-error 'usic-invalid-meta "Invalid meta" 'error)

;;; Metas

(defun usic--retrieve-meta (file)
  "Retrieve meta of FILE."
  (let* ((meta (plist-get
                (plist-get
                 (json-parse-string
                  (shell-command-to-string
                   (format "ffprobe -loglevel quiet -show_format -print_format json %s"
                           (shell-quote-argument file)))
                  :object-type 'plist)
                 :format)
                :tags))
         (track-raw (or (plist-get meta :track)
                        (signal 'usic-invalid-meta '(track))))
         (disc-raw (or (plist-get meta :disc)
                       (signal 'usic-invalid-meta '(disc))))
         (track (string-to-number (car (split-string track-raw "/" t))))
         (disc (string-to-number (car (split-string disc-raw "/" t)))))
    (list :album (or (plist-get meta :album)
                     (signal 'usic-invalid-meta '(album)))
          :artist (or (plist-get meta :artist)
                      (signal 'usic-invalid-meta '(artist)))
          :composer (plist-get meta :composer)
          :year (string-to-number
                 (or (plist-get meta :date)
                     (signal 'usic-invalid-meta '(year))))
          :genre (or (plist-get meta :genre)
                     (signal 'usic-invalid-meta '(genre)))
          :title (or (plist-get meta :title)
                     (signal 'usic-invalid-meta '(title)))
          :track track
          :disc disc
          :location file
          ;; :add-time
          ;; (time-to-seconds
          ;;  (iso8601-parse
          ;;   (or (plist-get meta :creation_time)
          ;;       (signal 'usic-invalid-meta '(creation-time)))))
          )))

(defun usic--retrieve-cover (file)
  "Retrieve album cover of FILE. Return a binary string."
  (with-temp-buffer
    (toggle-enable-multibyte-characters -1)
    (set-buffer-file-coding-system 'raw-text)
    (call-process-shell-command
     (format "ffmpeg -loglevel quiet -i %s -f singlejpeg -"
             (shell-quote-argument file))
     nil t)
    (buffer-string)))

(defun usic--metas-consistent-p (meta-list)
  "Check whether metas in META-LIST are consistent.
Consistent means they have produce the same album meta."
  (let ((list2 (cons (car (last meta-list)) (cdr meta-list))))
    (cl-loop for m1 in meta-list
             for m2 in list2
             if (not (and (equal (plist-get m1 :album)
                                 (plist-get m2 :album))
                          (equal (plist-get m1 :year)
                                 (plist-get m2 :year))
                          (equal (plist-get m1 :genre)
                                 (plist-get m2 :genre))))
             return nil
             finally return t)))

(defun usic--calculate-album-artist (meta-list)
  "Calculate an album artist from META-LIST.
The artist that appears the most times is the album artist."
  (let (alist)
    (dolist (meta meta-list)
      (let* ((artist (plist-get meta :artist))
             (count (alist-get artist alist 0 nil #'equal)))
        (setf (alist-get artist alist 0 nil #'equal)
              (1+ count))))
    (car (car (cl-sort alist #'> :key #'cdr)))))

;;; Database

(defun usic--get-db (db-file)
  "Return a database at DB-FILE.
If doesn’t exist, create one and set it up."
  (let ((db (sqlite-open db-file)))
    (sqlite-execute db "create table if not exists Albums (
title string not null,
artist string not null,
year integer not null,
genre string not null,
cover blob,
unique (title, artist),
primary key (title, artist)
);")
    (sqlite-execute db "create table if not exists Tracks (
album string not null,
artist string not null,
album_artist string not null,
composer string,
title string not null,
track integer not null,
disc integer not null,
location string not null,
add_time float,
primary key (album, album_artist, title),
foreign key (album, album_artist) references Albums
);")
    (sqlite-execute db "create table if not exists Scanned (
album_dir string,
primary key (album_dir)
);")
    db))

(defun usic--record-album (db metas cover)
  "Record METAS in DB.
METAS is a list of meta of each track of the same album. COVER is
the cover image in binary string."
  (let* ((album-artist (usic--calculate-album-artist metas))
         (album-unique-fields (list (plist-get (car metas) :album)
                                    album-artist))
         (args (list (plist-get (car metas) :album)
                     album-artist
                     (plist-get (car metas) :year)
                     (plist-get (car metas) :genre)
                     cover)))
    ;; 1.Insert album record. If already exists, update.
    (if (sqlite-select db "select * from Albums where title=? and artist=?;"
                       album-unique-fields)
        (sqlite-execute db "update Albums set
title=?, artist=?, year=?, genre=?, cover=?
where title=? and artist=?;" (append args album-unique-fields) )

      (sqlite-execute db "insert into Albums (title, artist, year, genre, cover) values (?, ?, ?, ?, ?);" args))
    ;; 2. Insert track records.
    (dolist (meta metas)
      (let ((args (list (plist-get meta :title)
                        (plist-get meta :album)
                        (plist-get meta :artist)
                        album-artist
                        (plist-get meta :composer)
                        (plist-get meta :track)
                        (plist-get meta :disc)
                        (plist-get meta :location)
                        (plist-get meta :add-time)))
            (track-unique-fields (list (plist-get meta :title)
                                       (plist-get meta :album)
                                       album-artist)))
        (if (sqlite-select db "select * from Tracks where title=? and album=? and album_artist=?;" track-unique-fields)
            (sqlite-execute db "update Tracks set
title=?, album=?, artist=?, album_artist=?,
composer=?, track=?, disc=?, location=?, add_time=?
where title=? and album=? and album_artist=?;"
                            (append args track-unique-fields))
          (sqlite-execute db "insert into Tracks (title, album, artist, album_artist, composer, track, disc, location, add_time) values (?, ?, ?, ?, ?, ?, ?, ?, ?);"
                          args))))))

(defun usic--scan-album-1 (db album-dir &optional force)
  "Scan the album at ALBUM-DIR to DB.
If force, always update, otherwise only update when album is not
in DB. Could signal 'usic-invalid-meta."
  (when (or force
            (not (sqlite-select
                  db "select * from Scanned where album_dir=?"
                  (list album-dir))))
    (let* ((files (cl-remove-if-not
                   (lambda (file)
                     (or (string-suffix-p ".m4a" file)
                         (string-suffix-p ".mp3" file)))
                   (directory-files album-dir t "^[^\\.]")))
           (metas (mapcar #'usic--retrieve-meta files))
           (cover (usic--retrieve-cover (car files))))
      (usic--record-album db metas cover)
      (unless (sqlite-select
               db "select * from Scanned where album_dir=?"
               (list album-dir))
        (sqlite-execute
         db "insert into Scanned (album_dir) values (?)"
         (list album-dir))))))

(defun usic--scan-album (db album-dir &optional force)
  "Scan the album at ALBUM-DIR to DB.
If force, always update, otherwise only update when album is not
in DB."
  (condition-case err
      (usic--scan-album-1 db album-dir force)
    (usic-invalid-meta
     (lwarn '(usic) :warning
            "Album %s has invalid metadata (%s)"
            album-dir (cadr err)))))

(defun usic--scan-library (db path &optional force)
  "Scan the albums in library at PATH to DB.
If force, update every album, otherwise only ones doesn’t exist
in DB."
  (let ((artist-dir-list
         (cl-remove-if #'file-regular-p
                       (directory-files path t "^[^\\.]")))
        (counter 0))
    ;; For each arist:
    (dolist (artist-dir artist-dir-list)
      (cl-incf counter)
      (let ((album-dir-list
             (cl-remove-if-not #'file-name-directory
                               (directory-files artist-dir t "^[^\\.]"))))
        ;; For each album:
        (dolist (album-dir album-dir-list)
          (message "Scanning [%s/%s] %s/%s"
                   counter (length artist-dir-list)
                   (file-name-base artist-dir)
                   (file-name-base album-dir))
          (usic--scan-album db album-dir force))))))

;;; UI

(defun usic--insert-album-cover (db title artist width)
  "Insert album cover of (TITLE, ARTIST) at point.
DB is the database. WIDTH is the pixel width of the cover."
  (if-let ((result (car (sqlite-select db "select year, genre, cover from Albums where title=? and artist=?;" (list title artist)))))
      (progn
        (let ((cover (create-image
                      (encode-coding-string (nth 2 result) 'raw-text)
                      nil t :width width)))
          (insert (propertize "X" 'display cover
                              'usic-album title
                              'usic-artist artist))))
    (error "Album (%s, %s) doesn’t exist" title artist)))

(defun usic--draw-grid (db)
  "Draw a grid of albums in current buffer.
DB is the database."
  (let* ((results (sqlite-select db "select title, artist from Albums;"))
         (gap-width (line-pixel-height))
         (cover-width (/ (- (window-width nil t)
                            (* (1- usic-columns) gap-width))
                         usic-columns))
         (idx 0))
    (while (nth idx results)
      ;; For each row of the grid, we draw three lines: cover, title
      ;; and artist.
      (dolist (purpose '(cover gap))
        ;; For each column of the grid, we draw the corresponding album.
        (dotimes (col usic-columns)
          (when (nth idx results)
            (let* ((album (nth idx results))
                   (title (nth 0 album))
                   (artist (nth 1 album))
                   (cover-right-edge (+ (* (1+ col) cover-width)
                                        (* col gap-width)))
                   ;; Pads to the right edge of the cover.
                   (padding (propertize
                             " " 'display
                             `(space :align-to (,cover-right-edge))))
                   (gap (propertize " " 'display
                                    `(space :width (,gap-width)))))
              (pcase purpose
                ('cover (usic--insert-album-cover
                         db title artist cover-width)
                        (add-text-properties (1- (point)) (point)
                                             (list 'usic-col col
                                                   'usic-album title
                                                   'usic-artist artist)))
                ('gap (insert (propertize padding 'usic-col col)))
                ('title (insert
                         (propertize title 'face 'usic-album-title
                                     'usic-col col)
                         (propertize padding 'usic-col col)))
                ('artist (insert
                          (propertize artist 'face 'usic-album-artist
                                      'usic-col col)
                          (propertize padding 'usic-col col))))
              (unless (eq col (1- usic-columns))
                (insert gap))))
          (cl-incf idx))
        (insert "\n")
        (setq idx (- idx usic-columns)))
      (cl-incf idx usic-columns))))

(defvar-local usic--highlight-overlays nil
  "Overlay used to highlight current album.")

(defun usic--highlight-album-at-point ()
  "Highlight the album title and artist of the album at point.
Point should be on the album cover."
  (let ((col (get-text-property (point) 'usic-col)))
    (save-excursion
      (unless usic--highlight-overlays
        (setq usic--highlight-overlays
              (list (make-overlay (point) (point))
                    (make-overlay (point) (point)))))
      (dotimes (idx 1)
        (forward-line 1)
        (let* ((match (text-property-search-forward 'usic-col col t))
               (beg (prop-match-beginning match)))
          (move-overlay (nth idx usic--highlight-overlays)
                        beg (point))
          (overlay-put (nth idx usic--highlight-overlays)
                       'face 'highlight))))))

(defun usic-forward ()
  "Move forward in album grid."
  (interactive)
  (let ((orig-point (point)))
    ;; Don’t move further if we are at the right most album of this
    ;; line.
    (unless (eq (point) (1- (line-end-position)))
      (forward-char 2))
    ;; Move back if the right of us is empty (row ended early).
    (unless (get-text-property (point) 'usic-col)
      (goto-char orig-point)))
  (usic--highlight-album-at-point))

(defun usic-backward ()
  "Move backward in album grid."
  (interactive)
  (unless (eq (point) (line-beginning-position))
    (forward-char -2))
  (usic--highlight-album-at-point))

(defun usic-downward ()
  "Move downward in album grid."
  (interactive)
  (let ((col (current-column))
        (orig-point (point)))
    (ignore-errors
      (forward-line 2)
      (forward-char col))
    ;; Move back if below us is empty (we are at the last row).
    (unless (get-text-property (point) 'usic-col)
      (goto-char orig-point)))
  (usic--highlight-album-at-point))

(defun usic-upward ()
  "Move upward in album grid."
  (interactive)
  (let ((col (current-column)))
    (forward-line -2)
    (forward-char col))
  (usic--highlight-album-at-point))

(defun usic-click (e)
  "Click stuff."
  (interactive "e")
  (goto-char (posn-point (event-end e)))
  (usic--highlight-album-at-point))

;;; Library

(defun usic--tracks-of-album (db album artist)
  "Get tracks of (ALBUM ARTIST).
DB is the database. Return (TITLE ALBUM ARTIST ALBUM_ARTIST
COMPOSER TRACK DISC LOCATION)."
  (sqlite-select db "select * from Tracks where
album=? and album_artist=?
order by track, disc;" (list album artist)))

;;; Command

(defun usic ()
  "Bring up usic buffer."
  (interactive)
  (switch-to-buffer "*Usic*")
  (unless (derived-mode-p 'usic-mode)
    (usic-mode)))

(defvar usic-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C-f" #'usic-forward)
    (keymap-set map "C-b" #'usic-backward)
    (keymap-set map "C-n" #'usic-downward)
    (keymap-set map "C-p" #'usic-upward)
    (keymap-set map "SPC" #'usic-play-album-at-point)
    (keymap-set map "<mouse-1>" #'usic-click)
    map)
  "Keymap for usic-mode.")

(defvar-local usic--db nil
  "Database.")

(define-derived-mode usic-mode special-mode "USIC"
  "Music player."
  :group 'usic
  (let ((inhibit-read-only t))
    (erase-buffer)
    (let ((db (usic--get-db usic-database-path)))
      (setq usic--db db)
      (usic--draw-grid db))
    (goto-char (point-min))
    (usic--highlight-album-at-point)))

(defun usic-scan ()
  "Scan the library at PATH in separate process."
  (interactive)
  (pop-to-buffer "*Messages*")
  (usic--scan-library (usic--get-db usic-database-path)
                      usic-library-path))

(defun usic-rescan-album (album)
  "Re-scan ALBUM."
  (interactive
   (list (read-directory-name
          "Album: " (file-name-directory usic-library-path)
          nil t)))
  (usic--scan-album (usic--get-db usic-database-path) album t))

;; (defun usic-scan (path)
;;   "Scan the library at PATH in separate process."
;;   (interactive (list (read-directory-name
;;                       "Library: " (file-name-directory usic-library-path)
;;                       nil t (file-name-base usic-library-path))))
;;   (make-process :name "usic scan"
;;                 :buffer "*usic scan*"
;;                 :command (list "emacs" "--batch" "-Q"
;;                                "-l" (locate-library "usic.el" t)
;;                                "--eval"
;;                                (prin1-to-string
;;                                 `(let* ((dbpath ,usic-database-path)
;;                                         (db (usic--get-db dbpath)))
;;                                    (usic-scan-library db ,path))))
;;                 :connection-type 'pipe)
;;   (pop-to-buffer "*usic scan*"))

;;; Mpv

(defvar usic--mpv-process nil
  "The mpv IPC process.")
(defvar usic--mpv-sock nil
  "The mpv socket process.")

(defun usic--get-mpv-process ()
  "Return a mpv IPC process."
  (cons
   (or (and (process-live-p usic--mpv-process)
            usic--mpv-process)
       (setq usic--mpv-process
             (make-process :name "mpv"
                           :buffer " *mpv*"
                           :command '("mpv" "--no-video" "--idle"
                                      "--volume" "50"
                                      "--input-ipc-server=/tmp/mpv.sock")
                           :connection-type 'pipe)))
   (or (and (process-live-p usic--mpv-sock)
            usic--mpv-sock)
       (setq usic--mpv-sock
             (make-network-process
              :name "mpv sock"
              :buffer " *mpv sock*"
              :family 'local
              :remote "/tmp/mpv.sock")))))

(defun usic-player-mpv (file action)
  "Player that use mpv. Play FILE by ACTION.
FILE can be a list of files to be played."
  (process-send-string
   (cdr (usic--get-mpv-process))
   (concat
    (string-replace
     "\n" ""
     (json-serialize
      (pcase action
        ('play (if (consp file)
                   (progn
                     (with-temp-buffer
                       (dolist (f file)
                         (insert f "\n"))
                       (write-file "/tmp/mpv.list"))
                     `(:command ["loadlist" "/tmp/mpv.list"]))
                 `(:command ["loadfile" ,file])))
        ('pauseplay '(:command ["cycle" "pause"]))
        ('pause '(:command ["set_property" "pause" t]))
        ('resume '(:command ["set_property" "pause" :false])))))
    "\n")))

;;; Playback

(defvar usic--playback-queue nil
  "A list of tracks to play next.")

(defun usic-play-album-at-point ()
  "Play the album at point."
  (interactive)
  (let* ((album (get-text-property (point) 'usic-album))
         (artist (get-text-property (point) 'usic-artist))
         (tracks (usic--tracks-of-album usic--db album artist))
         (locations (mapcar (lambda (track) (nth 7 track))
                            tracks)))
    (funcall usic-player locations 'play)
    (setq usic--playback-queue tracks)))

(provide 'usic)

;;; usic.el ends here
