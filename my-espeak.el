;; -*- lexical-binding: t; -*-

;; --- eSpeak Section ---

(defvar read-aloud-process nil
  "Variable to store the current reading process.")

(defun read-aloud-cleanup-hyphens (text)
  "Join words split by hyphen at end of line in TEXT (e.g., \"deal-\\ning\" → \"dealing\")."
  (when text
    (replace-regexp-in-string "-[ \t\n\r]+\\([A-Za-z]\\)" "\\1" text)))

(defun read-aloud-get-text ()
  "Get text to read based on buffer type and selection. Joins hyphen-split words and removes asterisks."
  (let ((raw
         (cond
          ((eq major-mode 'pdf-view-mode)
           (let ((text (if (pdf-view-active-region-p)
                           (pdf-view-active-region-text)
                         (condition-case nil
                             (pdf-info-gettext (pdf-view-current-page)
                                               0 0 -1 -1
                                               (current-buffer))
                           (error "Unable to extract text from PDF page")))))
             (if (listp text)
                 (mapconcat 'identity text " ")
               text)))
          (t
           (if (use-region-p)
               (buffer-substring-no-properties (region-beginning) (region-end))
             (buffer-substring-no-properties (point) (point-max)))))))
    (let ((enhanced (read-aloud-cleanup-hyphens raw)))
      (setq enhanced (replace-regexp-in-string "\\*" "" enhanced))
      (setq enhanced (read-aloud-normalize-text enhanced)) ; <-- Add this line
      enhanced)))

(defun read-aloud-from-cursor-espeak ()
  "Read aloud from the cursor position or selected text using eSpeak."
  (interactive)
  (let ((text (read-aloud-get-text)))
    (if (and text (not (string-empty-p (string-trim text))))
        (progn
          ;; Remove message to minibuffer
          ;; (message "Reading: %s..." (substring text 0 (min 50 (length text))))
          (when (and read-aloud-process (process-live-p read-aloud-process))
            (kill-process read-aloud-process))
          (setq read-aloud-process
                (start-process "espeak-process" nil "/usr/bin/espeak"
                              "-s" "180"
                              "-v" "en"
                              text))
          ;; Reset flag when process finishes
          (set-process-sentinel
           read-aloud-process
           (lambda (_proc _event)
             (setq my/reading-aloud nil))))
      (message "No text to read"))))

(defun stop-reading-aloud ()
  "Stop the current reading process."
  (interactive)
  (when (and read-aloud-process (process-live-p read-aloud-process))
    (kill-process read-aloud-process)
    (setq read-aloud-process nil)
    (message "Stopped reading")))

(defvar my/reading-aloud nil "Flag to track whether reading aloud is active.")

(defun my/toggle-read-aloud-espeak ()
  "Toggle reading aloud from the cursor position using eSpeak."
  (interactive)
  (if my/reading-aloud
      (progn
        (stop-reading-aloud)
        (setq my/reading-aloud nil))
    (progn
      (read-aloud-from-cursor-espeak)
      (setq my/reading-aloud t))))

(global-set-key (kbd "<f9>") 'my/toggle-read-aloud-espeak)

;; --- Fancy (Edge TTS) Section ---

(defun read-aloud-clean-text-enhanced (text)
  "Clean text for reading aloud without disrupting punctuation or hyphenated words."
  (let ((cleaned text))
    (setq cleaned (replace-regexp-in-string "[ \t]+" " " cleaned))
    (setq cleaned (replace-regexp-in-string "\\([a-zA-Z]\\)-\n\\([a-zA-Z]\\)" "\\1\\2" cleaned))
    (setq cleaned (replace-regexp-in-string "\\([a-zA-Z]\\)\n\\([a-z]\\)" "\\1 \\2" cleaned))
    (setq cleaned (replace-regexp-in-string "\n+" " " cleaned))
    (setq cleaned (string-trim cleaned))
    ;; Apply normalization to remove numbers, ligatures, etc.
    (setq cleaned (read-aloud-normalize-text cleaned))
    cleaned))

(defun read-aloud-enhance-punctuation-spacing (text)
  "Add pauses ONLY for sentence punctuation - NEVER touch hyphens in words"
  (let ((enhanced text))
    (setq enhanced (replace-regexp-in-string "\\*" "" enhanced))
    (setq enhanced (replace-regexp-in-string "\\([a-zA-Z]\\): " "\\1. : . " enhanced))
    (setq enhanced (replace-regexp-in-string "\\([a-zA-Z]\\); " "\\1, " enhanced))
    (setq enhanced (replace-regexp-in-string " — " " . — . " enhanced))
    (setq enhanced (replace-regexp-in-string " \"" " . \"" enhanced))
    enhanced))

(defun read-aloud-with-edge-tts (text)
  "Read text using Edge TTS - silent operation."
  (let* ((cleaned-text (read-aloud-clean-text-enhanced text))
         (enhanced-text (read-aloud-enhance-punctuation-spacing cleaned-text))
         (temp-file "/tmp/edge_test.wav")
         (voice "en-US-JennyNeural")
         (player (read-aloud-get-audio-player))
         (edge-tts-bin (or (executable-find "edge-tts")
                          (expand-file-name "~/.local/bin/edge-tts"))))
    (let ((exit-code (call-process edge-tts-bin nil "*edge-tts-output*" t
                                   "--voice" voice
                                   "--text" enhanced-text
                                   "--write-media" temp-file)))
      (if (and (= exit-code 0) (file-exists-p temp-file))
          (progn
            (setq read-aloud-process
                  (start-process "audio-player" "*audio-player*" player temp-file))
            (set-process-sentinel
             read-aloud-process
             (lambda (proc event)
               (when (string-match "finished\\|exited" event)
                 (when (file-exists-p temp-file)
                   (delete-file temp-file))))))
        (progn
          (with-current-buffer "*edge-tts-output*"
            (buffer-string)))))))

(defun read-aloud-get-audio-player ()
  "Find the best available audio player."
  (cond
   ((executable-find "paplay") "paplay")
   ((executable-find "ffplay") "ffplay")
   ((executable-find "mplayer") "mplayer")
   ((executable-find "vlc") "vlc")
   ((executable-find "aplay") "aplay")
   (t (error "No audio player found"))))

(defun read-aloud-from-cursor-fancy ()
  "Read aloud from the cursor position using Edge TTS."
  (interactive)
  (let* ((raw-text (read-aloud-get-text))
         (text (when raw-text raw-text)))
    (if (and text (not (string-empty-p (string-trim text))))
        (progn
          (when (and read-aloud-process (process-live-p read-aloud-process))
            (kill-process read-aloud-process))
          (condition-case err
              (read-aloud-with-edge-tts text)
            (error nil)))
      (message "No text to read"))))

(defun my/toggle-read-aloud-fancy ()
  "Toggle reading aloud using Edge TTS."
  (interactive)
  (if (and read-aloud-process (process-live-p read-aloud-process))
      (progn
        (stop-reading-aloud)
        (setq my/reading-aloud nil))
    (progn
      (read-aloud-from-cursor-fancy)
      (setq my/reading-aloud t))))

(global-set-key (kbd "<f10>") 'my/toggle-read-aloud-fancy)

(defun read-aloud-normalize-text (text)
  "Replace common ligatures and special characters with ASCII equivalents.
Remove text in square brackets (e.g., [A], [C], [200]).
Remove superscript numbers after periods (footnote references).
Remove standalone numbers that appear in the middle of text."
  (let ((normalized text))
    ;; Remove text in square brackets
    (setq normalized (replace-regexp-in-string "\\[[^]]*\\]" "" normalized))
    ;; Remove footnote references after periods and commas (both regular and superscript numbers)
    (setq normalized (replace-regexp-in-string "\\.\\([0-9⁰¹²³⁴⁵⁶⁷⁸⁹]+\\)" "." normalized))
    (setq normalized (replace-regexp-in-string ",\\([0-9⁰¹²³⁴⁵⁶⁷⁸⁹]+\\)" "," normalized))
    ;; Remove standalone numbers (surrounded by spaces or at word boundaries)
    (setq normalized (replace-regexp-in-string "\\b[0-9]+\\b" "" normalized))
    ;; Replace ligatures
    (setq normalized (replace-regexp-in-string "ﬃ" "ffi" normalized))
    (setq normalized (replace-regexp-in-string "ﬁ" "fi" normalized))
    (setq normalized (replace-regexp-in-string "ﬂ" "fl" normalized))
    (setq normalized (replace-regexp-in-string "ﬀ" "ff" normalized))
    (setq normalized (replace-regexp-in-string "ﬄ" "ffl" normalized))
    (setq normalized (replace-regexp-in-string "ﬅ" "ft" normalized))
    (setq normalized (replace-regexp-in-string "ﬆ" "st" normalized))
    normalized))

(provide 'my-espeak)
