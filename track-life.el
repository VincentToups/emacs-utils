(setq *track-life-file* "~/track-life.el")
(setq *track-life-tags* ())

(defmacro def-track-life-tag (tag desc)
  `(alist>> *track-life-tags* ,tag ,desc))